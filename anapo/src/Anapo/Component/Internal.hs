{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Note: we use 'Traversal' to keep a cursor to the
-- write end of the state, but really we should use an
-- "affine traversal" which guarantees we have either 0
-- or 1 positions to traverse. See
-- <https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/>
-- for why affine traversals do not play well with lens.
module Anapo.Component.Internal where

import qualified Data.HashMap.Strict as HMS
import Control.Lens (Lens', view, lens)
import qualified Control.Lens as Lens
import Control.Monad (ap)
import Data.Monoid ((<>), Endo)
import qualified Data.DList as DList
import Data.String (IsString(..))
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import GHC.Stack (HasCallStack, CallStack, callStack)
import Control.Monad.State (execStateT, StateT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (askUnliftIO, unliftIO, MonadUnliftIO, UnliftIO(..))
import Control.Exception.Safe (SomeException, uninterruptibleMask, tryAny)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Monad.Trans (lift)
import Data.DList (DList)
import Data.IORef (IORef, newIORef, modifyIORef')
import Control.Monad.Reader (MonadReader(..))
import qualified Data.Vector as Vec
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.EventM as DOM.EventM

import qualified Anapo.VDOM as V
import Anapo.Render
import Anapo.Logging
import Anapo.Text (Text, pack)
import qualified Anapo.Text as T
import qualified Anapo.OrderedHashMap as OHM

#if defined(ghcjs_HOST_OS)
import GHCJS.Types (JSVal)
#else
import qualified Language.Javascript.JSaddle as JSaddle
#endif

-- affine traversals
-- --------------------------------------------------------------------

-- | for the components we want affine traversals --
-- traversals which point either to one or to zero elements.
-- however lens currently does not provide them, see
-- <https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/>.
-- therefore we provide a type synonym for clarity.
type AffineTraversal a b c d = Lens.Traversal a b c d
type AffineTraversal' a b = Lens.Traversal' a b

-- | to be used with 'AffineTraversal'
toMaybeOf :: (HasCallStack) => Lens.Getting (Endo [a]) s a -> s -> Maybe a
toMaybeOf l x = case Lens.toListOf l x of
  [] -> Nothing
  [y] -> Just y
  _:_ -> error "toMaybeOf: multiple elements returned!"

-- Dispatching and handling
-- --------------------------------------------------------------------

newtype Dispatch stateRoot = Dispatch
  { unDispatch ::
      forall state props. CallStack -> AffineTraversal' stateRoot (Component props state) -> (state -> DOM.JSM state) -> IO ()
  }

-- Register / handle
-- --------------------------------------------------------------------

-- we use these two on events and on forks, so that we can handle
-- all exceptions in a central place and so that we don't leave
-- dangling threads

type RegisterThread = IO () -> IO ()
type HandleException = SomeException -> IO ()

-- Action
-- --------------------------------------------------------------------

-- | An action we'll spawn from a component -- for example when an event
-- fires or as a fork in an event
newtype Action state a = Action
  { unAction ::
         forall rootState compProps compState.
         ActionEnv rootState compProps compState state
      -> DOM.JSM a
  }

data ActionEnv rootState compProps compState state = ActionEnv
  { aeRegisterThread :: RegisterThread
  , aeHandleException :: HandleException
  , aeDispatch :: Dispatch rootState
  , aeTraverseToComp :: AffineTraversal' rootState (Component compProps compState)
  , aeTraverseToState :: AffineTraversal' compState state
  }

{-
{-# INLINE runAction #-}
runAction :: Action state a -> RegisterThread -> HandleException -> Dispatch state -> DOM.JSM a
runAction vdom reg hdl disp = unAction vdom reg hdl disp id
-}

instance Functor (Action state) where
  {-# INLINE fmap #-}
  fmap f (Action g) = Action $ \env -> do
    x <- g env
    return (f x)

instance Applicative (Action state) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Action state) where
  {-# INLINE return #-}
  return x = Action (\_env -> return x)
  {-# INLINE (>>=) #-}
  ma >>= mf = Action $ \env -> do
    x <- unAction ma env
    unAction (mf x) env

instance MonadIO (Action state) where
  {-# INLINE liftIO #-}
  liftIO m = Action (\_env -> liftIO m)

#if !defined(ghcjs_HOST_OS)
instance  JSaddle.MonadJSM (Action state) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = Action (\_env -> m)
#endif

instance MonadUnliftIO (Action state) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = Action $ \env -> do
    u <- askUnliftIO
    return (UnliftIO (\(Action m) -> unliftIO u (m env)))

class (DOM.MonadJSM m) => MonadAction state m | m -> state where
  liftAction :: Action state a -> m a

instance MonadAction state (Action state) where
  {-# INLINE liftAction #-}
  liftAction = id

instance (MonadAction state m) => MonadAction state (StateT s m) where
  {-# INLINE liftAction #-}
  liftAction m = lift (liftAction m)

{-# INLINE actionZoom #-}
actionZoom :: MonadAction out m => AffineTraversal' out in_ -> Action in_ a -> m a
actionZoom t m =
  liftAction (Action (\env -> unAction m env{aeTraverseToState = aeTraverseToState env . t}))

{-# INLINE actionComponent #-}
actionComponent ::
  MonadAction (Component props state) m => Action state a -> m a
actionComponent (Action m) = liftAction $ Action $ \env ->
  m env
    { aeTraverseToComp = aeTraverseToComp env . componentState . aeTraverseToState env
    , aeTraverseToState = id
    }

{-# INLINE forkRegistered #-}
forkRegistered :: MonadUnliftIO m => RegisterThread -> HandleException -> m () -> m ThreadId
forkRegistered register handler m = do
  u <- askUnliftIO
  liftIO $ uninterruptibleMask $ \restore -> forkIO $ register $ do
    mbErr <- tryAny (restore (unliftIO u m))
    case mbErr of
      Left err -> do
        tid <- myThreadId
        logError ("Caught exception in registered thread " <> pack (show tid) <> ", will handle it upstream: " <> pack (show err))
        handler err
      Right _ -> return ()

{-# INLINE actionFork #-}
actionFork :: MonadAction state m => Action state () -> m ThreadId
actionFork m =
  liftAction (Action (\env -> forkRegistered (aeRegisterThread env) (aeHandleException env) (unAction m env)))

{-# INLINE dispatch #-}
dispatch :: (HasCallStack, MonadAction state m) => StateT state (Action state) () -> m ()
dispatch m =
  liftAction $ Action $ \env ->
    liftIO $ unDispatch (aeDispatch env) callStack (aeTraverseToComp env) $ aeTraverseToState env $ \st ->
      unAction (execStateT m st) env

{-# INLINE askRegisterThread #-}
askRegisterThread :: (MonadAction state m) => m RegisterThread
askRegisterThread = liftAction (Action (\env -> return (aeRegisterThread env)))

{-# INLINE askHandleException #-}
askHandleException :: (MonadAction state m) => m HandleException
askHandleException = liftAction (Action (\env -> return (aeHandleException env)))

-- Monad
-- --------------------------------------------------------------------

data AnapoEnv stateRoot state = AnapoEnv
  { aeReversePath :: [VDomPathSegment]
  -- ^ this is stored in _reverse_ order
  , aePrevState :: Maybe stateRoot
  , aeState :: state
  }

newtype AnapoM dom state a = AnapoM
  { unAnapoM ::
         forall rootState compProps compState.
         ActionEnv rootState compProps compState state
      -> AnapoEnv rootState state
      -> dom
      -> DOM.JSM (dom, a)
  }

type ResetComponent = IO ()

data DomState = DomState
  { domStateLength :: Int
  , domStateDom :: ~(DList (V.Node V.SomeVDomNode))
  }

newtype KeyedDomState = KeyedDomState (DList (Text, V.Node V.SomeVDomNode))

newtype MapDomState = MapDomState (DList (Text, V.Node V.SomeVDomNode))

-- the Int is to store the current index in the dom, to be able o build
-- tthe next path segment.
type Dom state = AnapoM DomState state ()
type KeyedDom state = AnapoM KeyedDomState state ()
type MapDom state = AnapoM MapDomState state ()
type Node state = AnapoM () state (V.Node V.SomeVDomNode)

instance MonadIO (AnapoM dom state) where
  {-# INLINE liftIO #-}
  liftIO m = AnapoM $ \_actEnv _anEnv dom -> do
    x <- liftIO m
    return (dom, x)

#if !defined(ghcjs_HOST_OS)
instance JSaddle.MonadJSM (AnapoM dom state) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = AnapoM $ \_actEnv _anEnv dom -> do
    x <- m
    return (dom, x)
#endif

instance Functor (AnapoM dom state) where
  {-# INLINE fmap #-}
  fmap f (AnapoM g) = AnapoM $ \acEnv aeEnv dom -> do
    (dom', x) <- g acEnv aeEnv dom
    return (dom', f x)

instance Applicative (AnapoM dom state) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (AnapoM dom state) where
  {-# INLINE return #-}
  return x = AnapoM (\_acEnv _aeEnv dom -> return (dom, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = AnapoM $ \acEnv anEnv dom0 -> do
    (dom1, x) <- unAnapoM ma acEnv anEnv dom0
    (dom2, y) <- unAnapoM (mf x) acEnv anEnv dom1
    return (dom2, y)

instance (a ~ ()) => Monoid (AnapoM dom state a) where
  {-# INLINE mempty #-}
  mempty = return ()
  {-# INLINE mappend #-}
  mappend = (>>)

instance MonadAction state (AnapoM dom state) where
  {-# INLINE liftAction #-}
  liftAction (Action f) = AnapoM $ \acEnv _anEnv dom -> do
    x <- f acEnv
    return (dom, x)

instance MonadReader state (AnapoM dom state) where
  {-# INLINE ask #-}
  ask = AnapoM (\_acEnv anEnv dom -> return (dom, aeState anEnv))
  {-# INLINE local #-}
  local f m = AnapoM $ \acEnv anEnv dom -> do
    unAnapoM m acEnv anEnv{ aeState = f (aeState anEnv) } dom

{-# INLINE askPreviousState #-}
askPreviousState :: AnapoM dom state (Maybe state)
askPreviousState =
  AnapoM $ \acEnv anEnv dom ->
    return
      ( dom
      , toMaybeOf (aeTraverseToComp acEnv.componentState.aeTraverseToState acEnv) =<< aePrevState anEnv
      )

{-# INLINE zoomL #-}
zoomL :: Lens' out in_ -> AnapoM dom in_ a -> AnapoM dom out a
zoomL l m = AnapoM $ \acEnv anEnv dom ->
  unAnapoM m
    acEnv{ aeTraverseToState = aeTraverseToState acEnv . l }
    anEnv{ aeState = view l (aeState anEnv) }
    dom

{-# INLINE zoomT #-}
zoomT ::
     HasCallStack
  => in_
  -> AffineTraversal' out in_
  -- ^ note: if the traversal is not affine you'll get crashes.
  -> AnapoM dom in_ a
  -> AnapoM dom out a
zoomT st l m = AnapoM $ \acEnv anEnv dom ->
  unAnapoM m
    acEnv{ aeTraverseToState = aeTraverseToState acEnv . l }
    anEnv{ aeState = st }
    dom

-- to manipulate nodes
-- --------------------------------------------------------------------

{-# INLINE unsafeWillMount #-}
unsafeWillMount :: (DOM.Node -> Action state ()) -> NodePatch el state
unsafeWillMount = NPUnsafeWillMount

{-# INLINE unsafeDidMount #-}
unsafeDidMount :: (DOM.Node -> Action state ()) -> NodePatch el state
unsafeDidMount = NPUnsafeDidMount

{-# INLINE unsafeWillPatch #-}
unsafeWillPatch :: (DOM.Node -> Action state ()) -> NodePatch el state
unsafeWillPatch = NPUnsafeWillPatch

{-# INLINE unsafeDidPatch #-}
unsafeDidPatch :: (DOM.Node -> Action state ()) -> NodePatch el state
unsafeDidPatch = NPUnsafeDidPatch

{-# INLINE unsafeWillRemove #-}
unsafeWillRemove :: (DOM.Node -> Action state ()) -> NodePatch el state
unsafeWillRemove = NPUnsafeWillRemove

-- useful shorthands
-- --------------------------------------------------------------------

{-# INLINE n #-}
n :: Node state -> Dom state
n getNode = AnapoM $ \acEnv anEnv dom -> do
  (_, nod) <- unAnapoM getNode acEnv anEnv{ aeReversePath = VDPSNormal (domStateLength dom) : aeReversePath anEnv } ()
  return
    ( dom
        { domStateLength = domStateLength dom + 1
        , domStateDom = domStateDom dom <> DList.singleton nod
        }
    , ()
    )

{-# INLINE key #-}
key :: Text -> Node state -> KeyedDom state
key k getNode = AnapoM $ \acEnv anEnv (KeyedDomState dom) -> do
  (_, nod) <- unAnapoM getNode acEnv anEnv{ aeReversePath = VDPSKeyed k : aeReversePath anEnv } ()
  return (KeyedDomState (dom <> DList.singleton (k, nod)), ())

{-# INLINE ukey #-}
ukey :: Text -> Node state -> MapDom state
ukey k getNode = AnapoM $ \acEnv anEnv (MapDomState dom) -> do
  (_, nod) <- unAnapoM getNode acEnv anEnv{ aeReversePath = VDPSKeyed k : aeReversePath anEnv } ()
  return (MapDomState (dom <> DList.singleton (k, nod)), ())

{-# INLINE text #-}
text :: Text -> Node state
text txt = return $ V.Node
  { V.nodeBody = V.SomeVDomNode $ V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBText txt
      , V.vdomCallbacks = mempty
      , V.vdomWrap = DOM.Text
      }
  , V.nodeChildren = Nothing
  }

instance (el ~ DOM.Text) => IsString (Node state) where
  {-# INLINE fromString #-}
  fromString = text . T.pack

{-# INLINE rawNode #-}
rawNode ::
     (DOM.IsNode el)
  => (DOM.JSVal -> el) -> el
  -> [NodePatch el state]
  -> Node state
rawNode wrap x patches = do
  node <- patchNode patches
    V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBRawNode x
      , V.vdomCallbacks = mempty
      , V.vdomWrap = wrap
      }
  return V.Node
    { V.nodeBody = V.SomeVDomNode node
    , V.nodeChildren = Nothing
    }

-- TODO this causes linking errors, sometimes. bizzarely, the linking
-- errors seem to happen only if a closure is formed -- e.g. if we
-- define the function as
--
-- @
-- marked shouldRerender ptr = deRefStaticPtr ptr
-- @
--
-- things work, but if we define it as
--
-- @
-- marked shouldRerender ptr = do
--   nod <- deRefStaticPtr ptr
--   return nod
-- @
--
-- they don't. the errors look like this:
--
-- @
-- dist/build/test-app/test-app-tmp/Anapo/TestApps/YouTube.dyn_o: In function `hs_spt_init_AnapoziTestAppsziYouTube':
-- ghc_18.c:(.text.startup+0x3): undefined reference to `r19F9_closure'
-- @
--
-- at call site (see for example Anapo.TestApps.YouTube)
{-# INLINE marked #-}
marked ::
     (Maybe state -> props -> state -> V.Rerender)
  -> StaticPtr (props -> Node state) -> props -> Node state
marked shouldRerender ptr props = AnapoM $ \acEnv anEnv dom -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender
        (toMaybeOf (aeTraverseToComp acEnv.componentState.aeTraverseToState acEnv) =<< aePrevState anEnv)
        props
        (aeState anEnv)
  (_, V.Node (V.SomeVDomNode nod) children) <-
    unAnapoM (deRefStaticPtr ptr props) acEnv anEnv dom
  return ((), V.Node (V.SomeVDomNode nod{ V.vdomMark = Just (V.Mark fprint rer) }) children)

-- Utilities to quickly create nodes
-- --------------------------------------------------------------------

data SomeEventAction el write = forall e. (DOM.IsEvent e) =>
  SomeEventAction (DOM.EventM.EventName el e) (el -> e -> Action write ())
newtype UnsafeRawHtml = UnsafeRawHtml Text

data NodePatch el state =
    NPUnsafeWillMount (DOM.Node -> Action state ())
  | NPUnsafeDidMount (DOM.Node -> Action state ())
  | NPUnsafeWillPatch (DOM.Node -> Action state ())
  | NPUnsafeDidPatch (DOM.Node -> Action state ())
  | NPUnsafeWillRemove (DOM.Node -> Action state ())
  | NPStyle V.StylePropertyName V.StyleProperty
  | NPAttribute V.AttributeName (DOM.JSM V.AttributeBody)
  | NPProperty V.ElementPropertyName (DOM.JSM V.ElementProperty)
  | NPEvent (SomeEventAction el state)

class IsElementChildren a state where
  elementChildren :: HasCallStack => a -> AnapoM () state (V.Children V.SomeVDomNode)
instance IsElementChildren () state where
  {-# INLINE elementChildren #-}
  elementChildren _ = return (V.CNormal mempty)
instance (a ~ (), state1 ~ state2) => IsElementChildren (AnapoM DomState state1 a) state2 where
  {-# INLINE elementChildren #-}
  elementChildren (AnapoM f) = AnapoM $ \acEnv anEnv _ -> do
    (DomState _ dom, _) <- f acEnv anEnv (DomState 0 mempty)
    return ((), V.CNormal (Vec.fromList (DList.toList dom)))
instance (a ~ (), state1 ~ state2) => IsElementChildren (AnapoM KeyedDomState state1 a) state2 where
  {-# INLINE elementChildren #-}
  elementChildren (AnapoM f) = AnapoM $ \acEnv anEnv _ -> do
    (KeyedDomState dom, _) <- f acEnv anEnv (KeyedDomState mempty)
    return ((), V.CKeyed (OHM.fromList (DList.toList dom)))
instance (a ~ (), state1 ~ state2) => IsElementChildren (AnapoM MapDomState state1 a) state2 where
  {-# INLINE elementChildren #-}
  elementChildren (AnapoM f) = AnapoM $ \acEnv anEnv _ -> do
    (MapDomState dom, _) <- f acEnv anEnv (MapDomState mempty)
    let kvs = DList.toList dom
    let numKvs = length kvs
    let hm = HMS.fromList kvs
    if numKvs /= HMS.size hm
      then error "duplicate keys when building map vdom!"
      else return ((), V.CMap (HMS.fromList kvs))
instance IsElementChildren UnsafeRawHtml state2 where
  {-# INLINE elementChildren #-}
  elementChildren (UnsafeRawHtml txt) = return (V.CRawHtml txt)

{-# INLINE patchNode #-}
patchNode ::
     (HasCallStack, DOM.IsNode el)
  => [NodePatch el state] -> V.VDomNode el -> AnapoM () state (V.VDomNode el)
patchNode patches00 node00 = do
  u <- liftAction askUnliftIO
  let
    modifyCallbacks body f =
      body{ V.vdomCallbacks = f (V.vdomCallbacks body) }
  let
    modifyElement ::
         V.VDomNode el
      -> ((DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => V.Element el -> V.Element el)
      -> V.VDomNode el
    modifyElement body f = case V.vdomBody body of
      V.VDBElement e -> body{ V.vdomBody = V.VDBElement (f e) }
      V.VDBText{} -> error "got patch requiring an element body, but was NBText"
      V.VDBRawNode{} -> error "got patch requiring an element body, but was NBRawNode"
  let
    go !node = \case
      [] -> return node
      patch : patches -> case patch of
        NPUnsafeWillMount cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksUnsafeWillMount = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPUnsafeDidMount cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksUnsafeDidMount = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPUnsafeWillPatch cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksUnsafeWillPatch = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPUnsafeDidPatch cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksUnsafeDidPatch = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPUnsafeWillRemove cback -> go
          (modifyCallbacks node $ \cbacks -> mappend
            cbacks
            mempty{ V.callbacksUnsafeWillRemove = \e -> liftIO (unliftIO u (cback e)) })
          patches
        NPStyle styleName styleBody -> go
          (modifyElement node $ \vel -> vel
            { V.elementStyle =
                HMS.insert styleName styleBody (V.elementStyle vel)
            })
          patches
        NPAttribute attrName attrBody -> do
          attrBodyVal <- DOM.liftJSM attrBody
          go
            (modifyElement node $ \vel -> vel
              { V.elementAttributes =
                  HMS.insert attrName attrBodyVal (V.elementAttributes vel)
              })
            patches
        NPProperty propName propBody -> do
          propBodyVal <- DOM.liftJSM propBody
          go
            (modifyElement node $ \vel -> vel
              { V.elementProperties =
                  HMS.insert propName propBodyVal (V.elementProperties vel)
              })
            patches
        NPEvent (SomeEventAction evName evListener) -> go
          (modifyElement node $ \vel -> vel
            { V.elementEvents = DList.snoc
                (V.elementEvents vel)
                (V.SomeEvent evName $ \e ev ->
                  liftIO (unliftIO u (evListener e ev)))
            })
          patches
  go node00 patches00

{-# INLINE el #-}
el ::
     ( IsElementChildren a state
     , DOM.IsElement el, DOM.IsElementCSSInlineStyle el
     , HasCallStack
     )
  => V.ElementTag
  -> (DOM.JSVal -> el)
  -> [NodePatch el state]
  -> a
  -> Node state
el tag wrap patches isChildren = do
  children <- elementChildren isChildren
  vdom <- patchNode patches
    V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBElement V.Element
          { V.elementTag = tag
          , V.elementProperties = mempty
          , V.elementStyle = mempty
          , V.elementEvents = mempty
          , V.elementAttributes = mempty
          }
      , V.vdomCallbacks = mempty
      , V.vdomWrap = wrap
      }
  return V.Node
    { V.nodeBody = V.SomeVDomNode vdom
    , V.nodeChildren = Just children
    }

-- Elements
-- --------------------------------------------------------------------

{-# INLINE div_ #-}
div_ :: IsElementChildren a state => [NodePatch DOM.HTMLDivElement state] -> a -> Node state
div_ = el "div" DOM.HTMLDivElement

{-# INLINE span_ #-}
span_ :: IsElementChildren a state => [NodePatch DOM.HTMLSpanElement state] -> a -> Node state
span_ = el "span" DOM.HTMLSpanElement

{-# INLINE a_ #-}
a_ :: IsElementChildren a state => [NodePatch DOM.HTMLAnchorElement state] -> a -> Node state
a_ = el "a" DOM.HTMLAnchorElement

{-# INLINE p_ #-}
p_ :: IsElementChildren a state => [NodePatch DOM.HTMLParagraphElement state] -> a -> Node state
p_ = el "p" DOM.HTMLParagraphElement

{-# INLINE input_ #-}
input_ :: IsElementChildren a state => [NodePatch DOM.HTMLInputElement state] -> a -> Node state
input_ = el "input" DOM.HTMLInputElement

{-# INLINE form_ #-}
form_ :: IsElementChildren a state => [NodePatch DOM.HTMLFormElement state] -> a -> Node state
form_ = el "form" DOM.HTMLFormElement

{-# INLINE button_ #-}
button_ :: IsElementChildren a state => [NodePatch DOM.HTMLButtonElement state] -> a -> Node state
button_ = el "button" DOM.HTMLButtonElement

{-# INLINE ul_ #-}
ul_ :: IsElementChildren a state => [NodePatch DOM.HTMLUListElement state] -> a -> Node state
ul_ = el "ul" DOM.HTMLUListElement

{-# INLINE li_ #-}
li_ :: IsElementChildren a state => [NodePatch DOM.HTMLLIElement state] -> a -> Node state
li_ = el "li" DOM.HTMLLIElement

{-# INLINE h2_ #-}
h2_ :: IsElementChildren a state => [NodePatch DOM.HTMLHeadingElement state] -> a -> Node state
h2_ = el "h2" DOM.HTMLHeadingElement

{-# INLINE h5_ #-}
h5_ :: IsElementChildren a state => [NodePatch DOM.HTMLHeadingElement state] -> a -> Node state
h5_ = el "h5" DOM.HTMLHeadingElement

{-# INLINE select_ #-}
select_ :: IsElementChildren a state => [NodePatch DOM.HTMLSelectElement state] -> a -> Node state
select_ = el "select" DOM.HTMLSelectElement

{-# INLINE option_ #-}
option_ :: IsElementChildren a state => [NodePatch DOM.HTMLOptionElement state] -> a -> Node state
option_ = el "option" DOM.HTMLOptionElement

{-# INLINE label_ #-}
label_ :: IsElementChildren a state => [NodePatch DOM.HTMLLabelElement state] -> a -> Node state
label_ = el "label" DOM.HTMLLabelElement

{-# INLINE nav_ #-}
nav_ :: IsElementChildren a state => [NodePatch DOM.HTMLElement state] -> a -> Node state
nav_ = el "nav" DOM.HTMLElement

{-# INLINE h1_ #-}
h1_ :: IsElementChildren a state => [NodePatch DOM.HTMLHeadingElement state] -> a -> Node state
h1_ = el "h1" DOM.HTMLHeadingElement

{-# INLINE h4_ #-}
h4_ :: IsElementChildren a state => [NodePatch DOM.HTMLHeadingElement state] -> a -> Node state
h4_ = el "h4" DOM.HTMLHeadingElement

{-# INLINE h6_ #-}
h6_ :: IsElementChildren a state => [NodePatch DOM.HTMLHeadingElement state] -> a -> Node state
h6_ = el "h6" DOM.HTMLHeadingElement

{-# INLINE small_ #-}
small_ :: IsElementChildren a state => [NodePatch DOM.HTMLElement state] -> a -> Node state
small_ = el "small" DOM.HTMLElement

{-# INLINE pre_ #-}
pre_ :: IsElementChildren a state => [NodePatch DOM.HTMLElement state] -> a -> Node state
pre_ = el "pre" DOM.HTMLElement

{-# INLINE code_ #-}
code_ :: IsElementChildren a state => [NodePatch DOM.HTMLElement state] -> a -> Node state
code_ = el "code" DOM.HTMLElement

{-# INLINE iframe_ #-}
iframe_ :: IsElementChildren a state => [NodePatch DOM.HTMLIFrameElement state] -> a -> Node state
iframe_ = el "iframe" DOM.HTMLIFrameElement

-- Properties
-- --------------------------------------------------------------------

{-# INLINE property #-}
property :: Text -> DOM.JSVal -> NodePatch el state
property k v = NPProperty k (return v)

{-# INLINE style #-}
style :: (DOM.IsElementCSSInlineStyle el) => Text -> Text -> NodePatch el state
style = NPStyle

class_ :: Text -> NodePatch el state
class_ txt = NPProperty "class" (DOM.toJSVal txt)

id_ ::  Text -> NodePatch el state
id_ txt = NPProperty "id" (DOM.toJSVal txt)

type_ :: Text -> NodePatch el state
type_ txt = NPProperty "type" (DOM.toJSVal txt)

href_ :: Text -> NodePatch el state
href_ txt = NPProperty "href" (DOM.toJSVal txt)

value_ :: Text -> NodePatch el state
value_ txt = NPProperty "value" (DOM.toJSVal txt)

checked_ :: Bool -> NodePatch el state
checked_ b = NPProperty "checked" (DOM.toJSVal b)

selected_ :: Bool -> NodePatch DOM.HTMLOptionElement state
selected_ b = NPProperty "selected" (DOM.toJSVal b)

disabled_ :: Bool -> NodePatch el state
disabled_ b = NPProperty "disabled" (DOM.toJSVal b)

{-# INLINE rawAttribute #-}
rawAttribute :: Text -> DOM.JSVal -> NodePatch el state
rawAttribute k v = NPAttribute k (return v)

{-# INLINE attribute #-}
attribute :: Text -> Text -> NodePatch el state
attribute k v = NPAttribute k (DOM.toJSVal v)

placeholder_ :: Text -> NodePatch el state
placeholder_ txt = NPProperty "placeholder" (DOM.toJSVal txt)

{-# INLINE for_ #-}
for_ :: Text -> NodePatch DOM.HTMLLabelElement state
for_ txt = NPProperty "for" (DOM.toJSVal txt)

{-# INLINE multiple_ #-}
multiple_ :: Bool -> NodePatch el state
multiple_ txt = NPProperty "multiple" (DOM.toJSVal txt)

{-# INLINE src_ #-}
src_ :: Text -> NodePatch el state
src_ txt = NPProperty "src" (DOM.toJSVal txt)

{-# INLINE onEvent #-}
onEvent ::
     (DOM.IsEventTarget t, DOM.IsEvent e, MonadAction state m, MonadUnliftIO m)
  => t -> DOM.EventM.EventName t e -> (e -> m ()) -> m (DOM.JSM ())
onEvent el_ evName f = do
  u <- askUnliftIO
  DOM.liftJSM $ DOM.EventM.on el_ evName $ do
    ev <- ask
    liftIO (unliftIO u (f ev))

-- Events
-- --------------------------------------------------------------------

onclick_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.MouseEvent -> Action state ()) -> NodePatch el state
onclick_ = NPEvent . SomeEventAction DOM.click

onchange_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action state ()) -> NodePatch el state
onchange_ = NPEvent . SomeEventAction DOM.change

oninput_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action state ()) -> NodePatch el state
oninput_ = NPEvent . SomeEventAction DOM.input

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action state ()) -> NodePatch el state
onsubmit_ = NPEvent . SomeEventAction DOM.submit

onselect_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.UIEvent -> Action state ()) -> NodePatch el state
onselect_ = NPEvent . SomeEventAction DOM.select

-- simple rendering
-- --------------------------------------------------------------------

simpleNode :: forall state. state ->  Node state -> DOM.JSM (V.Node V.SomeVDomNode)
simpleNode st node0 = do
  comp <- newComponent st (\() -> node0)
  (_, vdom) <- unAnapoM
    (do
      node <- _componentNode comp ()
      forSomeNodeBody node $ \node' -> do
        patches <- registerComponent (_componentPositions comp) ()
        patchNode patches node')
    ActionEnv
      { aeRegisterThread = \_ -> fail "Trying to register a thread from the simpleRenderNode"
      , aeHandleException = \_ -> fail "Trying to handle an exception from the simpleRenderNode"
      , aeDispatch = Dispatch (\_ _ -> fail "Trying to dispatch from the simpleRenderNode")
      , aeTraverseToComp = id
      , aeTraverseToState = id
      }
    AnapoEnv
      { aeReversePath = []
      , aePrevState = Nothing
      , aeState = st
      }
    ()
  return vdom

-- when we want a quick render of a component, e.g. inside a raw node.
-- any attempt to use dispatch will result in an exception; e.g. this
-- will never redraw anything, it's just to quickly draw some elements
simpleRenderNode :: state -> Node state -> DOM.JSM DOM.Node
simpleRenderNode st node = do
  vdom <- simpleNode st node
  renderVirtualDom vdom (return . renderedVDomNodeDom . V.nodeBody)

-- utils
-- --------------------------------------------------------------------

type UnliftJSM = UnliftIO

{-# INLINE askUnliftJSM #-}
askUnliftJSM :: (MonadUnliftIO m) => m (UnliftJSM m)
askUnliftJSM = askUnliftIO

{-# INLINE unliftJSM #-}
unliftJSM :: (DOM.MonadJSM n) => UnliftJSM m -> m a -> n a
unliftJSM u m = DOM.liftJSM (liftIO (unliftIO u m))

{-# INLINE actionUnliftJSM #-}
actionUnliftJSM :: (MonadAction state m) => m (UnliftJSM (Action state))
actionUnliftJSM = liftAction askUnliftJSM

-- Components
-- --------------------------------------------------------------------

forSomeNodeBody ::
     Monad m
  => V.Node V.SomeVDomNode
  -> (forall el. DOM.IsNode el => V.VDomNode el -> m (V.VDomNode el))
  -> m (V.Node V.SomeVDomNode)
forSomeNodeBody node f = case V.nodeBody node of
  V.SomeVDomNode node' -> do
    node'' <- f node'
    return node{V.nodeBody = V.SomeVDomNode node''}

data Component props state = Component
  { _componentState :: state
  , _componentNode :: props -> Node state
  , _componentPositions :: IORef (HMS.HashMap VDomPath props)
  }

{-# INLINE componentState #-}
componentState :: Lens' (Component props state) state
componentState = lens _componentState (\comp st -> comp{ _componentState = st })

{-# INLINE componentNode #-}
componentNode :: Lens' (Component props state) (props -> Node state)
componentNode = lens _componentNode (\comp st -> comp{ _componentNode = st })

{-# INLINE newComponent #-}
newComponent :: MonadIO m => state -> (props -> Node state) -> m (Component props state)
newComponent st node = do
  posRef <- liftIO (newIORef mempty)
  return (Component st node posRef)

registerComponent :: IORef (HMS.HashMap VDomPath props) -> props -> AnapoM dom a [NodePatch el state]
registerComponent ref props = AnapoM $ \_acEnv anEnv dom -> do
  let add = \_ -> do
        liftIO (modifyIORef' ref (HMS.insert (reverse (aeReversePath anEnv)) props))
  let remove = \_ -> do
        liftIO (modifyIORef' ref (HMS.delete (reverse (aeReversePath anEnv))))
  return
    ( dom
    , [ NPUnsafeWillMount add
      , NPUnsafeWillPatch remove
      , NPUnsafeDidPatch add
      , NPUnsafeWillRemove remove
      ]
    )

-- | This function will fail if you've already inserted the component in
-- the VDOM.
{-# INLINE component #-}
component :: props -> Node (Component props state)
component props = do
  comp <- ask
  node <- AnapoM $ \acEnv anEnv dom ->
    unAnapoM
      (_componentNode (aeState anEnv) props)
      acEnv
        { aeTraverseToComp = aeTraverseToComp acEnv.componentState.aeTraverseToState acEnv
        , aeTraverseToState = id
        }
      anEnv
        { aeState = _componentState (aeState anEnv) }
      dom
  forSomeNodeBody node $ \node' -> do
    patches <- registerComponent (_componentPositions comp) props
    patchNode patches node'

{-# INLINE componentL #-}
componentL :: Lens' out (Component props state) -> props -> Node out
componentL l props = zoomL l (component props)

{-# INLINE componentT #-}
componentT ::
     HasCallStack
  => Component props state
  -> AffineTraversal' out (Component props state)
  -- ^ note: if the traversal is not affine you'll get crashes.
  -> props
  -> Node out
componentT st l props = zoomT st l (component props)
