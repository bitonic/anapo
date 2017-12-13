{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | Note: we use 'Traversal' to keep a cursor to the
-- write end of the state, but really we should use an
-- "affine traversal" which guarantees we have either 0
-- or 1 positions to traverse. See
-- <https://www.reddit.com/r/haskell/comments/60fha5/affine_traversal/>
-- for why affine traversals do not play well with lens.
module Anapo.Component
  ( -- * Re-exports
    V.Rerender(..)

    -- * affine traversals
  , AffineTraversal
  , AffineTraversal'
  , toMaybeOf

    -- * Dispatch
  , Dispatch
  , runDispatchT
  , runDispatch

    -- * Register / handler
  , RegisterThread
  , HandleException
  , forkRegistered
  , Action(..)
  , runAction
  , dispatch
  , forkAction
  , zoomAction
  , MonadAction(..)

    -- * ComponentM
  , ComponentM(..)
  , Component
  , Component'
  , Node
  , Node'
  , runComponentM
  , askDispatch
  , localDispatch
  , askRegisterThread
  , localRegisterThread
  , askHandleException
  , localHandleException
  , askState
  , localState
  , askPreviousState
  , zoom
  , zoomL
  , zoomT

    -- * basic combinators
  , n
  , key
  , text
  , rawNode
  , marked

    -- * callbacks
  , unsafeWillMount
  , unsafeDidMount
  , unsafeWillPatch
  , unsafeDidPatch
  , unsafeWillRemove

    -- * raw html
  , UnsafeRawHtml(..)

    -- * elements
  , el
  , div_
  , span_
  , a_
  , p_
  , input_
  , form_
  , h1_
  , h2_
  , h4_
  , h5_
  , h6_
  , select_
  , option_
  , button_
  , ul_
  , li_
  , label_
  , multiple_
  , iframe_
  , small_
  , pre_
  , code_
  , nav_

    -- * attributes
  , SomeEventAction(..)
  , style
  , class_
  , id_
  , HasTypeProperty(..)
  , type_
  , HasHrefProperty(..)
  , href_
  , HasValueProperty(..)
  , value_
  , selected_
  , HasCheckedProperty(..)
  , checked_
  , HasDisabledProperty(..)
  , disabled_
  , src_
  , placeholder_
  , for_
  , rawProperty
  , rawAttribute
  , onEvent

    -- * events
  , onclick_
  , onchange_
  , onsubmit_
  , oninput_
  , onselect_

    -- * dom re-exports
  , DOM.preventDefault

    -- * simple rendering
  , simpleRenderComponent
  ) where

import qualified Data.HashMap.Strict as HMS
import Control.Lens (Lens', view, LensLike')
import qualified Control.Lens as Lens
import Control.Monad (ap)
import Data.Monoid ((<>), Endo)
import qualified Data.DList as DList
import Data.String (IsString(..))
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import GHC.Stack (HasCallStack)
import Control.Monad.Trans.State (execStateT, execState, StateT, State)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (askUnliftIO, unliftIO, MonadUnliftIO, UnliftIO(..))
import Control.Exception.Safe (SomeException, uninterruptibleMask, tryAny, throwIO)
import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader (ask)
import Data.DList (DList)

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM.EventM
import qualified GHCJS.DOM.HTMLInputElement as DOM.Input
import qualified GHCJS.DOM.HTMLButtonElement as DOM.Button
import qualified GHCJS.DOM.HTMLOptionElement as DOM.Option
import qualified GHCJS.DOM.HTMLLabelElement as DOM.Label
import qualified GHCJS.DOM.HTMLSelectElement as DOM.Select
import qualified GHCJS.DOM.HTMLIFrameElement as DOM.IFrame
import qualified GHCJS.DOM.HTMLTextAreaElement as DOM.TextArea
import qualified GHCJS.DOM.HTMLHyperlinkElementUtils as DOM.HyperlinkElementUtils
import qualified Data.Vector as Vec

import qualified Anapo.VDOM as V
import Anapo.Render
import Anapo.Logging
import Anapo.Text (Text, pack)
import qualified Anapo.Text as T

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

type Dispatch state = (state -> DOM.JSM state) -> IO ()

{-# INLINE runDispatchT #-}
runDispatchT :: MonadIO m => Dispatch state -> StateT state DOM.JSM () -> m ()
runDispatchT disp st = liftIO (disp (execStateT st))

{-# INLINE runDispatch #-}
runDispatch :: MonadIO m => Dispatch state -> State state () -> m ()
runDispatch disp st = liftIO (disp (return . execState st))

-- Register / handle
-- --------------------------------------------------------------------

-- we use these two on events and on forks, so that we can handle
-- all exceptions in a central place and so that we don't leave
-- dangling threads

type RegisterThread = IO () -> IO ()
type HandleException = SomeException -> IO ()

-- Action
-- --------------------------------------------------------------------

-- | An action we'll spawn from a component -- for example when an
-- event fires or as a fork in an event
newtype Action state a = Action
  { unAction ::
         RegisterThread
      -> HandleException
      -> Dispatch state
      -> DOM.JSM a
  }

{-# INLINE runAction #-}
runAction :: Action state a -> RegisterThread -> HandleException -> Dispatch state -> DOM.JSM a
runAction vdom = unAction vdom

instance Functor (Action state) where
  {-# INLINE fmap #-}
  fmap f (Action g) = Action $ \reg hdl d -> do
    x <- g reg hdl d
    return (f x)

instance Applicative (Action state) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Action state) where
  {-# INLINE return #-}
  return x = Action (\_reg _hdl _d -> return x)
  {-# INLINE (>>=) #-}
  ma >>= mf = Action $ \reg hdl d -> do
    x <- unAction ma reg hdl d
    unAction (mf x) reg hdl d

instance MonadIO (Action state) where
  {-# INLINE liftIO #-}
  liftIO m = Action (\_reg _hdl _d -> liftIO m)

#if !defined(ghcjs_HOST_OS)
instance  JSaddle.MonadJSM (Action state) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = Action (\_reg _hdl _d -> m)
#endif

instance MonadUnliftIO (Action state) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = Action $ \reg hdl d -> do
    u <- askUnliftIO
    return (UnliftIO (\(Action m) -> unliftIO u (m reg hdl d)))

class (DOM.MonadJSM m) => MonadAction state m | m -> state where
  liftAction :: Action state a -> m a

instance MonadAction state (Action state) where
  {-# INLINE liftAction #-}
  liftAction = id

instance MonadAction state (StateT s (Action state)) where
  {-# INLINE liftAction #-}
  liftAction = lift

{-# INLINE zoomAction #-}
zoomAction :: MonadAction writeOut m => LensLike' DOM.JSM writeOut writeIn -> Action writeIn a -> m a
zoomAction l m = liftAction (Action (\reg hdl disp -> runAction m reg hdl (disp . l)))

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

{-# INLINE forkAction #-}
forkAction :: MonadAction state m => Action state () -> m ThreadId
forkAction m = liftAction (Action (\reg hdl d -> forkRegistered reg hdl (unAction m reg hdl d)))

{-# INLINE dispatch #-}
dispatch :: MonadAction write m => StateT write (Action write) () -> m ()
dispatch m = liftAction (Action (\reg hdl d -> liftIO (d (\st -> unAction (execStateT m st) reg hdl d))))

{-# INLINE askDispatch #-}
askDispatch :: (MonadAction state m) => m (Dispatch state)
askDispatch = liftAction (Action (\_reg _hdl d -> return d))

{-# INLINE askRegisterThread #-}
askRegisterThread :: (MonadAction state m) => m RegisterThread
askRegisterThread = liftAction (Action (\reg _hdl _d -> return reg))

{-# INLINE askHandleException #-}
askHandleException :: (MonadAction state m) => m HandleException
askHandleException = liftAction (Action (\_reg hdl _d -> return hdl))

-- Monad
-- --------------------------------------------------------------------

newtype ComponentM dom read write a = ComponentM
  { unComponentM ::
         RegisterThread
      -- how to register threads resulting from events / forks
      -> HandleException
      -- how to handle exceptions that happen in events / forks
      -> Dispatch write
      -- how to dispatch updates to the state
      -> Maybe write
      -- the previous state
      -> read
      -- the current state
      -> DOM.JSM (dom, a)
  }

instance (Monoid dom, a ~ ()) => Monoid (ComponentM dom read write a) where
  {-# INLINE mempty #-}
  mempty = return ()
  {-# INLINE mappend #-}
  a `mappend` b = a >> b

type ComponentM' dom state = ComponentM dom state state
type Component' state = ComponentM (DList (V.Node V.SomeVDomNode)) state state ()
type Component read write = ComponentM (DList (V.Node V.SomeVDomNode)) read write ()
type Node read write = ComponentM () read write (V.Node V.SomeVDomNode)
type Node' state = ComponentM () state state (V.Node V.SomeVDomNode)

instance Monoid dom => MonadIO (ComponentM dom read write) where
  {-# INLINE liftIO #-}
  liftIO m = ComponentM $ \_reg _hdl _d _mbst _st -> do
    x <- liftIO m
    return (mempty, x)

#if !defined(ghcjs_HOST_OS)
instance Monoid dom => JSaddle.MonadJSM (ComponentM dom read write) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = ComponentM $ \_reg _hdl _d _mbst _st -> do
    x <- m
    return (mempty, x)
#endif

{-# INLINE runComponentM #-}
runComponentM :: ComponentM dom read write a -> RegisterThread -> HandleException -> Dispatch write -> Maybe write -> read -> DOM.JSM (dom, a)
runComponentM vdom = unComponentM vdom

instance Functor (ComponentM dom read write) where
  {-# INLINE fmap #-}
  fmap f (ComponentM g) = ComponentM $ \reg hdl d mbst st -> do
    (dom, x) <- g reg hdl d mbst st
    return (dom, f x)

instance (Monoid dom) => Applicative (ComponentM dom read write) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Monoid dom) => Monad (ComponentM dom read write) where
  {-# INLINE return #-}
  return x = ComponentM (\_reg _hdl _d _mbst _st -> return (mempty, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = ComponentM $ \reg hdl d mbst st -> do
    (vdom1, x) <- unComponentM ma reg hdl d mbst st
    (vdom2, y) <- unComponentM (mf x) reg hdl d mbst st
    let !vdom = vdom1 <> vdom2
    return (vdom, y)

instance (Monoid dom) => MonadAction write (ComponentM dom read write) where
  {-# INLINE liftAction #-}
  liftAction (Action f) = ComponentM $ \reg hdl d _mbst _st -> do
    x <- f reg hdl d
    return (mempty, x)

{-# INLINE localDispatch #-}
localDispatch :: (Monoid dom) => Dispatch write' -> Maybe write' -> ComponentM dom read write' a -> ComponentM dom read write a
localDispatch d mbst comp = ComponentM (\reg hdl _d _mbst st -> unComponentM comp reg hdl d mbst st)

{-# INLINE localRegisterThread #-}
localRegisterThread :: (Monoid dom) => RegisterThread -> ComponentM dom read write a -> ComponentM dom read write a
localRegisterThread reg comp = ComponentM (\_reg hdl d mbst st -> unComponentM comp reg hdl d mbst st)

{-# INLINE localHandleException #-}
localHandleException :: (Monoid dom) => HandleException -> ComponentM dom read write a -> ComponentM dom read write a
localHandleException hdl comp = ComponentM (\reg _hdl d mbst st -> unComponentM comp reg hdl d mbst st)

{-# INLINE askState #-}
askState :: (Monoid dom) => ComponentM dom read write read
askState = ComponentM (\_reg _hdl _d _mbst st -> return (mempty, st))

{-# INLINE localState #-}
localState :: (Monoid dom) => read' -> ComponentM dom read' write a -> ComponentM dom read write a
localState st comp = ComponentM (\reg hdl d mbst _st -> runComponentM comp reg hdl d mbst st)

{-# INLINE askPreviousState #-}
askPreviousState :: (Monoid dom) => ComponentM dom read write (Maybe write)
askPreviousState = ComponentM (\_reg _hdl _d mbst _st -> return (mempty, mbst))

{-# INLINE zoom #-}
zoom ::
     readIn
  -> (writeOut -> Maybe writeIn)
  -> LensLike' DOM.JSM writeOut writeIn
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoom st' get write' dom = ComponentM $ \reg hdl d mbst _st ->
  unComponentM dom reg hdl (d . write') (mbst >>= get) st'

{-# INLINE zoomL #-}
zoomL :: Lens' out in_ -> ComponentM' dom in_ a -> ComponentM' dom out a
zoomL l dom = ComponentM $ \reg hdl d mbst st ->
  unComponentM dom reg hdl (d . l) (view l <$> mbst) (view l st)

{-# INLINE zoomT #-}
zoomT ::
     HasCallStack
  => readIn
  -> AffineTraversal' writeOut writeIn
  -- ^ note: if the traversal is not affine you'll get crashes.
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoomT st l = zoom st (toMaybeOf l) l

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
n :: Node read write -> Component read write
n getNode = ComponentM $ \reg hdl d mbst st -> do
  (_, nod) <- unComponentM getNode reg hdl d mbst st
  return (DList.singleton nod, ())

-- TODO right now this it not implemented, but we will implement it in
-- the future.
{-# INLINE key #-}
key :: Text -> Node read write -> Component read write
key _k = n

{-# INLINE text #-}
text :: Text -> Node read write
text txt = return $ V.Node
  { V.nodeBody = V.SomeVDomNode $ V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBText txt
      , V.vdomCallbacks = mempty
      , V.vdomWrap = DOM.Text
      }
  , V.nodeChildren = Nothing
  }

instance (el ~ DOM.Text) => IsString (Node read write) where
  {-# INLINE fromString #-}
  fromString = text . T.pack

{-# INLINE rawNode #-}
rawNode ::
     (DOM.IsNode el)
  => (DOM.JSVal -> el) -> el
  -> [NodePatch el write]
  -> Node read write
rawNode wrap x patches = do
  node <- patchNode
    V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBRawNode x
      , V.vdomCallbacks = mempty
      , V.vdomWrap = wrap
      }
    patches
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
     (Maybe write -> read -> V.Rerender)
  -> StaticPtr (Node read write) -> Node read write
marked shouldRerender ptr = ComponentM $ \reg hdl d mbst st -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender mbst st
  (_, V.Node (V.SomeVDomNode nod) children) <- unComponentM (deRefStaticPtr ptr) reg hdl d mbst st
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
  | NPProperty V.ElementPropertyName (V.ElementProperty el)
  | NPEvent (SomeEventAction el state)

class IsElementChildren a read write where
  elementChildren :: a -> ComponentM () read write (V.Children V.SomeVDomNode)
instance IsElementChildren () read write where
  {-# INLINE elementChildren #-}
  elementChildren _ = return (V.CNormal mempty)
instance (a ~ (), read1 ~ read2, write1 ~ write2) => IsElementChildren (ComponentM (DList (V.Node V.SomeVDomNode)) read1 write1 a) read2 write2 where
  {-# INLINE elementChildren #-}
  elementChildren (ComponentM f) = ComponentM $ \reg hdl disp mbSt st -> do
    (dom, _) <- f reg hdl disp mbSt st
    return ((), V.CNormal (Vec.fromList (DList.toList dom)))
instance (a ~ ()) => IsElementChildren UnsafeRawHtml read2 write2 where
  {-# INLINE elementChildren #-}
  elementChildren (UnsafeRawHtml txt) = return (V.CRawHtml txt)

{-# INLINE patchNode #-}
patchNode ::
     (HasCallStack, DOM.IsNode el)
  => V.VDomNode el -> [NodePatch el write] -> ComponentM () read write (V.VDomNode el)
patchNode node00 patches00 = do
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
        NPProperty propName propBody -> go
          (modifyElement node $ \vel -> vel
            { V.elementProperties =
                HMS.insert propName propBody (V.elementProperties vel)
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
     ( IsElementChildren a read write
     , DOM.IsElement el, DOM.IsElementCSSInlineStyle el
     , HasCallStack
     )
  => V.ElementTag
  -> (DOM.JSVal -> el)
  -> [NodePatch el write]
  -> a
  -> Node read write
el tag wrap patches isChildren = do
  children <- elementChildren isChildren
  vdom <- patchNode
    V.VDomNode
      { V.vdomMark = Nothing
      , V.vdomBody = V.VDBElement V.Element
          { V.elementTag = tag
          , V.elementProperties = mempty
          , V.elementStyle = mempty
          , V.elementEvents = mempty
          }
      , V.vdomCallbacks = mempty
      , V.vdomWrap = wrap
      }
    patches
  return V.Node
    { V.nodeBody = V.SomeVDomNode vdom
    , V.nodeChildren = Just children
    }

-- Elements
-- --------------------------------------------------------------------

{-# INLINE div_ #-}
div_ :: IsElementChildren a read write => [NodePatch DOM.HTMLDivElement write] -> a -> Node read write
div_ = el "div" DOM.HTMLDivElement

{-# INLINE span_ #-}
span_ :: IsElementChildren a read write => [NodePatch DOM.HTMLSpanElement write] -> a -> Node read write
span_ = el "span" DOM.HTMLSpanElement

{-# INLINE a_ #-}
a_ :: IsElementChildren a read write => [NodePatch DOM.HTMLAnchorElement write] -> a -> Node read write
a_ = el "a" DOM.HTMLAnchorElement

{-# INLINE p_ #-}
p_ :: IsElementChildren a read write => [NodePatch DOM.HTMLParagraphElement write] -> a -> Node read write
p_ = el "p" DOM.HTMLParagraphElement

{-# INLINE input_ #-}
input_ :: IsElementChildren a read write => [NodePatch DOM.HTMLInputElement write] -> a -> Node read write
input_ = el "input" DOM.HTMLInputElement

{-# INLINE form_ #-}
form_ :: IsElementChildren a read write => [NodePatch DOM.HTMLFormElement write] -> a -> Node read write
form_ = el "form" DOM.HTMLFormElement

{-# INLINE button_ #-}
button_ :: IsElementChildren a read write => [NodePatch DOM.HTMLButtonElement write] -> a -> Node read write
button_ = el "button" DOM.HTMLButtonElement

{-# INLINE ul_ #-}
ul_ :: IsElementChildren a read write => [NodePatch DOM.HTMLUListElement write] -> a -> Node read write
ul_ = el "ul" DOM.HTMLUListElement

{-# INLINE li_ #-}
li_ :: IsElementChildren a read write => [NodePatch DOM.HTMLLIElement write] -> a -> Node read write
li_ = el "li" DOM.HTMLLIElement

{-# INLINE h2_ #-}
h2_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node read write
h2_ = el "h2" DOM.HTMLHeadingElement

{-# INLINE h5_ #-}
h5_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node read write
h5_ = el "h5" DOM.HTMLHeadingElement

{-# INLINE select_ #-}
select_ :: IsElementChildren a read write => [NodePatch DOM.HTMLSelectElement write] -> a -> Node read write
select_ = el "select" DOM.HTMLSelectElement

{-# INLINE option_ #-}
option_ :: IsElementChildren a read write => [NodePatch DOM.HTMLOptionElement write] -> a -> Node read write
option_ = el "option" DOM.HTMLOptionElement

{-# INLINE label_ #-}
label_ :: IsElementChildren a read write => [NodePatch DOM.HTMLLabelElement write] -> a -> Node read write
label_ = el "label" DOM.HTMLLabelElement

{-# INLINE nav_ #-}
nav_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node read write
nav_ = el "nav" DOM.HTMLElement

{-# INLINE h1_ #-}
h1_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node read write
h1_ = el "h1" DOM.HTMLHeadingElement

{-# INLINE h4_ #-}
h4_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node read write
h4_ = el "h4" DOM.HTMLHeadingElement

{-# INLINE h6_ #-}
h6_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node read write
h6_ = el "h6" DOM.HTMLHeadingElement

{-# INLINE small_ #-}
small_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node read write
small_ = el "small" DOM.HTMLElement

{-# INLINE pre_ #-}
pre_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node read write
pre_ = el "pre" DOM.HTMLElement

{-# INLINE code_ #-}
code_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node read write
code_ = el "code" DOM.HTMLElement

{-# INLINE iframe_ #-}
iframe_ :: IsElementChildren a read write => [NodePatch DOM.HTMLIFrameElement write] -> a -> Node read write
iframe_ = el "iframe" DOM.HTMLIFrameElement

-- Properties
-- --------------------------------------------------------------------

{-# INLINE style #-}
style :: (DOM.IsElementCSSInlineStyle el) => Text -> Text -> NodePatch el write
style = NPStyle

class_ :: (DOM.IsElement el) => Text -> NodePatch el write
class_ txt = NPProperty "class" $ V.ElementProperty
  { V.eaGetProperty = DOM.getClassName
  , V.eaSetProperty = DOM.setClassName
  , V.eaValue = return txt
  }

id_ :: (DOM.IsElement el) => Text -> NodePatch el write
id_ txt = NPProperty "id" $ V.ElementProperty
  { V.eaGetProperty = DOM.getId
  , V.eaSetProperty = DOM.setId
  , V.eaValue = return txt
  }

class HasTypeProperty el where
  htpGetType :: el -> DOM.JSM Text
  htpSetType :: el -> Text -> DOM.JSM ()

instance HasTypeProperty DOM.HTMLInputElement where
  htpGetType = DOM.Input.getType
  htpSetType = DOM.Input.setType

instance HasTypeProperty DOM.HTMLButtonElement where
  htpGetType = DOM.Button.getType
  htpSetType = DOM.Button.setType

type_ :: (HasTypeProperty el) => Text -> NodePatch el write
type_ txt = NPProperty "type" $ V.ElementProperty
  { V.eaGetProperty = htpGetType
  , V.eaSetProperty = htpSetType
  , V.eaValue = return txt
  }

class HasHrefProperty el where
  htpGetHref :: el -> DOM.JSM Text
  htpSetHref :: el -> Text -> DOM.JSM ()

instance HasHrefProperty DOM.HTMLAnchorElement where
  htpGetHref = DOM.HyperlinkElementUtils.getHref
  htpSetHref = DOM.HyperlinkElementUtils.setHref

href_ :: (HasHrefProperty el) => Text -> NodePatch el write
href_ txt = NPProperty "href" $ V.ElementProperty
  { V.eaGetProperty = htpGetHref
  , V.eaSetProperty = htpSetHref
  , V.eaValue = return txt
  }

class HasValueProperty el where
  hvpGetValue :: el -> DOM.JSM Text
  hvpSetValue :: el -> Text -> DOM.JSM ()

instance HasValueProperty DOM.HTMLInputElement where
  hvpGetValue = DOM.Input.getValue
  hvpSetValue = DOM.Input.setValue

instance HasValueProperty DOM.HTMLOptionElement where
  hvpGetValue = DOM.Option.getValue
  hvpSetValue = DOM.Option.setValue

value_ :: (HasValueProperty el) => Text -> NodePatch el write
value_ txt = NPProperty "value" $ V.ElementProperty
  { V.eaGetProperty = hvpGetValue
  , V.eaSetProperty = hvpSetValue
  , V.eaValue = return txt
  }

class HasCheckedProperty el where
  hcpGetChecked :: el -> DOM.JSM Bool
  hcpSetChecked :: el -> Bool -> DOM.JSM ()

instance HasCheckedProperty DOM.HTMLInputElement where
  hcpGetChecked = DOM.Input.getChecked
  hcpSetChecked = DOM.Input.setChecked

checked_ :: (HasCheckedProperty el) => Bool -> NodePatch el write
checked_ b = NPProperty "checked" $ V.ElementProperty
  { V.eaGetProperty = hcpGetChecked
  , V.eaSetProperty = hcpSetChecked
  , V.eaValue = return b
  }

selected_ :: Bool -> NodePatch DOM.HTMLOptionElement write
selected_ b = NPProperty "selected" $ V.ElementProperty
  { V.eaGetProperty = DOM.Option.getSelected
  , V.eaSetProperty = DOM.Option.setSelected
  , V.eaValue = return b
  }

class HasDisabledProperty el where
  hdpGetDisabled :: el -> DOM.JSM Bool
  hdpSetDisabled :: el -> Bool -> DOM.JSM ()

instance HasDisabledProperty DOM.HTMLButtonElement where
  hdpGetDisabled = DOM.Button.getDisabled
  hdpSetDisabled = DOM.Button.setDisabled

disabled_ :: HasDisabledProperty el => Bool -> NodePatch el write
disabled_ b = NPProperty "disabled" $ V.ElementProperty
  { V.eaGetProperty = hdpGetDisabled
  , V.eaSetProperty = hdpSetDisabled
  , V.eaValue = return b
  }

#if defined(ghcjs_HOST_OS)
-- Raw FFI on js for performance

foreign import javascript unsafe
  "$2[$1]"
  js_getProperty :: Text -> JSVal -> IO JSVal

foreign import javascript unsafe
  "$2[$1] = $3"
  js_setProperty :: Text -> JSVal -> JSVal -> IO ()

rawProperty :: (DOM.PToJSVal el, DOM.ToJSVal a) => Text -> a -> NodePatch el write
rawProperty k x = NPProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> js_getProperty k (DOM.pToJSVal el_)
  , V.eaSetProperty = \el_ y -> do
      js_setProperty k (DOM.pToJSVal el_) y
  , V.eaValue = DOM.toJSVal x
  }
#else
rawProperty :: (JSaddle.MakeObject el, DOM.ToJSVal a) => Text -> a -> NodePatch el write
rawProperty k x = NPProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> el_ JSaddle.! k
  , V.eaSetProperty = \el_ y -> (el_ JSaddle.<# k) y
  , V.eaValue = DOM.toJSVal x
  }
#endif

{-# INLINE rawAttribute #-}
rawAttribute :: (DOM.IsElement el) => Text -> Text -> NodePatch el write
rawAttribute k x = NPProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> fromMaybe "" <$> DOM.getAttribute el_ k
  , V.eaSetProperty = \el_ y -> DOM.setAttribute el_ k y
  , V.eaValue = return x
  }

class HasPlaceholderProperty el where
  getPlaceholder :: el -> DOM.JSM Text
  setPlaceholder :: el -> Text -> DOM.JSM ()

instance HasPlaceholderProperty DOM.HTMLInputElement where
  getPlaceholder = DOM.Input.getPlaceholder
  setPlaceholder = DOM.Input.setPlaceholder

instance HasPlaceholderProperty DOM.HTMLTextAreaElement where
  getPlaceholder = DOM.TextArea.getPlaceholder
  setPlaceholder = DOM.TextArea.setPlaceholder

placeholder_ :: (HasPlaceholderProperty el) => Text -> NodePatch el write
placeholder_ txt = NPProperty "placeholder" $ V.ElementProperty
  { V.eaGetProperty = getPlaceholder
  , V.eaSetProperty = setPlaceholder
  , V.eaValue = return txt
  }

{-# INLINE for_ #-}
for_ :: Text -> NodePatch DOM.HTMLLabelElement write
for_ txt = NPProperty "for" $ V.ElementProperty
  { V.eaGetProperty = DOM.Label.getHtmlFor
  , V.eaSetProperty = DOM.Label.setHtmlFor
  , V.eaValue = return txt
  }

class HasMultipleProperty el where
  getMultiple :: el -> DOM.JSM Bool
  setMultiple :: el -> Bool -> DOM.JSM ()

instance HasMultipleProperty DOM.HTMLInputElement where
  getMultiple = DOM.Input.getMultiple
  setMultiple = DOM.Input.setMultiple

instance HasMultipleProperty DOM.HTMLSelectElement where
  getMultiple = DOM.Select.getMultiple
  setMultiple = DOM.Select.setMultiple

{-# INLINE multiple_ #-}
multiple_ :: (HasMultipleProperty el) => Bool -> NodePatch el write
multiple_ txt = NPProperty "multiple" $ V.ElementProperty
  { V.eaGetProperty = getMultiple
  , V.eaSetProperty = setMultiple
  , V.eaValue = return txt
  }

class HasSrcProperty el where
  getSrc :: el -> DOM.JSM Text
  setSrc :: el -> Text -> DOM.JSM ()

instance HasSrcProperty DOM.HTMLIFrameElement where
  getSrc = DOM.IFrame.getSrc
  setSrc = DOM.IFrame.setSrc

{-# INLINE src_ #-}
src_ :: (HasSrcProperty el) => Text -> NodePatch el write
src_ txt = NPProperty "src" $ V.ElementProperty
  { V.eaGetProperty = getSrc
  , V.eaSetProperty = setSrc
  , V.eaValue = return txt
  }

{-# INLINE onEvent #-}
onEvent ::
     (DOM.IsEventTarget t, DOM.IsEvent e, MonadAction write m, MonadUnliftIO m)
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
  => (el -> DOM.MouseEvent -> Action write ()) -> NodePatch el write
onclick_ = NPEvent . SomeEventAction DOM.click

onchange_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action write ()) -> NodePatch el write
onchange_ = NPEvent . SomeEventAction DOM.change

oninput_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action write ()) -> NodePatch el write
oninput_ = NPEvent . SomeEventAction DOM.input

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action write ()) -> NodePatch el write
onsubmit_ = NPEvent . SomeEventAction DOM.submit

onselect_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.UIEvent -> Action write ()) -> NodePatch el write
onselect_ = NPEvent . SomeEventAction DOM.select

-- simple rendering
-- --------------------------------------------------------------------

-- when we want a quick render of a component, e.g. inside a raw node.
-- any attempt to use dispatch will result in an exception; e.g. this
-- will never redraw anything, it's just to quickly draw some elements
simpleRenderComponent :: read -> Node read () -> DOM.JSM DOM.Node
simpleRenderComponent st comp = do
  (_, vdom) <- runComponentM
    comp
    (\_ -> fail "Trying to registering a thread from simpleRenderComponent")
    throwIO
    (\_ -> fail "Trying to dispatch from simpleRenderComponent!")
    Nothing
    st
  renderVirtualDom vdom (return . renderedVDomNodeDom . V.nodeBody)