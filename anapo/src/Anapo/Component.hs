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
  , Dispatch'
  , runDispatchT
  , runDispatch

    -- * Register / handler
  , RegisterThread
  , HandleException
  , forkRegistered
  , Action(..)
  , Action'
  , runAction
  , dispatch
  , forkAction
  , zoomAction
  , MonadAction(..)
  , MonadAction'

    -- * ComponentM
  , ComponentM(..)
  , Component
  , Component'
  , KeyedComponent
  , KeyedComponent'
  , Node
  , Node'
  , runComponentM
  , runComponent
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
import Control.Monad (ap, void)
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
import qualified GHCJS.DOM as DOM

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

-- | for the components we want affine traversals -- traversals which
-- point either to one or to zero elements. however lens currently
-- does not provide them, see
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

type Dispatch in_ out = (in_ -> DOM.JSM out) -> IO ()
type Dispatch' state = Dispatch state state

{-# INLINE runDispatchT #-}
runDispatchT :: MonadIO m => Dispatch' state -> StateT state DOM.JSM () -> m ()
runDispatchT disp st = liftIO (disp (execStateT st))

{-# INLINE runDispatch #-}
runDispatch :: MonadIO m => Dispatch' state -> State state () -> m ()
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
newtype Action in_ out a = Action
  { unAction ::
         RegisterThread
      -> HandleException
      -> Dispatch in_ out
      -> DOM.JSM a
  }
type Action' state = Action state state

{-# INLINE runAction #-}
runAction :: Action in_ out a -> RegisterThread -> HandleException -> Dispatch in_ out -> DOM.JSM a
runAction vdom = unAction vdom

instance Functor (Action in_ out) where
  {-# INLINE fmap #-}
  fmap f (Action g) = Action $ \reg hdl d -> do
    x <- g reg hdl d
    return (f x)

instance Applicative (Action in_ out) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Action in_ out) where
  {-# INLINE return #-}
  return x = Action (\_reg _hdl _d -> return x)
  {-# INLINE (>>=) #-}
  ma >>= mf = Action $ \reg hdl d -> do
    x <- unAction ma reg hdl d
    unAction (mf x) reg hdl d

instance MonadIO (Action in_ out) where
  {-# INLINE liftIO #-}
  liftIO m = Action (\_reg _hdl _d -> liftIO m)

#if !defined(ghcjs_HOST_OS)
instance  JSaddle.MonadJSM (Action in_ out) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = Action (\_reg _hdl _d -> m)
#endif

instance MonadUnliftIO (Action in_ out) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = Action $ \reg hdl d -> do
    u <- askUnliftIO
    return (UnliftIO (\(Action m) -> unliftIO u (m reg hdl d)))

class (DOM.MonadJSM m) => MonadAction in_ out m | m -> in_, m -> out where
  liftAction :: Action in_ out a -> m a
type MonadAction' state = MonadAction state state

instance MonadAction in_ out (Action in_ out) where
  {-# INLINE liftAction #-}
  liftAction = id

instance MonadAction in_ out (StateT s (Action in_ out)) where
  {-# INLINE liftAction #-}
  liftAction = lift

{-# INLINE zoomAction #-}
zoomAction :: MonadAction' writeOut m => LensLike' DOM.JSM writeOut writeIn -> Action' writeIn a -> m a
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
forkAction :: MonadAction in_ out m => Action in_ out () -> m ThreadId
forkAction m = liftAction (Action (\reg hdl d -> forkRegistered reg hdl (unAction m reg hdl d)))

{-# INLINE dispatch #-}
dispatch :: MonadAction' write m => StateT write (Action' write) () -> m ()
dispatch m = liftAction (Action (\reg hdl d -> liftIO (d (\st -> unAction (execStateT m st) reg hdl d))))

{-# INLINE askDispatch #-}
askDispatch :: (MonadAction in_ out m) => m (Dispatch in_ out)
askDispatch = liftAction (Action (\_reg _hdl d -> return d))

{-# INLINE askRegisterThread #-}
askRegisterThread :: (MonadAction in_ out m) => m RegisterThread
askRegisterThread = liftAction (Action (\reg _hdl _d -> return reg))

{-# INLINE askHandleException #-}
askHandleException :: (MonadAction in_ out m) => m HandleException
askHandleException = liftAction (Action (\_reg hdl _d -> return hdl))

-- Monad
-- --------------------------------------------------------------------

newtype ComponentM dom read write a = ComponentM
  { unComponentM ::
         RegisterThread
      -- how to register threads resulting from events / forks
      -> HandleException
      -- how to handle exceptions that happen in events / forks
      -> Dispatch' write
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
type Component' state = ComponentM V.Dom state state ()
type Component read write = ComponentM V.Dom read write ()
type KeyedComponent' state = ComponentM V.KeyedDom state state ()
type KeyedComponent read write = ComponentM V.KeyedDom read write ()
type Node el read write = ComponentM () read write (V.Node el)
type Node' el state = ComponentM () state state (V.Node el)

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

instance (el ~ DOM.Text) => IsString (Node el read write) where
  {-# INLINE fromString #-}
  fromString = text . T.pack

{-# INLINE runComponentM #-}
runComponentM :: ComponentM dom read write a -> RegisterThread -> HandleException -> Dispatch' write -> Maybe write -> read -> DOM.JSM (dom, a)
runComponentM vdom = unComponentM vdom

{-# INLINE runComponent #-}
runComponent :: Component read write -> RegisterThread -> HandleException -> Dispatch' write -> Maybe write -> read -> DOM.JSM V.Dom
runComponent vdom reg hdl d mbst st = fst <$> unComponentM vdom reg hdl d mbst st

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

instance (Monoid dom) => MonadAction write write (ComponentM dom read write) where
  {-# INLINE liftAction #-}
  liftAction (Action f) = ComponentM $ \reg hdl d _mbst _st -> do
    x <- f reg hdl d
    return (mempty, x)

{-# INLINE localDispatch #-}
localDispatch :: (Monoid dom) => Dispatch' write' -> Maybe write' -> ComponentM dom read write' a -> ComponentM dom read write a
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
unsafeWillMount :: (el -> Action' state ()) -> NodePatch el state
unsafeWillMount = NPUnsafeWillMount

{-# INLINE unsafeDidMount #-}
unsafeDidMount :: (el -> Action' state ()) -> NodePatch el state
unsafeDidMount = NPUnsafeDidMount

{-# INLINE unsafeWillPatch #-}
unsafeWillPatch :: (el -> Action' state ()) -> NodePatch el state
unsafeWillPatch = NPUnsafeWillPatch

{-# INLINE unsafeDidPatch #-}
unsafeDidPatch :: (el -> Action' state ()) -> NodePatch el state
unsafeDidPatch = NPUnsafeDidPatch

{-# INLINE unsafeWillRemove #-}
unsafeWillRemove :: (el -> Action' state ()) -> NodePatch el state
unsafeWillRemove = NPUnsafeWillRemove

-- useful shorthands
-- --------------------------------------------------------------------

{-# INLINE n #-}
n :: (DOM.IsNode el) => Node el read write -> Component read write
n getNode = ComponentM $ \reg hdl d mbst st -> do
  (_, nod) <- unComponentM getNode reg hdl d mbst st
  return (DList.singleton (V.SomeNode nod), ())

{-# INLINE key #-}
key :: (DOM.IsNode el) => Text -> Node el read write -> KeyedComponent read write
key k getNode = ComponentM $ \reg hdl d mbst st -> do
  (_, nod) <- unComponentM getNode reg hdl d mbst st
  return (V.KeyedDom (DList.singleton (k, V.SomeNode nod)), ())

{-# INLINE text #-}
text :: Text -> Node DOM.Text read write
text txt = return $ V.Node
  { V.nodeMark = Nothing
  , V.nodeBody = V.NBText txt
  , V.nodeCallbacks = mempty
  , V.nodeWrap = DOM.Text
  }

{-# INLINE rawNode #-}
rawNode :: (DOM.IsNode el) => (DOM.JSVal -> el) -> el -> Node el read write
rawNode wrap x = return $ V.Node
  { V.nodeMark = Nothing
  , V.nodeBody = V.NBRawNode x
  , V.nodeCallbacks = mempty
  , V.nodeWrap = wrap
  }

-- TODO this causes linking errors, sometimes. bizzarely,
-- the linking errors seem to happen only if a closure is
-- formed -- e.g. if we define the function as
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
  -> StaticPtr (Node el read write) -> Node el read write
marked shouldRerender ptr = ComponentM $ \reg hdl d mbst st -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender mbst st
  (_, nod) <- unComponentM (deRefStaticPtr ptr) reg hdl d mbst st
  return ((), nod{ V.nodeMark = Just (V.Mark fprint rer) })

-- Utilities to quickly create nodes
-- --------------------------------------------------------------------

data SomeEventAction el write = forall e. (DOM.IsEvent e) =>
  SomeEventAction (DOM.EventM.EventName el e) (el -> e -> Action' write ())
newtype UnsafeRawHtml = UnsafeRawHtml Text

data NodePatch el state =
    NPUnsafeWillMount (el -> Action' state ())
  | NPUnsafeDidMount (el -> Action' state ())
  | NPUnsafeWillPatch (el -> Action' state ())
  | NPUnsafeDidPatch (el -> Action' state ())
  | NPUnsafeWillRemove (el -> Action' state ())
  | NPStyle V.StylePropertyName V.StyleProperty
  | NPProperty V.ElementPropertyName (V.ElementProperty el)
  | NPEvent (SomeEventAction el state)

class IsElementChildren a read write where
  elementChildren :: a -> ComponentM () read write V.Children
instance IsElementChildren () read write where
  {-# INLINE elementChildren #-}
  elementChildren _ = return (V.CNormal mempty)
instance (a ~ (), read1 ~ read2, write1 ~ write2) => IsElementChildren (ComponentM V.Dom read1 write1 a) read2 write2 where
  {-# INLINE elementChildren #-}
  elementChildren (ComponentM f) = ComponentM $ \reg hdl disp mbSt st -> do
    (dom, _) <- f reg hdl disp mbSt st
    return ((), V.CNormal dom)
instance (a ~ (), read1 ~ read2, write1 ~ write2) => IsElementChildren (ComponentM V.KeyedDom read1 write1 a) read2 write2 where
  {-# INLINE elementChildren #-}
  elementChildren (ComponentM f) = ComponentM $ \reg hdl disp mbSt st -> do
    (dom, _) <- f reg hdl disp mbSt st
    return ((), V.CKeyed dom)
instance (a ~ ()) => IsElementChildren UnsafeRawHtml read2 write2 where
  {-# INLINE elementChildren #-}
  elementChildren (UnsafeRawHtml txt) = return (V.CRawHtml txt)

{-# INLINE patchNode #-}
patchNode ::
     (HasCallStack)
  => V.Node el -> [NodePatch el write] -> Node el read write
patchNode node00 patches00 = do
  u <- liftAction askUnliftIO
  let
    go !node = \case
      [] -> return node
      patch : patches -> case patch of
        NPUnsafeWillMount cback -> go
          node
            { V.nodeCallbacks = mappend
                (V.nodeCallbacks node)
                mempty{ V.callbacksUnsafeWillMount = \e -> liftIO (unliftIO u (cback e)) }
            }
          patches
        NPUnsafeDidMount cback -> go
          node
            { V.nodeCallbacks = mappend
                (V.nodeCallbacks node)
                mempty{ V.callbacksUnsafeDidMount = \e -> liftIO (unliftIO u (cback e)) }
            }
          patches
        NPUnsafeWillPatch cback -> go
          node
            { V.nodeCallbacks = mappend
                (V.nodeCallbacks node)
                mempty{ V.callbacksUnsafeWillPatch = \e -> liftIO (unliftIO u (cback e)) }
            }
          patches
        NPUnsafeDidPatch cback -> go
          node
            { V.nodeCallbacks = mappend
                (V.nodeCallbacks node)
                mempty{ V.callbacksUnsafeDidPatch = \e -> liftIO (unliftIO u (cback e)) }
            }
          patches
        NPUnsafeWillRemove cback -> go
          node
            { V.nodeCallbacks = mappend
                (V.nodeCallbacks node)
                mempty{ V.callbacksUnsafeWillRemove = \e -> liftIO (unliftIO u (cback e)) }
            }
          patches
        NPStyle styleName styleBody -> do
          let node' = node
                { V.nodeBody = assertElement node $ \vel -> V.NBElement $ vel
                    { V.elementStyle =
                        HMS.insert styleName styleBody (V.elementStyle vel)
                    }
                }
          go node' patches
        NPProperty propName propBody -> do
          let node' = node
                { V.nodeBody = assertElement node $ \vel -> V.NBElement $ vel
                    { V.elementProperties =
                        HMS.insert propName propBody (V.elementProperties vel)
                    }
                }
          go node' patches
        NPEvent (SomeEventAction evName evListener)  -> do
          let node' = node
                { V.nodeBody = assertElement node $ \vel -> V.NBElement $ vel
                    { V.elementEvents = DList.snoc
                        (V.elementEvents vel)
                        (V.SomeEvent evName $ \e ev ->
                          liftIO (unliftIO u (evListener e ev)))
                    }
                }
          go node' patches
  go node00 patches00
  where
    {-# INLINE assertElement #-}
    assertElement ::
         V.Node el
      -> ((DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => V.Element el -> a)
      -> a
    assertElement node f = case V.nodeBody node of
      V.NBElement e -> f e
      V.NBText{} -> error "got patch requiring an element body, but was NBText"
      V.NBRawNode{} -> error "got patch requiring an element body, but was NBRawNode"

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
  -> Node el read write
el tag wrap patches isChildren = do
  children <- elementChildren isChildren
  patchNode
    V.Node
      { V.nodeMark = Nothing
      , V.nodeBody = V.NBElement V.Element
          { V.elementTag = tag
          , V.elementProperties = mempty
          , V.elementStyle = mempty
          , V.elementEvents = mempty
          , V.elementChildren = children
          }
      , V.nodeCallbacks = mempty
      , V.nodeWrap = wrap
      }
    patches

-- Elements
-- --------------------------------------------------------------------

{-# INLINE div_ #-}
div_ :: IsElementChildren a read write => [NodePatch DOM.HTMLDivElement write] -> a -> Node DOM.HTMLDivElement read write
div_ = el "div" DOM.HTMLDivElement

{-# INLINE span_ #-}
span_ :: IsElementChildren a read write => [NodePatch DOM.HTMLSpanElement write] -> a -> Node DOM.HTMLSpanElement read write
span_ = el "span" DOM.HTMLSpanElement

{-# INLINE a_ #-}
a_ :: IsElementChildren a read write => [NodePatch DOM.HTMLAnchorElement write] -> a -> Node DOM.HTMLAnchorElement read write
a_ = el "a" DOM.HTMLAnchorElement

{-# INLINE p_ #-}
p_ :: IsElementChildren a read write => [NodePatch DOM.HTMLParagraphElement write] -> a -> Node DOM.HTMLParagraphElement read write
p_ = el "p" DOM.HTMLParagraphElement

{-# INLINE input_ #-}
input_ :: IsElementChildren a read write => [NodePatch DOM.HTMLInputElement write] -> a -> Node DOM.HTMLInputElement read write
input_ = el "input" DOM.HTMLInputElement

{-# INLINE form_ #-}
form_ :: IsElementChildren a read write => [NodePatch DOM.HTMLFormElement write] -> a -> Node DOM.HTMLFormElement read write
form_ = el "form" DOM.HTMLFormElement

{-# INLINE button_ #-}
button_ :: IsElementChildren a read write => [NodePatch DOM.HTMLButtonElement write] -> a -> Node DOM.HTMLButtonElement read write
button_ = el "button" DOM.HTMLButtonElement

{-# INLINE ul_ #-}
ul_ :: IsElementChildren a read write => [NodePatch DOM.HTMLUListElement write] -> a -> Node DOM.HTMLUListElement read write
ul_ = el "ul" DOM.HTMLUListElement

{-# INLINE li_ #-}
li_ :: IsElementChildren a read write => [NodePatch DOM.HTMLLIElement write] -> a -> Node DOM.HTMLLIElement read write
li_ = el "li" DOM.HTMLLIElement

{-# INLINE h2_ #-}
h2_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node DOM.HTMLHeadingElement read write
h2_ = el "h2" DOM.HTMLHeadingElement

{-# INLINE h5_ #-}
h5_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node DOM.HTMLHeadingElement read write
h5_ = el "h5" DOM.HTMLHeadingElement

{-# INLINE select_ #-}
select_ :: IsElementChildren a read write => [NodePatch DOM.HTMLSelectElement write] -> a -> Node DOM.HTMLSelectElement read write
select_ = el "select" DOM.HTMLSelectElement

{-# INLINE option_ #-}
option_ :: IsElementChildren a read write => [NodePatch DOM.HTMLOptionElement write] -> a -> Node DOM.HTMLOptionElement read write
option_ = el "option" DOM.HTMLOptionElement

{-# INLINE label_ #-}
label_ :: IsElementChildren a read write => [NodePatch DOM.HTMLLabelElement write] -> a -> Node DOM.HTMLLabelElement read write
label_ = el "label" DOM.HTMLLabelElement

{-# INLINE nav_ #-}
nav_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node DOM.HTMLElement read write
nav_ = el "nav" DOM.HTMLElement

{-# INLINE h1_ #-}
h1_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node DOM.HTMLHeadingElement read write
h1_ = el "h1" DOM.HTMLHeadingElement

{-# INLINE h4_ #-}
h4_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node DOM.HTMLHeadingElement read write
h4_ = el "h4" DOM.HTMLHeadingElement

{-# INLINE h6_ #-}
h6_ :: IsElementChildren a read write => [NodePatch DOM.HTMLHeadingElement write] -> a -> Node DOM.HTMLHeadingElement read write
h6_ = el "h6" DOM.HTMLHeadingElement

{-# INLINE small_ #-}
small_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node DOM.HTMLElement read write
small_ = el "small" DOM.HTMLElement

{-# INLINE pre_ #-}
pre_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node DOM.HTMLElement read write
pre_ = el "pre" DOM.HTMLElement

{-# INLINE code_ #-}
code_ :: IsElementChildren a read write => [NodePatch DOM.HTMLElement write] -> a -> Node DOM.HTMLElement read write
code_ = el "code" DOM.HTMLElement

{-# INLINE iframe_ #-}
iframe_ :: IsElementChildren a read write => [NodePatch DOM.HTMLIFrameElement write] -> a -> Node DOM.HTMLIFrameElement read write
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
     (DOM.IsEventTarget t, DOM.IsEvent e, MonadAction' write m, MonadUnliftIO m)
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
  => (el -> DOM.MouseEvent -> Action' write ()) -> NodePatch el write
onclick_ = NPEvent . SomeEventAction DOM.click

onchange_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action' write ()) -> NodePatch el write
onchange_ = NPEvent . SomeEventAction DOM.change

oninput_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action' write ()) -> NodePatch el write
oninput_ = NPEvent . SomeEventAction DOM.input

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action' write ()) -> NodePatch el write
onsubmit_ = NPEvent . SomeEventAction DOM.submit

onselect_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.UIEvent -> Action' write ()) -> NodePatch el write
onselect_ = NPEvent . SomeEventAction DOM.select

-- simple rendering
-- --------------------------------------------------------------------

-- when we want a quick render of a component, e.g. inside a raw node.
-- any dispatch will be dropped, e.g. this will never redraw anything,
-- it's just to place some elements in the provided element
simpleRenderComponent ::
     (DOM.IsElement el)
  => el -> read -> Component read () -> DOM.JSM ()
simpleRenderComponent container st comp = do
  doc <- DOM.currentDocumentUnchecked
  vdom <- runComponent comp id throwIO (\_ -> return ()) Nothing st
  void (renderVirtualDom RenderOptions{roAlwaysRerender = True, roErase = False, roSkipNode = \_ -> return False} doc container Nothing vdom)

