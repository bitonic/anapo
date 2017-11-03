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

    -- * node manipulation
  , unsafeWillMount
  , unsafeDidMount
  , unsafeWillPatch
  , unsafeDidPatch
  , unsafeWillRemove
  , marked

    -- * type classes to construct elements
  , ConstructElement(..)
  -- , el
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
  , NamedElementProperty(..)
  , StyleProperty(..)
  , SomeEventAction(..)
  , style_
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
import Anapo.Orphans ()
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
newtype Action write a = Action
  { unAction ::
         RegisterThread
      -> HandleException
      -> Dispatch write
      -> DOM.JSM a
  }

{-# INLINE runAction #-}
runAction :: Action write a -> RegisterThread -> HandleException -> Dispatch write -> DOM.JSM a
runAction vdom = unAction vdom

instance Functor (Action write) where
  {-# INLINE fmap #-}
  fmap f (Action g) = Action $ \reg hdl d -> do
    x <- g reg hdl d
    return (f x)

instance Applicative (Action write) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad (Action write) where
  {-# INLINE return #-}
  return x = Action (\_reg _hdl _d -> return x)
  {-# INLINE (>>=) #-}
  ma >>= mf = Action $ \reg hdl d -> do
    x <- unAction ma reg hdl d
    unAction (mf x) reg hdl d

instance MonadIO (Action write) where
  {-# INLINE liftIO #-}
  liftIO m = Action (\_reg _hdl _d -> liftIO m)

#if !defined(ghcjs_HOST_OS)
instance  JSaddle.MonadJSM (Action write) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = Action (\_reg _hdl _d -> m)
#endif

instance MonadUnliftIO (Action write) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO = Action $ \reg hdl d -> do
    u <- askUnliftIO
    return (UnliftIO (\(Action m) -> unliftIO u (m reg hdl d)))

class (DOM.MonadJSM m) => MonadAction write m | m -> write where
  liftAction :: Action write a -> m a

instance MonadAction write (Action write) where
  {-# INLINE liftAction #-}
  liftAction = id

instance MonadAction write (StateT s (Action write)) where
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
forkAction :: MonadAction write m => Action write () -> m ThreadId
forkAction m = liftAction (Action (\reg hdl d -> forkRegistered reg hdl (unAction m reg hdl d)))

{-# INLINE dispatch #-}
dispatch :: MonadAction write m => StateT write (Action write) () -> m ()
dispatch m = liftAction (Action (\reg hdl d -> liftIO (d (\st -> unAction (execStateT m st) reg hdl d))))

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
runComponentM :: ComponentM dom read write a -> RegisterThread -> HandleException -> Dispatch write -> Maybe write -> read -> DOM.JSM (dom, a)
runComponentM vdom = unComponentM vdom

{-# INLINE runComponent #-}
runComponent :: Component read write -> RegisterThread -> HandleException -> Dispatch write -> Maybe write -> read -> DOM.JSM V.Dom
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

{-# INLINE askDispatch #-}
askDispatch :: (Monoid dom) => ComponentM dom read write (Dispatch write)
askDispatch = ComponentM (\_reg _hdl d _mbst _st -> return (mempty, d))

{-# INLINE localDispatch #-}
localDispatch :: (Monoid dom) => Dispatch write' -> Maybe write' -> ComponentM dom read write' a -> ComponentM dom read write a
localDispatch d mbst comp = ComponentM (\reg hdl _d _mbst st -> unComponentM comp reg hdl d mbst st)

{-# INLINE askRegisterThread #-}
askRegisterThread :: (Monoid dom) => ComponentM dom read write RegisterThread
askRegisterThread = ComponentM (\reg _hdl _d _mbst _st -> return (mempty, reg))

{-# INLINE localRegisterThread #-}
localRegisterThread :: (Monoid dom) => RegisterThread -> ComponentM dom read write a -> ComponentM dom read write a
localRegisterThread reg comp = ComponentM (\_reg hdl d mbst st -> unComponentM comp reg hdl d mbst st)

{-# INLINE askHandleException #-}
askHandleException :: (Monoid dom) => ComponentM dom read write HandleException
askHandleException = ComponentM (\_reg hdl _d _mbst _st -> return (mempty, hdl))

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

-- Type class machinery
-- --------------------------------------------------------------------

data NamedElementProperty el write = NamedElementProperty Text (V.ElementProperty el)
data StyleProperty el write = StyleProperty Text Text
data SomeEventAction el write = forall e. (DOM.IsEvent e) =>
  SomeEventAction (DOM.EventM.EventName el e) (el -> e -> Action write ())

class (DOM.IsElement el) => ConstructElement el write a | a -> el, a -> write where
  constructElement :: (DOM.JSVal -> el) -> V.ElementTag -> [NamedElementProperty el write] -> [StyleProperty el write] -> [SomeEventAction el write] -> a

{-# INLINE constructElement_ #-}
constructElement_ ::
     (DOM.IsElement el, DOM.IsElementCSSInlineStyle el)
  => (DOM.JSVal -> el) -> V.ElementTag -> [NamedElementProperty el write] -> [StyleProperty el write] -> [SomeEventAction el write] -> V.Children
  -> Node el read write
constructElement_ wrap tag props style evts child = do
  register <- askRegisterThread
  handle <- askHandleException
  disp <- askDispatch
  return V.Node
    { V.nodeMark = Nothing
    , V.nodeCallbacks = mempty
    , V.nodeBody = V.NBElement V.Element
        { V.elementTag = tag
        , V.elementProperties = HMS.fromList $ do
            NamedElementProperty name prop <- reverse props
            return (name, prop)
        , V.elementStyle = HMS.fromList $ do
            StyleProperty name val <- reverse style
            return (name, val)
        , V.elementEvents = do
            SomeEventAction evtName evtHandler <- reverse evts
            return $ V.SomeEvent evtName $ \el_ ev -> do
              u <- askUnliftIO
              liftIO $ uninterruptibleMask $ \restore -> register $ do
                mbExc <- tryAny (restore (unliftIO u (unAction (evtHandler el_ ev) register handle disp)))
                case mbExc of
                  Left err -> do
                    logError ("Caught exception in event, will handle it upstream: " <> pack (show err))
                    handle err
                  Right x -> return x
        , V.elementChildren = child
        }
    , V.nodeWrap = wrap
    }

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => ConstructElement el write (Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts =
    constructElement_ wrap tag attrs style evts (V.CNormal mempty)

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el, read1 ~ read2, write1 ~ write2) => ConstructElement el write2 (Component read1 write1 -> Node el read2 write2) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts dom = ComponentM $ \reg hdl d mbst st -> do
    (vdom, _) <- unComponentM dom reg hdl d mbst st
    (_, el_) <- unComponentM (constructElement_ wrap tag attrs style evts (V.CNormal vdom)) reg hdl d mbst st
    return ((), el_)

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el, read1 ~ read2, write1 ~ write2) => ConstructElement el write2 (KeyedComponent read1 write1 -> Node el read2 write2) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts dom = ComponentM $ \reg hdl d mbst st -> do
    (vdom, _) <- unComponentM dom reg hdl d mbst st
    (_, el_) <- unComponentM (constructElement_ wrap tag attrs style evts (V.CKeyed vdom)) reg hdl d mbst st
    return ((), el_)

newtype UnsafeRawHtml = UnsafeRawHtml Text

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => ConstructElement el write (UnsafeRawHtml -> Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts (UnsafeRawHtml html) =
    constructElement_ wrap tag  attrs style evts  (V.CRawHtml html)

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => ConstructElement el write (Maybe UnsafeRawHtml -> Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts = \case
    Nothing -> constructElement wrap tag attrs style evts
    Just html -> constructElement wrap tag attrs style evts html

instance (ConstructElement el write a) => ConstructElement el write (NamedElementProperty el write -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts attr =
    constructElement f tag (attr : attrs) style evts

instance (ConstructElement el write a) => ConstructElement el write (Maybe (NamedElementProperty el write) -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts = \case
    Nothing -> constructElement f tag attrs style evts
    Just attr -> constructElement f tag attrs style evts attr

instance (ConstructElement el write a) => ConstructElement el write (StyleProperty el write -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts stylep =
    constructElement f tag attrs (stylep : style) evts

instance (ConstructElement el write a) => ConstructElement el write (Maybe (StyleProperty el write) -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts = \case
    Nothing -> constructElement f tag attrs style evts
    Just stylep -> constructElement f tag attrs (stylep : style) evts

instance (ConstructElement el write a) => ConstructElement el write (SomeEventAction el write -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts evt = constructElement f tag attrs style (evt : evts)

instance (ConstructElement el write a) => ConstructElement el write (Maybe (SomeEventAction el write) -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts = \case
    Nothing -> constructElement f tag attrs style evts
    Just evt -> constructElement f tag attrs style evts evt

{-# INLINE el #-}
el :: (ConstructElement el write a) => V.ElementTag -> (DOM.JSVal -> el) -> a
el tag f = constructElement f tag [] [] []

-- to manipulate nodes
-- --------------------------------------------------------------------

{-# INLINE unsafeWillMount #-}
unsafeWillMount :: (el -> DOM.JSM ()) -> V.Node el -> V.Node el
unsafeWillMount f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeWillMount = f }
  }

{-# INLINE unsafeDidMount #-}
unsafeDidMount :: (el -> DOM.JSM ()) -> V.Node el -> V.Node el
unsafeDidMount f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeDidMount = f }
  }

{-# INLINE unsafeWillPatch #-}
unsafeWillPatch :: (el -> DOM.JSM ()) -> V.Node el -> V.Node el
unsafeWillPatch f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeWillPatch = f }
  }

{-# INLINE unsafeDidPatch #-}
unsafeDidPatch :: (el -> DOM.JSM ()) -> V.Node el -> V.Node el
unsafeDidPatch f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeDidPatch = f }
  }

{-# INLINE unsafeWillRemove #-}
unsafeWillRemove :: (el -> DOM.JSM ()) -> V.Node el -> V.Node el
unsafeWillRemove f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeWillRemove = f }
  }

{-# INLINE marked #-}
marked ::
     (Maybe write -> read -> V.Rerender)
  -> StaticPtr (Node el read write) -> Node el read write
marked shouldRerender ptr = ComponentM $ \reg hdl d mbst st -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender mbst st
  (_, nod) <- unComponentM (deRefStaticPtr ptr) reg hdl d mbst st
  return ((), nod{ V.nodeMark = Just (V.Mark fprint rer) })

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

-- Elements
-- --------------------------------------------------------------------

{-# INLINE div_ #-}
div_ :: (ConstructElement DOM.HTMLDivElement write a) => a
div_ = el "div" DOM.HTMLDivElement

{-# INLINE span_ #-}
span_ :: (ConstructElement DOM.HTMLSpanElement write a) => a
span_ = el "span" DOM.HTMLSpanElement

{-# INLINE a_ #-}
a_ :: (ConstructElement DOM.HTMLAnchorElement write a) => a
a_ = el "a" DOM.HTMLAnchorElement

{-# INLINE p_ #-}
p_ :: (ConstructElement DOM.HTMLParagraphElement write a) => a
p_ = el "p" DOM.HTMLParagraphElement

{-# INLINE input_ #-}
input_ :: (ConstructElement DOM.HTMLInputElement write a) => a
input_ = el "input" DOM.HTMLInputElement

{-# INLINE form_ #-}
form_ :: (ConstructElement DOM.HTMLFormElement write a) => a
form_ = el "form" DOM.HTMLFormElement

{-# INLINE button_ #-}
button_ :: (ConstructElement DOM.HTMLButtonElement write a) => a
button_ = el "button" DOM.HTMLButtonElement

{-# INLINE ul_ #-}
ul_ :: (ConstructElement DOM.HTMLUListElement write a) => a
ul_ = el "ul" DOM.HTMLUListElement

{-# INLINE li_ #-}
li_ :: (ConstructElement DOM.HTMLLIElement write a) => a
li_ = el "li" DOM.HTMLLIElement

{-# INLINE h2_ #-}
h2_ :: (ConstructElement DOM.HTMLHeadingElement write a) => a
h2_ = el "h2" DOM.HTMLHeadingElement

{-# INLINE h5_ #-}
h5_ :: (ConstructElement DOM.HTMLHeadingElement write a) => a
h5_ = el "h5" DOM.HTMLHeadingElement

{-# INLINE select_ #-}
select_ :: (ConstructElement DOM.HTMLSelectElement write a) => a
select_ = el "select" DOM.HTMLSelectElement

{-# INLINE option_ #-}
option_ :: (ConstructElement DOM.HTMLOptionElement write a) => a
option_ = el "option" DOM.HTMLOptionElement

{-# INLINE label_ #-}
label_ :: (ConstructElement DOM.HTMLLabelElement write a) => a
label_ = el "label" DOM.HTMLLabelElement

{-# INLINE nav_ #-}
nav_ :: (ConstructElement DOM.HTMLElement write a) => a
nav_ = el "nav" DOM.HTMLElement

{-# INLINE h1_ #-}
h1_ :: (ConstructElement DOM.HTMLHeadingElement write a) => a
h1_ = el "h1" DOM.HTMLHeadingElement

{-# INLINE h4_ #-}
h4_ :: (ConstructElement DOM.HTMLHeadingElement write a) => a
h4_ = el "h4" DOM.HTMLHeadingElement

{-# INLINE h6_ #-}
h6_ :: (ConstructElement DOM.HTMLHeadingElement write a) => a
h6_ = el "h6" DOM.HTMLHeadingElement

{-# INLINE small_ #-}
small_ :: (ConstructElement DOM.HTMLElement write a) => a
small_ = el "small" DOM.HTMLElement

{-# INLINE pre_ #-}
pre_ :: (ConstructElement DOM.HTMLElement write a) => a
pre_ = el "pre" DOM.HTMLElement

{-# INLINE code_ #-}
code_ :: (ConstructElement DOM.HTMLElement write a) => a
code_ = el "code" DOM.HTMLElement


-- Properties
-- --------------------------------------------------------------------

style_ :: (DOM.IsElementCSSInlineStyle el) => Text -> Text -> StyleProperty el write
style_ k v = StyleProperty k v

class_ :: (DOM.IsElement el) => Text -> NamedElementProperty el write
class_ txt = NamedElementProperty "class" $ V.ElementProperty
  { V.eaGetProperty = DOM.getClassName
  , V.eaSetProperty = DOM.setClassName
  , V.eaValue = return txt
  }

id_ :: (DOM.IsElement el) => Text -> NamedElementProperty el write
id_ txt = NamedElementProperty "id" $ V.ElementProperty
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

type_ :: (HasTypeProperty el) => Text -> NamedElementProperty el write
type_ txt = NamedElementProperty "type" $ V.ElementProperty
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

href_ :: (HasHrefProperty el) => Text -> NamedElementProperty el write
href_ txt = NamedElementProperty "href" $ V.ElementProperty
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

value_ :: (HasValueProperty el) => Text -> NamedElementProperty el write
value_ txt = NamedElementProperty "value" $ V.ElementProperty
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

checked_ :: (HasCheckedProperty el) => Bool -> NamedElementProperty el write
checked_ b = NamedElementProperty "checked" $ V.ElementProperty
  { V.eaGetProperty = hcpGetChecked
  , V.eaSetProperty = hcpSetChecked
  , V.eaValue = return b
  }

selected_ :: Bool -> NamedElementProperty DOM.HTMLOptionElement write
selected_ b = NamedElementProperty "selected" $ V.ElementProperty
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

disabled_ :: HasDisabledProperty el => Bool -> NamedElementProperty el write
disabled_ b = NamedElementProperty "disabled" $ V.ElementProperty
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

rawProperty :: (DOM.PToJSVal el, DOM.ToJSVal a) => Text -> a -> NamedElementProperty el write
rawProperty k x = NamedElementProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> js_getProperty k (DOM.pToJSVal el_)
  , V.eaSetProperty = \el_ y -> do
      js_setProperty k (DOM.pToJSVal el_) y
  , V.eaValue = DOM.toJSVal x
  }
#else
rawProperty :: (JSaddle.MakeObject el, DOM.ToJSVal a) => Text -> a -> NamedElementProperty el write
rawProperty k x = NamedElementProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> el_ JSaddle.! k
  , V.eaSetProperty = \el_ y -> (el_ JSaddle.<# k) y
  , V.eaValue = DOM.toJSVal x
  }
#endif

{-# INLINE rawAttribute #-}
rawAttribute :: (DOM.IsElement el) => Text -> Text -> NamedElementProperty el write
rawAttribute k x = NamedElementProperty k $ V.ElementProperty
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

placeholder_ :: (HasPlaceholderProperty el) => Text -> NamedElementProperty el write
placeholder_ txt = NamedElementProperty "placeholder" $ V.ElementProperty
  { V.eaGetProperty = getPlaceholder
  , V.eaSetProperty = setPlaceholder
  , V.eaValue = return txt
  }

{-# INLINE for_ #-}
for_ :: Text -> NamedElementProperty DOM.HTMLLabelElement write
for_ txt = NamedElementProperty "for" $ V.ElementProperty
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
multiple_ :: (HasMultipleProperty el) => Bool -> NamedElementProperty el write
multiple_ txt = NamedElementProperty "multiple" $ V.ElementProperty
  { V.eaGetProperty = getMultiple
  , V.eaSetProperty = setMultiple
  , V.eaValue = return txt
  }

{-# INLINE iframe_ #-}
iframe_ :: (ConstructElement DOM.HTMLIFrameElement write a) => a
iframe_ = el "iframe" DOM.HTMLIFrameElement

class HasSrcProperty el where
  getSrc :: el -> DOM.JSM Text
  setSrc :: el -> Text -> DOM.JSM ()

instance HasSrcProperty DOM.HTMLIFrameElement where
  getSrc = DOM.IFrame.getSrc
  setSrc = DOM.IFrame.setSrc

{-# INLINE src_ #-}
src_ :: (HasSrcProperty el) => Text -> NamedElementProperty el write
src_ txt = NamedElementProperty "src" $ V.ElementProperty
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
  => (el -> DOM.MouseEvent -> Action write ()) -> SomeEventAction el write
onclick_ = SomeEventAction DOM.click

onchange_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action write ()) -> SomeEventAction el write
onchange_ = SomeEventAction DOM.change

oninput_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action write ()) -> SomeEventAction el write
oninput_ = SomeEventAction DOM.input

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> Action write ()) -> SomeEventAction el write
onsubmit_ = SomeEventAction DOM.submit

onselect_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.UIEvent -> Action write ()) -> SomeEventAction el write
onselect_ = SomeEventAction DOM.select

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
