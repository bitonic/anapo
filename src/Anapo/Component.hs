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
  , put
  , get

    -- * ComponentM
  , ComponentM
  , Component
  , Component'
  , KeyedComponent
  , KeyedComponent'
  , Node
  , Node'
  , runComponentM
  , runComponent
  , askDispatch
  , askState
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
  , h2_
  , h5_
  , select_
  , option_
  , button_
  , ul_
  , li_
  , label_

    -- * attributes
  , NamedElementProperty(..)
  , StyleProperty(..)
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
  , rawProperty

    -- * events
  , onclick_
  , onchange_
  , onsubmit_
  , oninput_
  , onselect_

    -- * dom re-exports
  , DOM.preventDefault
  , DOM.HTMLAnchorElement
  , DOM.HTMLButtonElement

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
import Control.Monad.Trans.State (execStateT, execState, StateT, State, put, get)
import Control.Monad.IO.Class (MonadIO(..))

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM.Input
import qualified GHCJS.DOM.HTMLButtonElement as DOM.Button
import qualified GHCJS.DOM.HTMLOptionElement as DOM.Option
import qualified GHCJS.DOM.HTMLHyperlinkElementUtils as DOM.HyperlinkElementUtils
import qualified GHCJS.DOM as DOM

import qualified Anapo.VDOM as V
import Anapo.Render
import Anapo.Text (Text)
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

-- Dispatching
-- --------------------------------------------------------------------

type Dispatch state = (state -> DOM.JSM state) -> IO ()

{-# INLINE runDispatchT #-}
runDispatchT :: MonadIO m => Dispatch state -> StateT state DOM.JSM () -> m ()
runDispatchT disp st = liftIO (disp (execStateT st))

{-# INLINE runDispatch #-}
runDispatch :: MonadIO m => Dispatch state -> State state () -> m ()
runDispatch disp st = liftIO (disp (return . execState st))

-- Monad
-- --------------------------------------------------------------------

newtype ComponentM dom read write a = ComponentM
  { unComponentM ::
         Dispatch write
      -- how to dispatch updates to the state
      -> Maybe write
      -- the previous state
      -> read
      -- the current state
      -> DOM.JSM (dom, a)
  }

instance (Monoid dom, a ~ ()) => Monoid (ComponentM dom read write a) where
  mempty = return ()
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
  liftIO m = ComponentM $ \_d _mbst _st -> do
    x <- liftIO m
    return (mempty, x)

#if !defined(ghcjs_HOST_OS)
instance Monoid dom => JSaddle.MonadJSM (ComponentM dom read write) where
  {-# INLINE liftJSM' #-}
  liftJSM' m = ComponentM $ \_d _mbst _st -> do
    x <- m
    return (mempty, x)
#endif

instance (el ~ DOM.Text) => IsString (Node el read write) where
  {-# INLINE fromString #-}
  fromString = text . T.pack

{-# INLINE runComponentM #-}
runComponentM :: ComponentM dom read write a -> Dispatch write -> Maybe write -> read -> DOM.JSM (dom, a)
runComponentM vdom = unComponentM vdom

{-# INLINE runComponent #-}
runComponent :: Component read write -> Dispatch write -> Maybe write -> read -> DOM.JSM V.Dom
runComponent vdom d mbst st = fst <$> unComponentM vdom d mbst st

instance Functor (ComponentM dom read write) where
  {-# INLINE fmap #-}
  fmap f (ComponentM g) = ComponentM $ \d mbst st -> do
    (dom, x) <- g d mbst st
    return (dom, f x)

instance (Monoid dom) => Applicative (ComponentM dom read write) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Monoid dom) => Monad (ComponentM dom read write) where
  {-# INLINE return #-}
  return x = ComponentM (\_d _mbst _st -> return (mempty, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = ComponentM $ \d mbst st -> do
    (vdom1, x) <- unComponentM ma d mbst st
    (vdom2, y) <- unComponentM (mf x) d mbst st
    let !vdom = vdom1 <> vdom2
    return (vdom, y)

{-# INLINE askDispatch #-}
askDispatch :: (Monoid dom) => ComponentM dom read write (Dispatch write)
askDispatch = ComponentM (\d _mbst _st -> return (mempty, d))

{-# INLINE askState #-}
askState :: (Monoid dom) => ComponentM dom read write read
askState = ComponentM (\_d _mbst st -> return (mempty, st))

{-# INLINE askPreviousState #-}
askPreviousState ::
     (Monoid dom)
  => ComponentM dom read write (Maybe write)
askPreviousState = ComponentM (\_d mbst _st -> return (mempty, mbst))

{-# INLINE zoom #-}
zoom ::
     readIn
  -> (writeOut -> Maybe writeIn)
  -> LensLike' DOM.JSM writeOut writeIn
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoom st' get write' dom = ComponentM $ \d mbst _st ->
  unComponentM dom (d . write') (mbst >>= get) st'

{-# INLINE zoomL #-}
zoomL :: Lens' out in_ -> ComponentM' dom in_ a -> ComponentM' dom out a
zoomL l dom = ComponentM $ \d mbst st ->
  unComponentM dom (d . l) (view l <$> mbst) (view l st)

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

data NamedElementProperty el = NamedElementProperty Text (V.ElementProperty el)
data StyleProperty el = StyleProperty Text Text

class (DOM.IsElement el) => ConstructElement el a | a -> el where
  constructElement :: (DOM.JSVal -> el) -> V.ElementTag -> [NamedElementProperty el] -> [StyleProperty el] -> [V.SomeEvent el] -> a

{-# INLINE constructElement_ #-}
constructElement_ ::
     (DOM.IsElement el, DOM.IsElementCSSInlineStyle el)
  => (DOM.JSVal -> el) -> V.ElementTag -> [NamedElementProperty el] -> [StyleProperty el] -> [V.SomeEvent el] -> V.Children -> V.Node el
constructElement_ wrap tag props style evts child = V.Node
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
      , V.elementEvents = reverse evts
      , V.elementChildren = child
      }
  , V.nodeWrap = wrap
  }

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => ConstructElement el (Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts =
    return (constructElement_ wrap tag attrs style evts (V.CNormal mempty))

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el, read1 ~ read2, write1 ~ write2) => ConstructElement el (Component read1 write1 -> Node el read2 write2) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts dom = ComponentM $ \d mbst st -> do
    (vdom, _) <- unComponentM dom d mbst st
    return ((), constructElement_ wrap tag attrs style evts (V.CNormal vdom))

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el, read1 ~ read2, write1 ~ write2) => ConstructElement el (KeyedComponent read1 write1 -> Node el read2 write2) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts dom = ComponentM $ \d mbst st -> do
    (vdom, _) <- unComponentM dom d mbst st
    return ((), constructElement_ wrap tag attrs style evts (V.CKeyed vdom))

newtype UnsafeRawHtml = UnsafeRawHtml Text

instance (DOM.IsElement el, DOM.IsElementCSSInlineStyle el) => ConstructElement el (UnsafeRawHtml -> Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs style evts (UnsafeRawHtml html) =
    return (constructElement_ wrap tag  attrs style evts  (V.CRawHtml html))

instance (ConstructElement el a) => ConstructElement el (NamedElementProperty el -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts attr =
    constructElement f tag (attr : attrs) style evts

instance (ConstructElement el a) => ConstructElement el (StyleProperty el -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts stylep =
    constructElement f tag attrs (stylep : style) evts

instance (ConstructElement el a) => ConstructElement el (V.SomeEvent el -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs style evts evt = constructElement f tag attrs style (evt : evts)

{-# INLINE el #-}
el :: (ConstructElement el a) => V.ElementTag -> (DOM.JSVal -> el) -> a
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
marked shouldRerender ptr = ComponentM $ \d mbst st -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender mbst st
  (_, nod) <- unComponentM (deRefStaticPtr ptr) d mbst st
  return ((), nod{ V.nodeMark = Just (V.Mark fprint rer) })

-- useful shorthands
-- --------------------------------------------------------------------

{-# INLINE n #-}
n :: (DOM.IsNode el) => Node el read write -> Component read write
n getNode = ComponentM $ \d mbst st -> do
  (_, nod) <- unComponentM getNode d mbst st
  return (DList.singleton (V.SomeNode nod), ())

{-# INLINE key #-}
key :: (DOM.IsNode el) => Text -> Node el read write -> KeyedComponent read write
key k getNode = ComponentM $ \d mbst st -> do
  (_, nod) <- unComponentM getNode d mbst st
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
div_ :: (ConstructElement DOM.HTMLDivElement a) => a
div_ = el "div" DOM.HTMLDivElement

{-# INLINE span_ #-}
span_ :: (ConstructElement DOM.HTMLSpanElement a) => a
span_ = el "span" DOM.HTMLSpanElement

{-# INLINE a_ #-}
a_ :: (ConstructElement DOM.HTMLAnchorElement a) => a
a_ = el "a" DOM.HTMLAnchorElement

{-# INLINE p_ #-}
p_ :: (ConstructElement DOM.HTMLParagraphElement a) => a
p_ = el "p" DOM.HTMLParagraphElement

{-# INLINE input_ #-}
input_ :: (ConstructElement DOM.HTMLInputElement a) => a
input_ = el "input" DOM.HTMLInputElement

{-# INLINE form_ #-}
form_ :: (ConstructElement DOM.HTMLFormElement a) => a
form_ = el "form" DOM.HTMLFormElement

{-# INLINE button_ #-}
button_ :: (ConstructElement DOM.HTMLButtonElement a) => a
button_ = el "button" DOM.HTMLButtonElement

{-# INLINE ul_ #-}
ul_ :: (ConstructElement DOM.HTMLUListElement a) => a
ul_ = el "ul" DOM.HTMLUListElement

{-# INLINE li_ #-}
li_ :: (ConstructElement DOM.HTMLLIElement a) => a
li_ = el "li" DOM.HTMLLIElement

{-# INLINE h2_ #-}
h2_ :: (ConstructElement DOM.HTMLHeadingElement a) => a
h2_ = el "h2" DOM.HTMLHeadingElement

{-# INLINE h5_ #-}
h5_ :: (ConstructElement DOM.HTMLHeadingElement a) => a
h5_ = el "h5" DOM.HTMLHeadingElement

{-# INLINE select_ #-}
select_ :: (ConstructElement DOM.HTMLSelectElement a) => a
select_ = el "select" DOM.HTMLSelectElement

{-# INLINE option_ #-}
option_ :: (ConstructElement DOM.HTMLOptionElement a) => a
option_ = el "option" DOM.HTMLOptionElement

{-# INLINE label_ #-}
label_ :: (ConstructElement DOM.HTMLLabelElement a) => a
label_ = el "label" DOM.HTMLLabelElement

-- Properties
-- --------------------------------------------------------------------

style_ :: (DOM.IsElementCSSInlineStyle el) => Text -> Text -> StyleProperty el
style_ k v = StyleProperty k v

class_ :: (DOM.IsElement el) => Text -> NamedElementProperty el
class_ txt = NamedElementProperty "class" $ V.ElementProperty
  { V.eaGetProperty = DOM.getClassName
  , V.eaSetProperty = DOM.setClassName
  , V.eaValue = return txt
  }

id_ :: (DOM.IsElement el) => Text -> NamedElementProperty el
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

type_ :: (HasTypeProperty el) => Text -> NamedElementProperty el
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

href_ :: (HasHrefProperty el) => Text -> NamedElementProperty el
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

value_ :: (HasValueProperty el) => Text -> NamedElementProperty el
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

checked_ :: (HasCheckedProperty el) => Bool -> NamedElementProperty el
checked_ b = NamedElementProperty "checked" $ V.ElementProperty
  { V.eaGetProperty = hcpGetChecked
  , V.eaSetProperty = hcpSetChecked
  , V.eaValue = return b
  }

selected_ :: Bool -> NamedElementProperty DOM.HTMLOptionElement
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

disabled_ :: HasDisabledProperty el => Bool -> NamedElementProperty el
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

rawProperty :: (DOM.PToJSVal el, DOM.ToJSVal a) => Text -> a -> NamedElementProperty el
rawProperty k x = NamedElementProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> js_getProperty k (DOM.pToJSVal el_)
  , V.eaSetProperty = \el_ y -> do
      js_setProperty k (DOM.pToJSVal el_) y
  , V.eaValue = DOM.toJSVal x
  }
#else
rawProperty :: (JSaddle.MakeObject el, DOM.ToJSVal a) => Text -> a -> NamedElementProperty el
rawProperty k x = NamedElementProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> el_ JSaddle.! k
  , V.eaSetProperty = \el_ y -> (el_ JSaddle.<# k) y
  , V.eaValue = DOM.toJSVal x
  }
#endif

-- Events
-- --------------------------------------------------------------------

onclick_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.MouseEvent -> DOM.JSM ()) -> V.SomeEvent el
onclick_ = V.SomeEvent DOM.click

onchange_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> DOM.JSM ()) -> V.SomeEvent el
onchange_ = V.SomeEvent DOM.change

oninput_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> DOM.JSM ()) -> V.SomeEvent el
oninput_ = V.SomeEvent DOM.input

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> DOM.JSM ()) -> V.SomeEvent el
onsubmit_ = V.SomeEvent DOM.submit

onselect_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.UIEvent -> DOM.JSM ()) -> V.SomeEvent el
onselect_ = V.SomeEvent DOM.select

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
  vdom <- runComponent comp (\_ -> return ()) Nothing st
  void (renderVirtualDom RenderOptions{roAlwaysRerender = True} doc container Nothing vdom)
