{-# LANGUAGE OverloadedStrings #-}
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

    -- * ComponentM
  , DispatchM
  , Dispatch
  , ComponentM
  , Component
  , Component'
  , KeyedComponent
  , KeyedComponent'
  , Node
  , Node'
  , runComponentM
  , runComponent
  , unsafeLiftClientM
  , askDispatch
  , askDispatchM
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
  , rawProperty_

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
import Control.Lens (Lens', view)
import qualified Control.Lens as Lens
import Control.Monad (ap, void)
import Data.Monoid ((<>), Endo)
import qualified Data.DList as DList
import Data.String (IsString(..))
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import GHC.Stack (HasCallStack)
import Data.JSString (JSString)
import qualified Data.JSString as JSS
import GHCJS.Types (JSVal)
import Data.Coerce

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM.Input
import qualified GHCJS.DOM.HTMLButtonElement as DOM.Button
import qualified GHCJS.DOM.HTMLOptionElement as DOM.Option
import qualified GHCJS.DOM.HTMLHyperlinkElementUtils as DOM.HyperlinkElementUtils
import qualified GHCJS.DOM as DOM

import Anapo.ClientM
import qualified Anapo.VDOM as V
import Anapo.Render

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

-- Monad
-- --------------------------------------------------------------------

type DispatchM state = (state -> ClientM state) -> ClientM ()
type Dispatch state = (state -> state) -> ClientM ()

newtype ComponentM dom read write a = ComponentM
  { unComponentM :: forall out.
         ((write -> ClientM write) -> out -> ClientM out)
      -- ^ how to modify the outside state given a modification to the
      -- inside state
      -> DispatchM out
      -- ^ how to dispatch updates to the state
      -> Maybe write
      -- ^ the previous state
      -> read
      -- ^ the current state
      -> ClientM (dom, a)
  }

type ComponentM' dom state = ComponentM dom state state
type Component' state = ComponentM V.Dom state state ()
type Component read write = ComponentM V.Dom read write ()
type KeyedComponent' state = ComponentM V.KeyedDom state state ()
type KeyedComponent read write = ComponentM V.KeyedDom read write ()
type Node el read write = ComponentM () read write (V.Node el)
type Node' el state = ComponentM () state state (V.Node el)

instance (el ~ DOM.Text) => IsString (Node el read write) where
  fromString = text . JSS.pack

{-# INLINE runComponentM #-}
runComponentM :: ComponentM dom read write a -> DispatchM write -> Maybe write -> read -> ClientM (dom, a)
runComponentM vdom = unComponentM vdom id

{-# INLINE runComponent #-}
runComponent :: Component read write -> DispatchM write -> Maybe write -> read -> ClientM V.Dom
runComponent vdom d mbst st = fst <$> unComponentM vdom id d mbst st

instance Functor (ComponentM dom read write) where
  {-# INLINE fmap #-}
  fmap f (ComponentM g) = ComponentM $ \write d mbst st -> do
    (dom, x) <- g write d mbst st
    return (dom, f x)

instance (Monoid dom) => Applicative (ComponentM dom read write) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Monoid dom) => Monad (ComponentM dom read write) where
  {-# INLINE return #-}
  return x = ComponentM (\_write _d _mbst _st -> return (mempty, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = ComponentM $ \write d mbst st -> do
    (vdom1, x) <- unComponentM ma write d mbst st
    (vdom2, y) <- unComponentM (mf x) write d mbst st
    let !vdom = vdom1 <> vdom2
    return (vdom, y)

{-# INLINE unsafeLiftClientM #-}
unsafeLiftClientM :: Monoid dom => ClientM a -> ComponentM dom read write a
unsafeLiftClientM m = ComponentM $ \_write _d _mbst _st -> do
  x <- m
  return (mempty, x)

{-# INLINE askDispatchM #-}
askDispatchM :: (Monoid dom) => ComponentM dom read write (DispatchM write)
askDispatchM = ComponentM $ \write d _mbst _st -> return
  ( mempty
  , \modifySt -> d (write modifySt)
  )

{-# INLINE askDispatch #-}
askDispatch :: (Monoid dom) => ComponentM dom read write (Dispatch write)
askDispatch = ComponentM $ \write d _mbst _st -> return
  ( mempty
  , \modifySt -> d (write (return . modifySt))
  )

{-# INLINE askState #-}
askState :: (Monoid dom) => ComponentM dom read write read
askState = ComponentM (\_write _d _mbst st -> return (mempty, st))

{-# INLINE askPreviousState #-}
askPreviousState ::
     (Monoid dom)
  => ComponentM dom read write (Maybe write)
askPreviousState = ComponentM (\_write _d mbst _st -> return (mempty, mbst))

{-# INLINE zoom #-}
zoom ::
     readIn
  -> (writeOut -> Maybe writeIn)
  -> ((writeIn -> ClientM writeIn) -> writeOut -> ClientM writeOut)
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoom st' get write' dom = ComponentM $ \write d mbst _st ->
  unComponentM dom (write . write') d (mbst >>= get) st'

{-# INLINE zoomL #-}
zoomL :: Lens' out in_ -> ComponentM' dom in_ a -> ComponentM' dom out a
zoomL l dom = ComponentM $ \write d mbst st ->
  unComponentM dom (write . l) d (view l <$> mbst) (view l st)

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

data NamedElementProperty el = NamedElementProperty JSString (V.ElementProperty el)

class (DOM.IsElement el) => ConstructElement el a | a -> el where
  constructElement :: (DOM.JSVal -> el) -> V.ElementTag -> [NamedElementProperty el] -> [V.SomeEvent el] -> a

{-# INLINE constructElement_ #-}
constructElement_ ::
     (DOM.IsElement el)
  => (DOM.JSVal -> el) -> V.ElementTag -> [NamedElementProperty el] -> [V.SomeEvent el] -> V.Children -> V.Node el
constructElement_ wrap tag props evts child = V.Node
  { V.nodeMark = Nothing
  , V.nodeCallbacks = mempty
  , V.nodeBody = V.NBElement V.Element
      { V.elementTag = tag
      , V.elementProperties = HMS.fromList $ do
          NamedElementProperty name prop <- reverse props
          return (name, prop)
      , V.elementEvents = reverse evts
      , V.elementChildren = child
      }
  , V.nodeWrap = wrap
  }

instance (DOM.IsElement el) => ConstructElement el (Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs evts =
    return (constructElement_ wrap tag attrs evts (V.CNormal mempty))

instance (DOM.IsElement el, read1 ~ read2, write1 ~ write2) => ConstructElement el (Component read1 write1 -> Node el read2 write2) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs evts dom = ComponentM $ \l d mbst st -> do
    (vdom, _) <- unComponentM dom l d mbst st
    return ((), constructElement_ wrap tag attrs evts (V.CNormal vdom))

instance (DOM.IsElement el, read1 ~ read2, write1 ~ write2) => ConstructElement el (KeyedComponent read1 write1 -> Node el read2 write2) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs evts dom = ComponentM $ \l d mbst st -> do
    (vdom, _) <- unComponentM dom l d mbst st
    return ((), constructElement_ wrap tag attrs evts (V.CKeyed vdom))

newtype UnsafeRawHtml = UnsafeRawHtml JSString

instance (DOM.IsElement el) => ConstructElement el (UnsafeRawHtml -> Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs evts (UnsafeRawHtml html) =
    return (constructElement_ wrap tag  attrs evts  (V.CRawHtml html))

instance (ConstructElement el a) => ConstructElement el (NamedElementProperty el -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs evts  attr =
    constructElement f tag (attr : attrs) evts

instance (ConstructElement el a) => ConstructElement el (V.SomeEvent el -> a) where
  {-# INLINE constructElement #-}
  constructElement f tag attrs evts evt = constructElement f tag attrs (evt : evts)

{-# INLINE el #-}
el :: (ConstructElement el a) => V.ElementTag -> (DOM.JSVal -> el) -> a
el tag f = constructElement f tag [] []

-- to manipulate nodes
-- --------------------------------------------------------------------

{-# INLINE unsafeWillMount #-}
unsafeWillMount :: (el -> ClientM ()) -> V.Node el -> V.Node el
unsafeWillMount f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeWillMount = f }
  }

{-# INLINE unsafeDidMount #-}
unsafeDidMount :: (el -> ClientM ()) -> V.Node el -> V.Node el
unsafeDidMount f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeDidMount = f }
  }

{-# INLINE unsafeWillPatch #-}
unsafeWillPatch :: (el -> ClientM ()) -> V.Node el -> V.Node el
unsafeWillPatch f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeWillPatch = f }
  }

{-# INLINE unsafeDidPatch #-}
unsafeDidPatch :: (el -> ClientM ()) -> V.Node el -> V.Node el
unsafeDidPatch f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeDidPatch = f }
  }

{-# INLINE unsafeWillRemove #-}
unsafeWillRemove :: (el -> ClientM ()) -> V.Node el -> V.Node el
unsafeWillRemove f nod = nod
  { V.nodeCallbacks = mappend (V.nodeCallbacks nod) $ mempty
      { V.callbacksUnsafeWillRemove = f }
  }

{-# INLINE marked #-}
marked ::
     (Maybe write -> read -> V.Rerender)
  -> StaticPtr (Node el read write) -> Node el read write
marked shouldRerender ptr = ComponentM $ \l d mbst st -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender mbst st
  (_, nod) <- unComponentM (deRefStaticPtr ptr) l d mbst st
  return ((), nod{ V.nodeMark = Just (V.Mark fprint rer) })

-- useful shorthands
-- --------------------------------------------------------------------

{-# INLINE n #-}
n :: (DOM.IsNode el) => Node el read write -> Component read write
n getNode = ComponentM $ \l d mbst st -> do
  (_, nod) <- unComponentM getNode l d mbst st
  return (DList.singleton (V.SomeNode nod), ())

{-# INLINE key #-}
key :: (DOM.IsNode el) => JSString -> Node el read write -> KeyedComponent read write
key k getNode = ComponentM $ \l d mbst st -> do
  (_, nod) <- unComponentM getNode l d mbst st
  return (V.KeyedDom (HMS.singleton k (V.SomeNode nod)) (DList.singleton k), ())

{-# INLINE text #-}
text :: JSString -> Node DOM.Text read write
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

class_ :: (DOM.IsElement el) => JSString -> NamedElementProperty el
class_ txt = NamedElementProperty "class" $ V.ElementProperty
  { V.eaGetProperty = DOM.getClassName
  , V.eaSetProperty = DOM.setClassName
  , V.eaValue = txt
  }

id_ :: (DOM.IsElement el) => JSString -> NamedElementProperty el
id_ txt = NamedElementProperty "id" $ V.ElementProperty
  { V.eaGetProperty = DOM.getId
  , V.eaSetProperty = DOM.setId
  , V.eaValue = txt
  }

class HasTypeProperty el where
  htpGetType :: el -> ClientM JSString
  htpSetType :: el -> JSString -> ClientM ()

instance HasTypeProperty DOM.HTMLInputElement where
  htpGetType = DOM.Input.getType
  htpSetType = DOM.Input.setType

instance HasTypeProperty DOM.HTMLButtonElement where
  htpGetType = DOM.Button.getType
  htpSetType = DOM.Button.setType

type_ :: (HasTypeProperty el) => JSString -> NamedElementProperty el
type_ txt = NamedElementProperty "type" $ V.ElementProperty
  { V.eaGetProperty = htpGetType
  , V.eaSetProperty = htpSetType
  , V.eaValue = txt
  }

class HasHrefProperty el where
  htpGetHref :: el -> ClientM JSString
  htpSetHref :: el -> JSString -> ClientM ()

instance HasHrefProperty DOM.HTMLAnchorElement where
  htpGetHref = DOM.HyperlinkElementUtils.getHref
  htpSetHref = DOM.HyperlinkElementUtils.setHref

href_ :: (HasHrefProperty el) => JSString -> NamedElementProperty el
href_ txt = NamedElementProperty "href" $ V.ElementProperty
  { V.eaGetProperty = htpGetHref
  , V.eaSetProperty = htpSetHref
  , V.eaValue = txt
  }

class HasValueProperty el where
  hvpGetValue :: el -> ClientM JSString
  hvpSetValue :: el -> JSString -> ClientM ()

instance HasValueProperty DOM.HTMLInputElement where
  hvpGetValue = DOM.Input.getValue
  hvpSetValue = DOM.Input.setValue

instance HasValueProperty DOM.HTMLOptionElement where
  hvpGetValue = DOM.Option.getValue
  hvpSetValue = DOM.Option.setValue

value_ :: (HasValueProperty el) => JSString -> NamedElementProperty el
value_ txt = NamedElementProperty "value" $ V.ElementProperty
  { V.eaGetProperty = hvpGetValue
  , V.eaSetProperty = hvpSetValue
  , V.eaValue = txt
  }

class HasCheckedProperty el where
  hcpGetChecked :: el -> ClientM Bool
  hcpSetChecked :: el -> Bool -> ClientM ()

instance HasCheckedProperty DOM.HTMLInputElement where
  hcpGetChecked = DOM.Input.getChecked
  hcpSetChecked = DOM.Input.setChecked

checked_ :: (HasCheckedProperty el) => Bool -> NamedElementProperty el
checked_ b = NamedElementProperty "checked" $ V.ElementProperty
  { V.eaGetProperty = hcpGetChecked
  , V.eaSetProperty = hcpSetChecked
  , V.eaValue = b
  }

selected_ :: Bool -> NamedElementProperty DOM.HTMLOptionElement
selected_ b = NamedElementProperty "selected" $ V.ElementProperty
  { V.eaGetProperty = DOM.Option.getSelected
  , V.eaSetProperty = DOM.Option.setSelected
  , V.eaValue = b
  }

class HasDisabledProperty el where
  hdpGetDisabled :: el -> ClientM Bool
  hdpSetDisabled :: el -> Bool -> ClientM ()

instance HasDisabledProperty DOM.HTMLButtonElement where
  hdpGetDisabled = DOM.Button.getDisabled
  hdpSetDisabled = DOM.Button.setDisabled

disabled_ :: HasDisabledProperty el => Bool -> NamedElementProperty el
disabled_ b = NamedElementProperty "disabled" $ V.ElementProperty
  { V.eaGetProperty = hdpGetDisabled
  , V.eaSetProperty = hdpSetDisabled
  , V.eaValue = b
  }

foreign import javascript unsafe
  "$2[$1]"
  js_getProperty :: JSString -> JSVal -> IO JSVal

foreign import javascript unsafe
  "$2[$1] = $3"
  js_setProperty :: JSString -> JSVal -> JSVal -> IO ()

rawProperty_ :: (DOM.ToJSVal el, Coercible a JSVal) => JSString -> a -> NamedElementProperty el
rawProperty_ k x = NamedElementProperty k $ V.ElementProperty
  { V.eaGetProperty = \el_ -> js_getProperty k =<< DOM.toJSVal el_
  , V.eaSetProperty = \el_ y -> do
      el' <- DOM.toJSVal el_
      js_setProperty (coerce k) el' y
  , V.eaValue = coerce x
  }

-- Events
-- --------------------------------------------------------------------

onclick_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.MouseEvent -> ClientM ()) -> V.SomeEvent el
onclick_ = V.SomeEvent DOM.click

onchange_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> ClientM ()) -> V.SomeEvent el
onchange_ = V.SomeEvent DOM.change

oninput_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> ClientM ()) -> V.SomeEvent el
oninput_ = V.SomeEvent DOM.input

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> ClientM ()) -> V.SomeEvent el
onsubmit_ = V.SomeEvent DOM.submit

onselect_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.UIEvent -> ClientM ()) -> V.SomeEvent el
onselect_ = V.SomeEvent DOM.select

-- simple rendering
-- --------------------------------------------------------------------

-- when we want a quick render of a component, e.g. inside a raw node.
-- any dispatch will be dropped, e.g. this will never redraw anything,
-- it's just to place some elements in the provided element
simpleRenderComponent ::
     (DOM.IsElement el)
  => el -> read -> Component read () -> ClientM ()
simpleRenderComponent container st comp = do
  doc <- DOM.currentDocumentUnchecked
  vdom <- runComponent comp (\_ -> return ()) Nothing st
  void (renderVirtualDom RenderOptions{roAlwaysRerender = True, roDebugOutput = False} doc container Nothing vdom)
