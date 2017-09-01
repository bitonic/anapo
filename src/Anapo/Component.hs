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
  , zoom'
  , zoom_

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
  , div_
  , span_
  , a_
  , p_
  , input_
  , form_
  , h2_
  , select_
  , option_
  , button_
  , ul_
  , li_

    -- * attributes
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

    -- * events
  , onclick_
  , onchange_
  , onsubmit_
  , oninput_
  , onselect_

    -- * simple rendering
  , simpleRenderComponent
  ) where

import qualified Data.HashMap.Strict as HMS
import Control.Lens (Lens', view, _Just)
import qualified Control.Lens as Lens
import Control.Monad (ap, void)
import qualified Data.Text as T
import Data.Monoid ((<>), Endo)
import qualified Data.DList as DList
import Data.String (IsString(..))
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
import GHC.Stack (HasCallStack)

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM.Input
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
  { unComponentM :: forall out. AffineTraversal' out write -> DispatchM out -> Maybe out -> read -> ClientM (dom, a) }

type ComponentM' dom state = ComponentM dom state state
type Component' state = ComponentM V.Dom state state ()
type Component read write = ComponentM V.Dom read write ()
type KeyedComponent' state = ComponentM V.KeyedDom state state ()
type KeyedComponent read write = ComponentM V.KeyedDom read write ()
type Node el read write = ComponentM () read write (V.Node el)
type Node' el state = ComponentM () state state (V.Node el)

instance (el ~ DOM.Text) => IsString (Node el read write) where
  fromString = text . T.pack

{-# INLINE runComponentM #-}
runComponentM :: ComponentM dom read write a -> DispatchM write -> Maybe write -> read -> ClientM (dom, a)
runComponentM vdom = unComponentM vdom id

{-# INLINE runComponent #-}
runComponent :: Component read write -> DispatchM write -> Maybe write -> read -> ClientM V.Dom
runComponent vdom d mbst st = fst <$> unComponentM vdom id d mbst st

instance Functor (ComponentM dom read write) where
  {-# INLINE fmap #-}
  fmap f (ComponentM g) = ComponentM $ \l d mbst st -> do
    (dom, x) <- g l d mbst st
    return (dom, f x)

instance (Monoid dom) => Applicative (ComponentM dom read write) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Monoid dom) => Monad (ComponentM dom read write) where
  {-# INLINE return #-}
  return x = ComponentM (\_l _d _mbst _st -> return (mempty, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = ComponentM $ \l d mbst st -> do
    !(!vdom1, x) <- unComponentM ma l d mbst st
    !(!vdom2, y) <- unComponentM (mf x) l d mbst st
    let !vdom = vdom1 <> vdom2
    return (vdom, y)

{-# INLINE unsafeLiftClientM #-}
unsafeLiftClientM :: Monoid dom => ClientM a -> ComponentM dom read write a
unsafeLiftClientM m = ComponentM $ \_l _d _mbst _st -> do
  x <- m
  return (mempty, x)

{-# INLINE askDispatchM #-}
askDispatchM :: (Monoid dom) => ComponentM dom read write (DispatchM write)
askDispatchM = ComponentM $ \lst d _mbst _st -> return
  ( mempty
  , \modifySt -> d (lst modifySt)
  )

{-# INLINE askDispatch #-}
askDispatch :: (Monoid dom) => ComponentM dom read write (Dispatch write)
askDispatch = ComponentM $ \lst d _mbst _st -> return
  ( mempty
  , \modifySt -> d (lst (return . modifySt))
  )

{-# INLINE askState #-}
askState :: (Monoid dom) => ComponentM dom read write read
askState = ComponentM (\_lst _d _mbst st -> return (mempty, st))

{-# INLINE askPreviousState #-}
askPreviousState ::
     (Monoid dom)
  => (forall out. AffineTraversal' out write -> out -> ComponentM dom read write a)
  -> ComponentM dom read write a
askPreviousState cont = ComponentM $ \lst d mbst st ->
  unComponentM (cont (_Just . lst) mbst) lst d mbst st

{-# INLINE zoom' #-}
zoom' :: Lens' out in_ -> ComponentM' dom in_ a -> ComponentM' dom out a
zoom' l' dom = ComponentM $ \l d mbst st ->
  unComponentM dom (l . l') d mbst (view l' st)

{-# INLINE zoom #-}
zoom ::
     (readOut -> readIn)
  -> AffineTraversal' writeOut writeIn
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoom f l' dom = ComponentM $ \l d mbst st ->
  unComponentM dom (l . l') d mbst (f st)

{-# INLINE zoom_ #-}
zoom_ ::
     readIn
  -> AffineTraversal' writeOut writeIn
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoom_ x l' dom = ComponentM $ \l d mbst _st ->
  unComponentM dom (l . l') d mbst x

-- Type class machinery
-- --------------------------------------------------------------------

data NamedElementProperty el = NamedElementProperty T.Text (V.ElementProperty el)

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
    !(!vdom, _) <- unComponentM dom l d mbst st
    return ((), constructElement_ wrap tag attrs evts (V.CNormal vdom))

instance (DOM.IsElement el, read1 ~ read2, write1 ~ write2) => ConstructElement el (KeyedComponent read1 write1 -> Node el read2 write2) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs evts dom = ComponentM $ \l d mbst st -> do
    !(!vdom, _) <- unComponentM dom l d mbst st
    return ((), constructElement_ wrap tag attrs evts (V.CKeyed vdom))

newtype UnsafeRawHtml = UnsafeRawHtml T.Text

instance (DOM.IsElement el) => ConstructElement el (UnsafeRawHtml -> Node el read write) where
  {-# INLINE constructElement #-}
  constructElement wrap tag attrs evts  (UnsafeRawHtml html) =
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
     (forall out. AffineTraversal' out write -> out -> read -> V.Rerender)
  -> StaticPtr (Node el read write) -> Node el read write
marked shouldRerender ptr = ComponentM $ \l d mbst st -> do
  let !fprint = staticKey ptr
  let !rer = shouldRerender (_Just . l) mbst st
  !(_, !nod) <- unComponentM (deRefStaticPtr ptr) l d mbst st
  return ((), nod{ V.nodeMark = Just (V.Mark fprint rer) })

-- useful shorthands
-- --------------------------------------------------------------------

{-# INLINE n #-}
n :: (DOM.IsNode el) => Node el read write -> Component read write
n getNode = ComponentM $ \l d mbst st -> do
  !(_, !nod) <- unComponentM getNode l d mbst st
  return (DList.singleton (V.SomeNode nod), ())

{-# INLINE key #-}
key :: (DOM.IsNode el) => T.Text -> Node el read write -> KeyedComponent read write
key k getNode = ComponentM $ \l d mbst st -> do
  !(_, !nod) <- unComponentM getNode l d mbst st
  return (V.KeyedDom (HMS.singleton k (V.SomeNode nod)) (DList.singleton k), ())

{-# INLINE text #-}
text :: T.Text -> Node DOM.Text read write
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

{-# INLINE select_ #-}
select_ :: (ConstructElement DOM.HTMLSelectElement a) => a
select_ = el "select" DOM.HTMLSelectElement

{-# INLINE option_ #-}
option_ :: (ConstructElement DOM.HTMLOptionElement a) => a
option_ = el "option" DOM.HTMLOptionElement

-- Properties
-- --------------------------------------------------------------------

class_ :: (DOM.IsElement el) => T.Text -> NamedElementProperty el
class_ txt = NamedElementProperty "id" $ V.ElementProperty
  { V.eaGetProperty = DOM.getClassName
  , V.eaSetProperty = DOM.setClassName
  , V.eaValue = txt
  }

id_ :: (DOM.IsElement el) => T.Text -> NamedElementProperty el
id_ txt = NamedElementProperty "id" $ V.ElementProperty
  { V.eaGetProperty = DOM.getId
  , V.eaSetProperty = DOM.setId
  , V.eaValue = txt
  }

class HasTypeProperty el where
  htpGetType :: el -> ClientM T.Text
  htpSetType :: el -> T.Text -> ClientM ()

instance HasTypeProperty DOM.HTMLInputElement where
  htpGetType = DOM.Input.getType
  htpSetType = DOM.Input.setType

type_ :: (HasTypeProperty el) => T.Text -> NamedElementProperty el
type_ txt = NamedElementProperty "type" $ V.ElementProperty
  { V.eaGetProperty = htpGetType
  , V.eaSetProperty = htpSetType
  , V.eaValue = txt
  }

class HasHrefProperty el where
  htpGetHref :: el -> ClientM T.Text
  htpSetHref :: el -> T.Text -> ClientM ()

instance HasHrefProperty DOM.HTMLAnchorElement where
  htpGetHref = DOM.HyperlinkElementUtils.getHref
  htpSetHref = DOM.HyperlinkElementUtils.setHref

href_ :: (HasHrefProperty el) => T.Text -> NamedElementProperty el
href_ txt = NamedElementProperty "href" $ V.ElementProperty
  { V.eaGetProperty = htpGetHref
  , V.eaSetProperty = htpSetHref
  , V.eaValue = txt
  }

class HasValueProperty el where
  hvpGetValue :: el -> ClientM T.Text
  hvpSetValue :: el -> T.Text -> ClientM ()

instance HasValueProperty DOM.HTMLInputElement where
  hvpGetValue = DOM.Input.getValue
  hvpSetValue = DOM.Input.setValue

instance HasValueProperty DOM.HTMLOptionElement where
  hvpGetValue = DOM.Option.getValue
  hvpSetValue = DOM.Option.setValue

value_ :: (HasValueProperty el) => T.Text -> NamedElementProperty el
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
