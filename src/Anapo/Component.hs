{-# LANGUAGE OverloadedStrings #-}
module Anapo.Component
  ( -- * Re-exports
    VirtualDom
  , VirtualDomNode
  , ClientM
  , runClientM
  , DispatchM
  , Dispatch

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
  , askDispatchM
  , askState
  , askPreviousState

    -- * basic combinators
  , n
  , key
  , text
  , marked
  , zoom
  , zoom_
  , zoom'
  , rawNode

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
  , inputType_
  , aHref_
  , inputValue_
  , optionValue_
  , inputChecked_
  , selected_

    -- * events
  , onclick_
  , onchange_
  , onsubmit_
  , oninput_

    -- * Type class machinery
  , ConstructVirtualDomElement
  ) where

import qualified Data.HashMap.Strict as HMS
import Control.Lens (Traversal', Lens', view)
import Control.Monad (ap)
import qualified Data.Text as T
import Data.Monoid ((<>))
import GHC.StaticPtr
import qualified Data.DList as DList
import Data.String (IsString(..))

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.HTMLInputElement as DOM.Input
import qualified GHCJS.DOM.HTMLOptionElement as DOM.Option
import qualified GHCJS.DOM.HTMLHyperlinkElementUtils as DOM.HyperlinkElementUtils

import Anapo.Core

-- Monad
-- --------------------------------------------------------------------

type DispatchM state = (state -> ClientM state) -> ClientM ()
type Dispatch state = (state -> state) -> ClientM ()

newtype ComponentM dom read write a = ComponentM
  { unComponentM :: forall out. Traversal' out write -> DispatchM out -> Maybe out -> read -> (dom, a) }

type ComponentM' dom state = ComponentM dom state state
type Component' state = ComponentM VirtualDom state state ()
type Component read write = ComponentM VirtualDom read write ()
type KeyedComponent' state = ComponentM KeyedVirtualDom state state ()
type KeyedComponent read write = ComponentM KeyedVirtualDom read write ()
type Node read write = ComponentM () read write VirtualDomNode
type Node' state = ComponentM () state state VirtualDomNode

instance IsString (Node read write) where
  fromString = text . T.pack

{-# INLINE runComponentM #-}
runComponentM :: ComponentM dom read write a -> DispatchM write -> Maybe write -> read -> (dom, a)
runComponentM vdom = unComponentM vdom id

{-# INLINE runComponent #-}
runComponent :: Component read write -> DispatchM write -> Maybe write -> read -> VirtualDom
runComponent vdom d mbst st = fst (unComponentM vdom id d mbst st)

instance Functor (ComponentM dom read write) where
  {-# INLINE fmap #-}
  fmap f (ComponentM g) = ComponentM $ \l d mbst st -> let
    (dom, x) = g l d mbst st
    in (dom, f x)

instance (Monoid dom) => Applicative (ComponentM dom read write) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Monoid dom) => Monad (ComponentM dom read write) where
  {-# INLINE return #-}
  return x = ComponentM (\_l _d _mbst _st -> (mempty, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = ComponentM $ \l d mbst st -> let
    (!vdom1, x) = unComponentM ma l d mbst st
    (!vdom2, y) = unComponentM (mf x) l d mbst st
    !vdom = vdom1 <> vdom2
    in (vdom, y)

{-# INLINE askDispatchM #-}
askDispatchM :: (Monoid dom) => ComponentM dom read write (DispatchM write)
askDispatchM = ComponentM $ \lst d _mbst _st ->
  ( mempty
  , \modifySt -> d (lst modifySt)
  )

{-# INLINE askDispatch #-}
askDispatch :: (Monoid dom) => ComponentM dom read write (Dispatch write)
askDispatch = ComponentM $ \lst d _mbst _st ->
  ( mempty
  , \modifySt -> d (lst (return . modifySt))
  )

{-# INLINE askState #-}
askState :: (Monoid dom) => ComponentM dom read write read
askState = ComponentM (\_lst _d _mbst st -> (mempty, st))

{-# INLINE askPreviousState #-}
askPreviousState ::
     (Monoid dom)
  => (forall out. Traversal' out write -> Maybe out -> ComponentM dom read write a)
  -> ComponentM dom read write a
askPreviousState cont = ComponentM $ \lst d mbst st ->
  unComponentM (cont lst mbst) lst d mbst st

{-# INLINE key #-}
key :: T.Text -> Node read write -> KeyedComponent read write
key k getNode = ComponentM $ \l d mbst st -> let
  !(_, !nod) = unComponentM getNode l d mbst st
  in (KeyedVirtualDom (HMS.singleton k nod) (DList.singleton k), ())

{-# INLINE n #-}
n :: Node read write -> Component read write
n getNode = ComponentM $ \l d mbst st -> let
  !(_, !nod) = unComponentM getNode l d mbst st
  in (DList.singleton nod, ())

{-# INLINE text #-}
text :: T.Text -> Node read write
text txt = return $ VirtualDomNode
  { vdnMark = Nothing
  , vdnWrap = DOM.Text
  , vdnBody = VDNBText txt
  , vdnCallbacks = noVirtualDomNodeCallbacks
  }

{-# INLINE marked #-}
marked ::
     (forall out. Traversal' out write -> Maybe out -> read -> Render)
  -> StaticPtr (Node read write)
  -> Node read write
marked rerender domPtr = ComponentM $ \l d mbst st -> let
  !render = rerender l mbst st
  !fprint = staticKey domPtr
  -- do not force evaluation for this one since we actually
  -- want it to be lazy to avoid computing the vdom if we don't
  -- have to
  (_, dom) = unComponentM (deRefStaticPtr domPtr) l d mbst st
  in ((), dom{ vdnMark = Just (VirtualDomNodeMark fprint render) })

{-# INLINE zoom' #-}
zoom' :: Lens' out in_ -> ComponentM' dom in_ a -> ComponentM' dom out a
zoom' l' dom = ComponentM $ \l d mbst st ->
  unComponentM dom (l . l') d mbst (view l' st)

{-# INLINE zoom #-}
zoom ::
     (readOut -> readIn)
  -> Traversal' writeOut writeIn
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoom f l' dom = ComponentM $ \l d mbst st ->
  unComponentM dom (l . l') d mbst (f st)

{-# INLINE zoom_ #-}
zoom_ ::
     readIn
  -> Traversal' writeOut writeIn
  -> ComponentM dom readIn writeIn a
  -> ComponentM dom readOut writeOut a
zoom_ x l' dom = ComponentM $ \l d mbst _st ->
  unComponentM dom (l . l') d mbst x

{-# INLINE rawNode #-}
rawNode :: (DOM.IsNode el) => (DOM.JSVal -> el) -> el -> Node read write
rawNode wrap x = return $ VirtualDomNode
  { vdnMark = Nothing
  , vdnBody = VDNBRawNode x
  , vdnWrap = wrap
  , vdnCallbacks = noVirtualDomNodeCallbacks
  }

class (DOM.IsElement el) => ConstructVirtualDomElement el a where
  constructVirtualDomElement :: ElementTag -> (DOM.JSVal -> el) -> [NamedElementProperty el] -> [SomeEvent el] -> a

{-# INLINE constructVirtualDomElement_ #-}
constructVirtualDomElement_ ::
     (DOM.IsElement el)
  => ElementTag -> (DOM.JSVal -> el) -> [NamedElementProperty el] -> [SomeEvent el] -> VirtualDomChildren -> VirtualDomNode
constructVirtualDomElement_ tag f props evts child = VirtualDomNode
  { vdnMark = Nothing
  , vdnWrap = f
  , vdnBody = VDNBElement $ VirtualDomElement
      { vdeTag = tag
      , vdeProperties = HMS.fromList $ do
          NamedElementProperty name prop <- reverse props
          return (name, prop)
      , vdeEvents = reverse evts
      , vdeChildren = child
      }
  , vdnCallbacks = noVirtualDomNodeCallbacks
  }

instance (DOM.IsElement el) => ConstructVirtualDomElement el (ComponentM () read write VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts =
    return (constructVirtualDomElement_ tag f attrs evts (VDCNormal mempty))

instance (DOM.IsElement el, read1 ~ read2, write1 ~ write2) => ConstructVirtualDomElement el (ComponentM VirtualDom read1 write1 () -> ComponentM () read2 write2 VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts dom = ComponentM $ \l d mbst st -> let
    !(!vdom, _) = unComponentM dom l d mbst st
    in ((), constructVirtualDomElement_ tag f attrs evts (VDCNormal vdom))

instance (DOM.IsElement el, read1 ~ read2, write1 ~ write2) => ConstructVirtualDomElement el (ComponentM KeyedVirtualDom read1 write1 () -> ComponentM () read2 write2 VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts dom = ComponentM $ \l d mbst st -> let
    !(!vdom, _) = unComponentM dom l d mbst st
    in ((), constructVirtualDomElement_ tag f attrs evts (VDCKeyed vdom))

newtype UnsafeRawHtml = UnsafeRawHtml T.Text

instance (DOM.IsElement el) => ConstructVirtualDomElement el (UnsafeRawHtml -> ComponentM () read write VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts (UnsafeRawHtml html) = return (constructVirtualDomElement_ tag f attrs evts (VDCRawHtml html))

data NamedElementProperty el = NamedElementProperty T.Text (ElementProperty el)

instance (ConstructVirtualDomElement el1 a, el1 ~ el2) => ConstructVirtualDomElement el1 (NamedElementProperty el2 -> a) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts attr =
    constructVirtualDomElement tag f (attr : attrs) evts

instance (ConstructVirtualDomElement el1 a, el1 ~ el2) => ConstructVirtualDomElement el1 (SomeEvent el2 -> a) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts evt = constructVirtualDomElement tag f attrs (evt : evts)

{-# INLINE el #-}
el :: (ConstructVirtualDomElement el a) => ElementTag -> (DOM.JSVal -> el) -> a
el tag f = constructVirtualDomElement tag f [] []

-- Elements
-- --------------------------------------------------------------------

{-# INLINE div_ #-}
div_ :: (ConstructVirtualDomElement DOM.HTMLDivElement a) => a
div_ = el "div" DOM.HTMLDivElement

{-# INLINE span_ #-}
span_ :: (ConstructVirtualDomElement DOM.HTMLSpanElement a) => a
span_ = el "span" DOM.HTMLSpanElement

{-# INLINE a_ #-}
a_ :: (ConstructVirtualDomElement DOM.HTMLAnchorElement a) => a
a_ = el "a" DOM.HTMLAnchorElement

{-# INLINE p_ #-}
p_ :: (ConstructVirtualDomElement DOM.HTMLParagraphElement a) => a
p_ = el "p" DOM.HTMLParagraphElement

{-# INLINE input_ #-}
input_ :: (ConstructVirtualDomElement DOM.HTMLInputElement a) => a
input_ = el "input" DOM.HTMLInputElement

{-# INLINE form_ #-}
form_ :: (ConstructVirtualDomElement DOM.HTMLFormElement a) => a
form_ = el "form" DOM.HTMLFormElement

{-# INLINE button_ #-}
button_ :: (ConstructVirtualDomElement DOM.HTMLButtonElement a) => a
button_ = el "button" DOM.HTMLButtonElement

{-# INLINE ul_ #-}
ul_ :: (ConstructVirtualDomElement DOM.HTMLUListElement a) => a
ul_ = el "ul" DOM.HTMLUListElement

{-# INLINE li_ #-}
li_ :: (ConstructVirtualDomElement DOM.HTMLLIElement a) => a
li_ = el "li" DOM.HTMLLIElement

{-# INLINE h2_ #-}
h2_ :: (ConstructVirtualDomElement DOM.HTMLHeadingElement a) => a
h2_ = el "h2" DOM.HTMLHeadingElement

{-# INLINE select_ #-}
select_ :: (ConstructVirtualDomElement DOM.HTMLSelectElement a) => a
select_ = el "select" DOM.HTMLSelectElement

{-# INLINE option_ #-}
option_ :: (ConstructVirtualDomElement DOM.HTMLOptionElement a) => a
option_ = el "option" DOM.HTMLOptionElement

-- Properties
-- --------------------------------------------------------------------

class_ :: (DOM.IsElement el) => T.Text -> NamedElementProperty el
class_ txt = NamedElementProperty "className" $ ElementProperty
  { eaGetProperty = DOM.getClassName
  , eaSetProperty = DOM.setClassName
  , eaValue = txt
  }

inputType_ :: T.Text -> NamedElementProperty DOM.HTMLInputElement
inputType_ txt = NamedElementProperty "type" $ ElementProperty
  { eaGetProperty = DOM.Input.getType
  , eaSetProperty = DOM.Input.setType
  , eaValue = txt
  }

aHref_ :: (DOM.IsHTMLHyperlinkElementUtils el) => T.Text -> NamedElementProperty el
aHref_ txt = NamedElementProperty "href" $ ElementProperty
  { eaGetProperty = DOM.HyperlinkElementUtils.getHref
  , eaSetProperty = DOM.HyperlinkElementUtils.setHref
  , eaValue = txt
  }

inputValue_ :: T.Text -> NamedElementProperty DOM.HTMLInputElement
inputValue_ txt = NamedElementProperty "value" $ ElementProperty
  { eaGetProperty = DOM.Input.getValue
  , eaSetProperty = DOM.Input.setValue
  , eaValue = txt
  }

optionValue_ :: T.Text -> NamedElementProperty DOM.HTMLOptionElement
optionValue_ txt = NamedElementProperty "value" $ ElementProperty
  { eaGetProperty = DOM.Option.getValue
  , eaSetProperty = DOM.Option.setValue
  , eaValue = txt
  }

inputChecked_ :: Bool -> NamedElementProperty DOM.HTMLInputElement
inputChecked_ b = NamedElementProperty "checked" $ ElementProperty
  { eaGetProperty = DOM.Input.getChecked
  , eaSetProperty = DOM.Input.setChecked
  , eaValue = b
  }

selected_ :: Bool -> NamedElementProperty DOM.HTMLOptionElement
selected_ b = NamedElementProperty "selected" $ ElementProperty
  { eaGetProperty = DOM.Option.getSelected
  , eaSetProperty = DOM.Option.setSelected
  , eaValue = b
  }

{-
name_ :: T.Text -> Attr
name_ = Attr "name" . Just
-}

-- Events
-- --------------------------------------------------------------------

onclick_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.MouseEvent -> ClientM ()) -> SomeEvent el
onclick_ = SomeEvent DOM.click

onchange_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> ClientM ()) -> SomeEvent el
onchange_ = SomeEvent DOM.change

oninput_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> ClientM ()) -> SomeEvent el
oninput_ = SomeEvent DOM.input

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> ClientM ()) -> SomeEvent el
onsubmit_ = SomeEvent DOM.submit
