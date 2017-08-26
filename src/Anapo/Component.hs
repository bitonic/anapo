{-# LANGUAGE OverloadedStrings #-}
module Anapo.Component
  ( -- * Re-exports
    VirtualDom
  , VirtualDomNode
  , ClientM
  , runClientM

    -- * ComponentM
  , ComponentM
  , Component
  , KeyedComponent
  , Node
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

    -- * attributes
  , class_
  , type_
  , href_
  , value_
  , li_
  , ul_
  , button_
  , checked_
  , name_

    -- * events
  , onclick_
  , onchange_
  , onsubmit_

    -- * Type class machinery
  , ConstructVirtualDomElement
  ) where

import qualified Data.HashMap.Strict as HMS
import Control.Lens (Lens', view, set, over)
import Control.Monad (ap, guard)
import qualified Data.Text as T
import Data.Monoid ((<>))
import GHC.StaticPtr
import qualified Data.DList as DList
import Data.String (IsString(..))
import Data.List (foldl')

import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM

import Anapo.Core

-- Monad
-- --------------------------------------------------------------------

type Dispatch state = (state -> ClientM state) -> ClientM ()

newtype ComponentM dom state a = ComponentM
  { unComponentM :: forall out. Lens' out state -> Dispatch out -> Maybe out -> out -> (dom, a) }
type Component state = ComponentM VirtualDom state ()
type KeyedComponent state = ComponentM KeyedVirtualDom state ()
type Node state = ComponentM () state VirtualDomNode

instance IsString (Node state) where
  fromString = text . T.pack

{-# INLINE runComponentM #-}
runComponentM :: ComponentM dom state a -> Dispatch state -> Maybe state -> state -> (dom, a)
runComponentM vdom = unComponentM vdom id

{-# INLINE runComponent #-}
runComponent :: Component state -> Dispatch state -> Maybe state -> state -> VirtualDom
runComponent vdom d mbst st = fst (unComponentM vdom id d mbst st)

instance Functor (ComponentM dom state) where
  {-# INLINE fmap #-}
  fmap f (ComponentM g) = ComponentM $ \l d mbst st -> let
    (dom, x) = g l d mbst st
    in (dom, f x)

instance (Monoid dom) => Applicative (ComponentM dom state) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance (Monoid dom) => Monad (ComponentM dom state) where
  {-# INLINE return #-}
  return x = ComponentM (\_l _d _mbst _st -> (mempty, x))
  {-# INLINE (>>=) #-}
  ma >>= mf = ComponentM $ \l d mbst st -> let
    (vdom1, x) = unComponentM ma l d mbst st
    (vdom2, y) = unComponentM (mf x) l d mbst st
    !vdom = vdom1 <> vdom2
    in (vdom, y)

{-# INLINE askDispatchM #-}
askDispatchM :: (Monoid dom) => ComponentM dom state (Dispatch state)
askDispatchM = ComponentM $ \lst d _mbst _st ->
  ( mempty
  , \modifySt -> d (\st -> do st' <- modifySt (view lst st); return (set lst st' st))
  )

{-# INLINE askDispatch #-}
askDispatch :: (Monoid dom) => ComponentM dom state ((state -> state) -> ClientM ())
askDispatch = ComponentM $ \lst d _mbst _st ->
  ( mempty
  , \modifySt -> d (\st -> return (over lst modifySt st))
  )

{-# INLINE askState #-}
askState :: (Monoid dom) => ComponentM dom state state
askState = ComponentM (\lst _d _mbst st -> (mempty, view lst st))

{-# INLINE askPreviousState #-}
askPreviousState :: (Monoid dom) => ComponentM dom state (Maybe state)
askPreviousState = ComponentM (\lst _d mbst _st -> (mempty, fmap (view lst) mbst))

{-# INLINE key #-}
key :: T.Text -> Node state -> KeyedComponent state
key k getNode = ComponentM $ \l d mbst st -> let
  !(_, !nod) = unComponentM getNode l d mbst st
  in (KeyedVirtualDom (HMS.singleton k nod) (DList.singleton k), ())

{-# INLINE n #-}
n :: Node state -> Component state
n getNode = ComponentM $ \l d mbst st -> let
  !(_, !nod) = unComponentM getNode l d mbst st
  in (DList.singleton nod, ())

{-# INLINE text #-}
text :: T.Text -> Node state
text txt = return $ VirtualDomNode
  { vdnMark = Nothing
  , vdnWrap = DOM.Text
  , vdnBody = VDNBText txt
  , vdnCallbacks = noVirtualDomNodeCallbacks
  }

{-# INLINE marked #-}
marked ::
     (state -> state -> Render)
  -> StaticPtr (Node state)
  -> Node state
marked rerender domPtr = ComponentM $ \l d mbst st -> let
  !render = case mbst of
    Nothing -> ReRender
    Just oldSt -> rerender (view l oldSt) (view l st)
  !fprint = staticKey domPtr
  -- do not force evaluation for this one since we actually
  -- want it to be lazy to avoid computing the vdom if we don't
  -- have to
  (_, dom) = unComponentM (deRefStaticPtr domPtr) l d mbst st
  in ((), dom{ vdnMark = Just (VirtualDomNodeMark fprint render) })

{-# INLINE zoom #-}
zoom :: Lens' out in_ -> ComponentM dom in_ a -> ComponentM dom out a
zoom l' dom = ComponentM (\l d mbst st -> unComponentM dom (l . l') d mbst st)

{-# INLINE rawNode #-}
rawNode :: (DOM.IsNode el) => (DOM.JSVal -> el) -> el -> Node state
rawNode wrap x = return $ VirtualDomNode
  { vdnMark = Nothing
  , vdnBody = VDNBRawNode x
  , vdnWrap = wrap
  , vdnCallbacks = noVirtualDomNodeCallbacks
  }

class (DOM.IsElement el) => ConstructVirtualDomElement el a where
  constructVirtualDomElement :: ElementTag -> (DOM.JSVal -> el) -> [Attr] -> [SomeEvent el] -> a

{-# INLINE constructVirtualDomElement_ #-}
constructVirtualDomElement_ ::
     (DOM.IsElement el)
  => ElementTag -> (DOM.JSVal -> el) -> [Attr] -> [SomeEvent el] -> VirtualDomChildren -> VirtualDomNode
constructVirtualDomElement_ tag f attrs evts child = VirtualDomNode
  { vdnMark = Nothing
  , vdnWrap = f
  , vdnBody = VDNBElement $ VirtualDomElement
      { vdeTag = tag
      , vdeAttributes = foldl'
          (\attrMap (Attr attrName mbAttr) -> case mbAttr of
              Just attr -> HMS.insert attrName attr attrMap
              Nothing -> HMS.delete attrName attrMap)
          HMS.empty
          (reverse attrs)
      , vdeEvents = reverse evts
      , vdeChildren = child
      }
  , vdnCallbacks = noVirtualDomNodeCallbacks
  }

instance (DOM.IsElement el) => ConstructVirtualDomElement el (ComponentM () state VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts =
    return (constructVirtualDomElement_ tag f attrs evts (VDCNormal mempty))

instance (DOM.IsElement el, state1 ~ state2) => ConstructVirtualDomElement el (ComponentM VirtualDom state1 () -> ComponentM () state2 VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts dom = ComponentM $ \l d mbst st -> let
    !(!vdom, _) = unComponentM dom l d mbst st
    in ((), constructVirtualDomElement_ tag f attrs evts (VDCNormal vdom))

instance (DOM.IsElement el, state1 ~ state2) => ConstructVirtualDomElement el (ComponentM KeyedVirtualDom state1 () -> ComponentM () state2 VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts dom = ComponentM $ \l d mbst st -> let
    !(!vdom, _) = unComponentM dom l d mbst st
    in ((), constructVirtualDomElement_ tag f attrs evts (VDCKeyed vdom))

newtype UnsafeRawHtml = UnsafeRawHtml T.Text

instance (DOM.IsElement el) => ConstructVirtualDomElement el (UnsafeRawHtml -> ComponentM () state VirtualDomNode) where
  {-# INLINE constructVirtualDomElement #-}
  constructVirtualDomElement tag f attrs evts (UnsafeRawHtml html) = return (constructVirtualDomElement_ tag f attrs evts (VDCRawHtml html))

data Attr = Attr T.Text (Maybe T.Text)

instance (ConstructVirtualDomElement el a) => ConstructVirtualDomElement el (Attr -> a) where
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

-- Attributes
-- --------------------------------------------------------------------

class_ :: T.Text -> Attr
class_ = Attr "class" . Just

type_ :: T.Text -> Attr
type_ = Attr "type" . Just

href_ :: T.Text -> Attr
href_ = Attr "href" . Just

value_ :: T.Text -> Attr
value_ = Attr "value" . Just

checked_ :: Bool -> Attr
checked_ b = Attr "checked" ("" <$ guard b)

name_ :: T.Text -> Attr
name_ = Attr "name" . Just

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

onsubmit_ ::
     (DOM.IsElement el, DOM.IsGlobalEventHandlers el)
  => (el -> DOM.Event -> ClientM ()) -> SomeEvent el
onsubmit_ = SomeEvent DOM.submit
