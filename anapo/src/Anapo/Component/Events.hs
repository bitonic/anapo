module Anapo.Component.Events where

import Anapo.Component.Internal
import GHCJS.DOM.Types
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM

-- Events
-- --------------------------------------------------------------------
 
{-# INLINE onabort_ #-}
onabort_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onabort_ = NPEvent . SomeEventAction DOM.abort

{-# INLINE onblur_ #-}
onblur_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> FocusEvent -> Action ctx st ()) -> NodePatch el ctx st
onblur_ = NPEvent . SomeEventAction DOM.blur

{-# INLINE oncanplay_ #-}
oncanplay_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncanplay_ = NPEvent . SomeEventAction DOM.canPlay

{-# INLINE oncanplaythrough_ #-}
oncanplaythrough_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncanplaythrough_ = NPEvent . SomeEventAction DOM.canPlayThrough

{-# INLINE onchange_ #-}
onchange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onchange_ = NPEvent . SomeEventAction DOM.change

{-# INLINE onclick_ #-}
onclick_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onclick_ = NPEvent . SomeEventAction DOM.click

{-# INLINE oncontextmenu_ #-}
oncontextmenu_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
oncontextmenu_ = NPEvent . SomeEventAction DOM.contextMenu

{-# INLINE oncuechange_ #-}
oncuechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncuechange_ = NPEvent . SomeEventAction DOM.cueChange

{-# INLINE ondblclick_ #-}
ondblclick_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondblclick_ = NPEvent . SomeEventAction DOM.dblClick

{-# INLINE ondrag_ #-}
ondrag_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondrag_ = NPEvent . SomeEventAction DOM.drag

{-# INLINE ondragend_ #-}
ondragend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragend_ = NPEvent . SomeEventAction DOM.dragEnd

{-# INLINE ondragenter_ #-}
ondragenter_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragenter_ = NPEvent . SomeEventAction DOM.dragEnter

{-# INLINE ondragleave_ #-}
ondragleave_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragleave_ = NPEvent . SomeEventAction DOM.dragLeave

{-# INLINE ondragover_ #-}
ondragover_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragover_ = NPEvent . SomeEventAction DOM.dragOver

{-# INLINE ondragstart_ #-}
ondragstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragstart_ = NPEvent . SomeEventAction DOM.dragStart

{-# INLINE ondrop_ #-}
ondrop_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondrop_ = NPEvent . SomeEventAction DOM.drop

{-# INLINE ondurationchange_ #-}
ondurationchange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
ondurationchange_ = NPEvent . SomeEventAction DOM.durationChange

{-# INLINE onemptied_ #-}
onemptied_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onemptied_ = NPEvent . SomeEventAction DOM.emptied

{-# INLINE onended_ #-}
onended_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onended_ = NPEvent . SomeEventAction DOM.ended

{-# INLINE onerror_ #-}
onerror_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onerror_ = NPEvent . SomeEventAction DOM.error

{-# INLINE onfocus_ #-}
onfocus_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> FocusEvent -> Action ctx st ()) -> NodePatch el ctx st
onfocus_ = NPEvent . SomeEventAction DOM.focus

{-# INLINE oninput_ #-}
oninput_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oninput_ = NPEvent . SomeEventAction DOM.input

{-# INLINE oninvalid_ #-}
oninvalid_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oninvalid_ = NPEvent . SomeEventAction DOM.invalid

{-# INLINE onkeydown_ #-}
onkeydown_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeydown_ = NPEvent . SomeEventAction DOM.keyDown

{-# INLINE onkeypress_ #-}
onkeypress_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeypress_ = NPEvent . SomeEventAction DOM.keyPress

{-# INLINE onkeyup_ #-}
onkeyup_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeyup_ = NPEvent . SomeEventAction DOM.keyUp

{-# INLINE onload_ #-}
onload_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onload_ = NPEvent . SomeEventAction DOM.load

{-# INLINE onloadeddata_ #-}
onloadeddata_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onloadeddata_ = NPEvent . SomeEventAction DOM.loadedData

{-# INLINE onloadedmetadata_ #-}
onloadedmetadata_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onloadedmetadata_ = NPEvent . SomeEventAction DOM.loadedMetadata

{-# INLINE onloadstart_ #-}
onloadstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> ProgressEvent -> Action ctx st ()) -> NodePatch el ctx st
onloadstart_ = NPEvent . SomeEventAction DOM.loadStart

{-# INLINE onmousedown_ #-}
onmousedown_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousedown_ = NPEvent . SomeEventAction DOM.mouseDown

{-# INLINE onmouseenter_ #-}
onmouseenter_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseenter_ = NPEvent . SomeEventAction DOM.mouseEnter

{-# INLINE onmouseleave_ #-}
onmouseleave_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseleave_ = NPEvent . SomeEventAction DOM.mouseLeave

{-# INLINE onmousemove_ #-}
onmousemove_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousemove_ = NPEvent . SomeEventAction DOM.mouseMove

{-# INLINE onmouseout_ #-}
onmouseout_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseout_ = NPEvent . SomeEventAction DOM.mouseOut

{-# INLINE onmouseover_ #-}
onmouseover_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseover_ = NPEvent . SomeEventAction DOM.mouseOver

{-# INLINE onmouseup_ #-}
onmouseup_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseup_ = NPEvent . SomeEventAction DOM.mouseUp

{-# INLINE onmousewheel_ #-}
onmousewheel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousewheel_ = NPEvent . SomeEventAction DOM.mouseWheel

{-# INLINE onpause_ #-}
onpause_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onpause_ = NPEvent . SomeEventAction DOM.pause

{-# INLINE onplay_ #-}
onplay_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onplay_ = NPEvent . SomeEventAction DOM.play

{-# INLINE onplaying_ #-}
onplaying_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onplaying_ = NPEvent . SomeEventAction DOM.playing

{-# INLINE onprogress_ #-}
onprogress_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> ProgressEvent -> Action ctx st ()) -> NodePatch el ctx st
onprogress_ = NPEvent . SomeEventAction DOM.progress

{-# INLINE onratechange_ #-}
onratechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onratechange_ = NPEvent . SomeEventAction DOM.rateChange

{-# INLINE onreset_ #-}
onreset_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onreset_ = NPEvent . SomeEventAction DOM.reset

{-# INLINE onresize_ #-}
onresize_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onresize_ = NPEvent . SomeEventAction DOM.resize

{-# INLINE onscroll_ #-}
onscroll_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onscroll_ = NPEvent . SomeEventAction DOM.scroll

{-# INLINE onseeked_ #-}
onseeked_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onseeked_ = NPEvent . SomeEventAction DOM.seeked

{-# INLINE onseeking_ #-}
onseeking_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onseeking_ = NPEvent . SomeEventAction DOM.seeking

{-# INLINE onselect_ #-}
onselect_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onselect_ = NPEvent . SomeEventAction DOM.select

{-# INLINE onstalled_ #-}
onstalled_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onstalled_ = NPEvent . SomeEventAction DOM.stalled

{-# INLINE onsubmit_ #-}
onsubmit_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsubmit_ = NPEvent . SomeEventAction DOM.submit

{-# INLINE onsuspend_ #-}
onsuspend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsuspend_ = NPEvent . SomeEventAction DOM.suspend

{-# INLINE ontimeupdate_ #-}
ontimeupdate_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
ontimeupdate_ = NPEvent . SomeEventAction DOM.timeUpdate

{-# INLINE onvolumechange_ #-}
onvolumechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onvolumechange_ = NPEvent . SomeEventAction DOM.volumeChange

{-# INLINE onwaiting_ #-}
onwaiting_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onwaiting_ = NPEvent . SomeEventAction DOM.waiting

{-# INLINE ontransitionend_ #-}
ontransitionend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TransitionEvent -> Action ctx st ()) -> NodePatch el ctx st
ontransitionend_ = NPEvent . SomeEventAction DOM.transitionEnd

{-# INLINE onanimationend_ #-}
onanimationend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationend_ = NPEvent . SomeEventAction DOM.animationEnd

{-# INLINE onanimationiteration_ #-}
onanimationiteration_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationiteration_ = NPEvent . SomeEventAction DOM.animationIteration

{-# INLINE onanimationstart_ #-}
onanimationstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationstart_ = NPEvent . SomeEventAction DOM.animationStart

{-# INLINE onsearch_ #-}
onsearch_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsearch_ = NPEvent . SomeEventAction DOM.search

{-# INLINE onwheel_ #-}
onwheel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> WheelEvent -> Action ctx st ()) -> NodePatch el ctx st
onwheel_ = NPEvent . SomeEventAction DOM.wheel

{-# INLINE ontouchcancel_ #-}
ontouchcancel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchcancel_ = NPEvent . SomeEventAction DOM.touchCancel

{-# INLINE ontouchend_ #-}
ontouchend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchend_ = NPEvent . SomeEventAction DOM.touchEnd

{-# INLINE ontouchmove_ #-}
ontouchmove_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchmove_ = NPEvent . SomeEventAction DOM.touchMove

{-# INLINE ontouchstart_ #-}
ontouchstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchstart_ = NPEvent . SomeEventAction DOM.touchStart
