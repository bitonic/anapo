module Anapo.Component.Events where

import Anapo.Component.Internal
import GHCJS.DOM.Types
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM

-- Events
-- --------------------------------------------------------------------

onabort_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onabort_ = NPEvent . SomeEventAction DOM.abort

onblur_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> FocusEvent -> Action ctx st ()) -> NodePatch el ctx st
onblur_ = NPEvent . SomeEventAction DOM.blur

oncanplay_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncanplay_ = NPEvent . SomeEventAction DOM.canPlay

oncanplaythrough_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncanplaythrough_ = NPEvent . SomeEventAction DOM.canPlayThrough

onchange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onchange_ = NPEvent . SomeEventAction DOM.change

onclick_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onclick_ = NPEvent . SomeEventAction DOM.click

oncontextmenu_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
oncontextmenu_ = NPEvent . SomeEventAction DOM.contextMenu

oncuechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncuechange_ = NPEvent . SomeEventAction DOM.cueChange

ondblclick_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondblclick_ = NPEvent . SomeEventAction DOM.dblClick

ondrag_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondrag_ = NPEvent . SomeEventAction DOM.drag

ondragend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragend_ = NPEvent . SomeEventAction DOM.dragEnd

ondragenter_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragenter_ = NPEvent . SomeEventAction DOM.dragEnter

ondragleave_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragleave_ = NPEvent . SomeEventAction DOM.dragLeave

ondragover_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragover_ = NPEvent . SomeEventAction DOM.dragOver

ondragstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragstart_ = NPEvent . SomeEventAction DOM.dragStart

ondrop_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondrop_ = NPEvent . SomeEventAction DOM.drop

ondurationchange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
ondurationchange_ = NPEvent . SomeEventAction DOM.durationChange

onemptied_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onemptied_ = NPEvent . SomeEventAction DOM.emptied

onended_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onended_ = NPEvent . SomeEventAction DOM.ended

onerror_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onerror_ = NPEvent . SomeEventAction DOM.error

onfocus_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> FocusEvent -> Action ctx st ()) -> NodePatch el ctx st
onfocus_ = NPEvent . SomeEventAction DOM.focus

oninput_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oninput_ = NPEvent . SomeEventAction DOM.input

oninvalid_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oninvalid_ = NPEvent . SomeEventAction DOM.invalid

onkeydown_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeydown_ = NPEvent . SomeEventAction DOM.keyDown

onkeypress_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeypress_ = NPEvent . SomeEventAction DOM.keyPress

onkeyup_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeyup_ = NPEvent . SomeEventAction DOM.keyUp

onload_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onload_ = NPEvent . SomeEventAction DOM.load

onloadeddata_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onloadeddata_ = NPEvent . SomeEventAction DOM.loadedData

onloadedmetadata_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onloadedmetadata_ = NPEvent . SomeEventAction DOM.loadedMetadata

onloadstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> ProgressEvent -> Action ctx st ()) -> NodePatch el ctx st
onloadstart_ = NPEvent . SomeEventAction DOM.loadStart

onmousedown_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousedown_ = NPEvent . SomeEventAction DOM.mouseDown

onmouseenter_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseenter_ = NPEvent . SomeEventAction DOM.mouseEnter

onmouseleave_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseleave_ = NPEvent . SomeEventAction DOM.mouseLeave

onmousemove_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousemove_ = NPEvent . SomeEventAction DOM.mouseMove

onmouseout_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseout_ = NPEvent . SomeEventAction DOM.mouseOut

onmouseover_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseover_ = NPEvent . SomeEventAction DOM.mouseOver

onmouseup_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseup_ = NPEvent . SomeEventAction DOM.mouseUp

onmousewheel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousewheel_ = NPEvent . SomeEventAction DOM.mouseWheel

onpause_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onpause_ = NPEvent . SomeEventAction DOM.pause

onplay_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onplay_ = NPEvent . SomeEventAction DOM.play

onplaying_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onplaying_ = NPEvent . SomeEventAction DOM.playing

onprogress_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> ProgressEvent -> Action ctx st ()) -> NodePatch el ctx st
onprogress_ = NPEvent . SomeEventAction DOM.progress

onratechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onratechange_ = NPEvent . SomeEventAction DOM.rateChange

onreset_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onreset_ = NPEvent . SomeEventAction DOM.reset

onresize_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onresize_ = NPEvent . SomeEventAction DOM.resize

onscroll_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onscroll_ = NPEvent . SomeEventAction DOM.scroll

onseeked_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onseeked_ = NPEvent . SomeEventAction DOM.seeked

onseeking_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onseeking_ = NPEvent . SomeEventAction DOM.seeking

onselect_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onselect_ = NPEvent . SomeEventAction DOM.select

onstalled_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onstalled_ = NPEvent . SomeEventAction DOM.stalled

onsubmit_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsubmit_ = NPEvent . SomeEventAction DOM.submit

onsuspend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsuspend_ = NPEvent . SomeEventAction DOM.suspend

ontimeupdate_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
ontimeupdate_ = NPEvent . SomeEventAction DOM.timeUpdate

onvolumechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onvolumechange_ = NPEvent . SomeEventAction DOM.volumeChange

onwaiting_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onwaiting_ = NPEvent . SomeEventAction DOM.waiting

ontransitionend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TransitionEvent -> Action ctx st ()) -> NodePatch el ctx st
ontransitionend_ = NPEvent . SomeEventAction DOM.transitionEnd

onanimationend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationend_ = NPEvent . SomeEventAction DOM.animationEnd

onanimationiteration_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationiteration_ = NPEvent . SomeEventAction DOM.animationIteration

onanimationstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationstart_ = NPEvent . SomeEventAction DOM.animationStart

onsearch_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsearch_ = NPEvent . SomeEventAction DOM.search

onwheel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> WheelEvent -> Action ctx st ()) -> NodePatch el ctx st
onwheel_ = NPEvent . SomeEventAction DOM.wheel

ontouchcancel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchcancel_ = NPEvent . SomeEventAction DOM.touchCancel

ontouchend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchend_ = NPEvent . SomeEventAction DOM.touchEnd

ontouchmove_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchmove_ = NPEvent . SomeEventAction DOM.touchMove

ontouchstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchstart_ = NPEvent . SomeEventAction DOM.touchStart
