{-# LANGUAGE OverloadedStrings #-}
module Anapo.Component.Events where

import Anapo.JsComponent.Internal
import GHCJS.DOM.Types
import qualified GHCJS.DOM.Types as DOM

-- Events
-- --------------------------------------------------------------------

{-# INLINE onabort_ #-}
onabort_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onabort_ = NPEvent . SomeEventAction "abort"

{-# INLINE onblur_ #-}
onblur_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> FocusEvent -> Action ctx st ()) -> NodePatch el ctx st
onblur_ = NPEvent . SomeEventAction "blur"

{-# INLINE oncanplay_ #-}
oncanplay_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncanplay_ = NPEvent . SomeEventAction "canPlay"

{-# INLINE oncanplaythrough_ #-}
oncanplaythrough_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncanplaythrough_ = NPEvent . SomeEventAction "canPlayThrough"

{-# INLINE onchange_ #-}
onchange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onchange_ = NPEvent . SomeEventAction "change"

{-# INLINE onclick_ #-}
onclick_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onclick_ = NPEvent . SomeEventAction "click"

{-# INLINE oncontextmenu_ #-}
oncontextmenu_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
oncontextmenu_ = NPEvent . SomeEventAction "contextMenu"

{-# INLINE oncuechange_ #-}
oncuechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oncuechange_ = NPEvent . SomeEventAction "cueChange"

{-# INLINE ondblclick_ #-}
ondblclick_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondblclick_ = NPEvent . SomeEventAction "dblClick"

{-# INLINE ondrag_ #-}
ondrag_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondrag_ = NPEvent . SomeEventAction "drag"

{-# INLINE ondragend_ #-}
ondragend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragend_ = NPEvent . SomeEventAction "dragEnd"

{-# INLINE ondragenter_ #-}
ondragenter_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragenter_ = NPEvent . SomeEventAction "dragEnter"

{-# INLINE ondragleave_ #-}
ondragleave_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragleave_ = NPEvent . SomeEventAction "dragLeave"

{-# INLINE ondragover_ #-}
ondragover_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragover_ = NPEvent . SomeEventAction "dragOver"

{-# INLINE ondragstart_ #-}
ondragstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondragstart_ = NPEvent . SomeEventAction "dragStart"

{-# INLINE ondrop_ #-}
ondrop_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
ondrop_ = NPEvent . SomeEventAction "drop"

{-# INLINE ondurationchange_ #-}
ondurationchange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
ondurationchange_ = NPEvent . SomeEventAction "durationChange"

{-# INLINE onemptied_ #-}
onemptied_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onemptied_ = NPEvent . SomeEventAction "emptied"

{-# INLINE onended_ #-}
onended_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onended_ = NPEvent . SomeEventAction "ended"

{-# INLINE onerror_ #-}
onerror_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onerror_ = NPEvent . SomeEventAction "error"

{-# INLINE onfocus_ #-}
onfocus_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> FocusEvent -> Action ctx st ()) -> NodePatch el ctx st
onfocus_ = NPEvent . SomeEventAction "focus"

{-# INLINE oninput_ #-}
oninput_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oninput_ = NPEvent . SomeEventAction "input"

{-# INLINE oninvalid_ #-}
oninvalid_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
oninvalid_ = NPEvent . SomeEventAction "invalid"

{-# INLINE onkeydown_ #-}
onkeydown_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeydown_ = NPEvent . SomeEventAction "keyDown"

{-# INLINE onkeypress_ #-}
onkeypress_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeypress_ = NPEvent . SomeEventAction "keyPress"

{-# INLINE onkeyup_ #-}
onkeyup_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> KeyboardEvent -> Action ctx st ()) -> NodePatch el ctx st
onkeyup_ = NPEvent . SomeEventAction "keyUp"

{-# INLINE onload_ #-}
onload_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onload_ = NPEvent . SomeEventAction "load"

{-# INLINE onloadeddata_ #-}
onloadeddata_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onloadeddata_ = NPEvent . SomeEventAction "loadedData"

{-# INLINE onloadedmetadata_ #-}
onloadedmetadata_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onloadedmetadata_ = NPEvent . SomeEventAction "loadedMetadata"

{-# INLINE onloadstart_ #-}
onloadstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> ProgressEvent -> Action ctx st ()) -> NodePatch el ctx st
onloadstart_ = NPEvent . SomeEventAction "loadStart"

{-# INLINE onmousedown_ #-}
onmousedown_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousedown_ = NPEvent . SomeEventAction "mouseDown"

{-# INLINE onmouseenter_ #-}
onmouseenter_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseenter_ = NPEvent . SomeEventAction "mouseEnter"

{-# INLINE onmouseleave_ #-}
onmouseleave_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseleave_ = NPEvent . SomeEventAction "mouseLeave"

{-# INLINE onmousemove_ #-}
onmousemove_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousemove_ = NPEvent . SomeEventAction "mouseMove"

{-# INLINE onmouseout_ #-}
onmouseout_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseout_ = NPEvent . SomeEventAction "mouseOut"

{-# INLINE onmouseover_ #-}
onmouseover_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseover_ = NPEvent . SomeEventAction "mouseOver"

{-# INLINE onmouseup_ #-}
onmouseup_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmouseup_ = NPEvent . SomeEventAction "mouseUp"

{-# INLINE onmousewheel_ #-}
onmousewheel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> MouseEvent -> Action ctx st ()) -> NodePatch el ctx st
onmousewheel_ = NPEvent . SomeEventAction "mouseWheel"

{-# INLINE onpause_ #-}
onpause_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onpause_ = NPEvent . SomeEventAction "pause"

{-# INLINE onplay_ #-}
onplay_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onplay_ = NPEvent . SomeEventAction "play"

{-# INLINE onplaying_ #-}
onplaying_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onplaying_ = NPEvent . SomeEventAction "playing"

{-# INLINE onprogress_ #-}
onprogress_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> ProgressEvent -> Action ctx st ()) -> NodePatch el ctx st
onprogress_ = NPEvent . SomeEventAction "progress"

{-# INLINE onratechange_ #-}
onratechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onratechange_ = NPEvent . SomeEventAction "rateChange"

{-# INLINE onreset_ #-}
onreset_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onreset_ = NPEvent . SomeEventAction "reset"

{-# INLINE onresize_ #-}
onresize_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onresize_ = NPEvent . SomeEventAction "resize"

{-# INLINE onscroll_ #-}
onscroll_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onscroll_ = NPEvent . SomeEventAction "scroll"

{-# INLINE onseeked_ #-}
onseeked_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onseeked_ = NPEvent . SomeEventAction "seeked"

{-# INLINE onseeking_ #-}
onseeking_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onseeking_ = NPEvent . SomeEventAction "seeking"

{-# INLINE onselect_ #-}
onselect_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> UIEvent -> Action ctx st ()) -> NodePatch el ctx st
onselect_ = NPEvent . SomeEventAction "select"

{-# INLINE onstalled_ #-}
onstalled_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onstalled_ = NPEvent . SomeEventAction "stalled"

{-# INLINE onsubmit_ #-}
onsubmit_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsubmit_ = NPEvent . SomeEventAction "submit"

{-# INLINE onsuspend_ #-}
onsuspend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsuspend_ = NPEvent . SomeEventAction "suspend"

{-# INLINE ontimeupdate_ #-}
ontimeupdate_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
ontimeupdate_ = NPEvent . SomeEventAction "timeUpdate"

{-# INLINE onvolumechange_ #-}
onvolumechange_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onvolumechange_ = NPEvent . SomeEventAction "volumeChange"

{-# INLINE onwaiting_ #-}
onwaiting_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onwaiting_ = NPEvent . SomeEventAction "waiting"

{-# INLINE ontransitionend_ #-}
ontransitionend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TransitionEvent -> Action ctx st ()) -> NodePatch el ctx st
ontransitionend_ = NPEvent . SomeEventAction "transitionEnd"

{-# INLINE onanimationend_ #-}
onanimationend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationend_ = NPEvent . SomeEventAction "animationEnd"

{-# INLINE onanimationiteration_ #-}
onanimationiteration_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationiteration_ = NPEvent . SomeEventAction "animationIteration"

{-# INLINE onanimationstart_ #-}
onanimationstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> AnimationEvent -> Action ctx st ()) -> NodePatch el ctx st
onanimationstart_ = NPEvent . SomeEventAction "animationStart"

{-# INLINE onsearch_ #-}
onsearch_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> Event -> Action ctx st ()) -> NodePatch el ctx st
onsearch_ = NPEvent . SomeEventAction "search"

{-# INLINE onwheel_ #-}
onwheel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> WheelEvent -> Action ctx st ()) -> NodePatch el ctx st
onwheel_ = NPEvent . SomeEventAction "wheel"

{-# INLINE ontouchcancel_ #-}
ontouchcancel_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchcancel_ = NPEvent . SomeEventAction "touchCancel"

{-# INLINE ontouchend_ #-}
ontouchend_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchend_ = NPEvent . SomeEventAction "touchEnd"

{-# INLINE ontouchmove_ #-}
ontouchmove_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchmove_ = NPEvent . SomeEventAction "touchMove"

{-# INLINE ontouchstart_ #-}
ontouchstart_ :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => (el -> TouchEvent -> Action ctx st ()) -> NodePatch el ctx st
ontouchstart_ = NPEvent . SomeEventAction "touchStart"
