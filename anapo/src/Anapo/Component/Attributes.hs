{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
module Anapo.Component.Attributes where

import Anapo.Component.Internal (textProperty, textAttribute, boolProperty, boolAttribute, NodePatch(NPClasses))
import Anapo.Text (Text)
import qualified Anapo.Text as T
import Data.Monoid ((<>))

#if defined(ghcjs_HOST_OS)
import Unsafe.Coerce (unsafeCoerce)
import Data.JSString (JSString)
import qualified GHC.Exts as Exts
#endif

{-# INLINE accept_ #-}
accept_ :: Text -> NodePatch el ctx st
accept_ = textProperty (T.pack "accept")

{-# INLINE acceptCharset_ #-}
acceptCharset_ :: Text -> NodePatch el ctx st
acceptCharset_ = textProperty (T.pack "acceptCharset")

{-# INLINE accesskey_ #-}
accesskey_ :: Text -> NodePatch el ctx st
accesskey_ = textProperty (T.pack "accessKey")

{-# INLINE action_ #-}
action_ :: Text -> NodePatch el ctx st
action_ = textProperty (T.pack "action")

{-# INLINE alt_ #-}
alt_ :: Text -> NodePatch el ctx st
alt_ = textProperty (T.pack "alt")

{-# INLINE async_ #-}
async_ :: Text -> NodePatch el ctx st
async_ = textProperty (T.pack "async")

{-# INLINE autocomplete_ #-}
autocomplete_ :: Text -> NodePatch el ctx st
autocomplete_ = textProperty (T.pack "autocomplete")

{-# INLINE autofocus_ #-}
autofocus_ :: Bool -> NodePatch el ctx st
autofocus_ = boolProperty (T.pack "autofocus")

{-# INLINE autoplay_ #-}
autoplay_ :: Text -> NodePatch el ctx st
autoplay_ = textProperty (T.pack "autoplay")

{-# INLINE challenge_ #-}
challenge_ :: Text -> NodePatch el ctx st
challenge_ = textProperty (T.pack "challenge")

{-# INLINE charset_ #-}
charset_ :: Text -> NodePatch el ctx st
charset_ = textProperty (T.pack "charset")

{-# INLINE checked_ #-}
checked_ :: Bool -> NodePatch el ctx st
checked_ = boolProperty (T.pack "checked")

{-# INLINE class_ #-}
class_ :: Text -> NodePatch el ctx st
class_ = NPClasses . filter (not . T.null) . textWords

#if defined(ghcjs_HOST_OS)

foreign import javascript unsafe
  "h$toHsListJSVal($1.split(\" \"))"
  js_textWords :: JSString -> Exts.Any

{-# INLINE textWords #-}
textWords :: JSString -> [JSString]
textWords s = unsafeCoerce (js_textWords s)

#else

textWords :: Text -> [Text]
textWords = T.words

#endif

{-# INLINE classes_ #-}
classes_ :: [Text] -> NodePatch el ctx st
classes_ = NPClasses . filter (not . T.null)

{-# INLINE cols_ #-}
cols_ :: Text -> NodePatch el ctx st
cols_ = textProperty (T.pack "cols")

{-# INLINE colspan_ #-}
colspan_ :: Text -> NodePatch el ctx st
colspan_ = textProperty (T.pack "colSpan")

{-# INLINE content_ #-}
content_ :: Text -> NodePatch el ctx st
content_ = textProperty (T.pack "content")

{-# INLINE contenteditable_ #-}
contenteditable_ :: Bool -> NodePatch el ctx st
contenteditable_ = boolProperty (T.pack "contentEditable")

{-# INLINE contextmenu_ #-}
contextmenu_ :: Text -> NodePatch el ctx st
contextmenu_ = textAttribute (T.pack "contextmenu")

{-# INLINE controls_ #-}
controls_ :: Text -> NodePatch el ctx st
controls_ = textProperty (T.pack "controls")

{-# INLINE coords_ #-}
coords_ :: Text -> NodePatch el ctx st
coords_ = textProperty (T.pack "coords")

{-# INLINE crossorigin_ #-}
crossorigin_ :: Text -> NodePatch el ctx st
crossorigin_ = textProperty (T.pack "crossOrigin")

{-# INLINE data_ #-}
data_ :: Text -> Text -> NodePatch el ctx st
data_ name = textAttribute (T.pack "data-" <> name)

{-# INLINE datetime_ #-}
datetime_ :: Text -> NodePatch el ctx st
datetime_ = textProperty (T.pack "dateTime")

{-# INLINE defer_ #-}
defer_ :: Text -> NodePatch el ctx st
defer_ = textProperty (T.pack "defer")

{-# INLINE dir_ #-}
dir_ :: Text -> NodePatch el ctx st
dir_ = textProperty (T.pack "dir")

{-# INLINE disabled_ #-}
disabled_ :: Bool -> NodePatch el ctx st
disabled_ = boolProperty (T.pack "disabled")

{-# INLINE download_ #-}
download_ :: Text -> NodePatch el ctx st
download_ = textProperty (T.pack "download")

{-# INLINE draggable_ #-}
draggable_ :: Bool -> NodePatch el ctx st
draggable_ = boolProperty (T.pack "draggable")

{-# INLINE enctype_ #-}
enctype_ :: Text -> NodePatch el ctx st
enctype_ = textProperty (T.pack "enctype")

{-# INLINE for_ #-}
for_ :: Text -> NodePatch el ctx st
for_ = textProperty (T.pack "htmlFor")

{-# INLINE formaction_ #-}
formaction_ :: Text -> NodePatch el ctx st
formaction_ = textProperty (T.pack "formAction")

{-# INLINE formenctype_ #-}
formenctype_ :: Text -> NodePatch el ctx st
formenctype_ = textProperty (T.pack "formEnctype")

{-# INLINE formmethod_ #-}
formmethod_ :: Text -> NodePatch el ctx st
formmethod_ = textProperty (T.pack "formMethod")

{-# INLINE formnovalidate_ #-}
formnovalidate_ :: Text -> NodePatch el ctx st
formnovalidate_ = textProperty (T.pack "formNoValidate")

{-# INLINE formtarget_ #-}
formtarget_ :: Text -> NodePatch el ctx st
formtarget_ = textProperty (T.pack "formTarget")

{-# INLINE headers_ #-}
headers_ :: Text -> NodePatch el ctx st
headers_ = textProperty (T.pack "headers")

{-# INLINE height_ #-}
height_ :: Text -> NodePatch el ctx st
height_ = textProperty (T.pack "height")

{-# INLINE hidden_ #-}
hidden_ :: Text -> NodePatch el ctx st
hidden_ = textProperty (T.pack "hidden")

{-# INLINE high_ #-}
high_ :: Text -> NodePatch el ctx st
high_ = textProperty (T.pack "high")

{-# INLINE href_ #-}
href_ :: Text -> NodePatch el ctx st
href_ = textProperty (T.pack "href")

{-# INLINE hreflang_ #-}
hreflang_ :: Text -> NodePatch el ctx st
hreflang_ = textProperty (T.pack "hreflang")

{-# INLINE httpEquiv_ #-}
httpEquiv_ :: Text -> NodePatch el ctx st
httpEquiv_ = textProperty (T.pack "httpEquiv")

{-# INLINE id_ #-}
id_ :: Text -> NodePatch el ctx st
id_ = textProperty (T.pack "id")

{-# INLINE integrity_ #-}
integrity_ :: Text -> NodePatch el ctx st
integrity_ = textProperty (T.pack "integrity")

{-# INLINE ismap_ #-}
ismap_ :: Bool -> NodePatch el ctx st
ismap_ = boolAttribute (T.pack "ismap")

{-# INLINE item_ #-}
item_ :: Text -> NodePatch el ctx st
item_ = textProperty (T.pack "item")

{-# INLINE itemprop_ #-}
itemprop_ :: Text -> NodePatch el ctx st
itemprop_ = textAttribute (T.pack "itemprop")

{-# INLINE keytype_ #-}
keytype_ :: Text -> NodePatch el ctx st
keytype_ = textProperty (T.pack "keytype")

{-# INLINE lang_ #-}
lang_ :: Text -> NodePatch el ctx st
lang_ = textProperty (T.pack "lang")

{-# INLINE list_ #-}
list_ :: Text -> NodePatch el ctx st
list_ = textProperty (T.pack "list")

{-# INLINE loop_ #-}
loop_ :: Text -> NodePatch el ctx st
loop_ = textProperty (T.pack "loop")

{-# INLINE low_ #-}
low_ :: Text -> NodePatch el ctx st
low_ = textProperty (T.pack "low")

{-# INLINE manifest_ #-}
manifest_ :: Text -> NodePatch el ctx st
manifest_ = textProperty (T.pack "manifest")

{-# INLINE max_ #-}
max_ :: Text -> NodePatch el ctx st
max_ = textProperty (T.pack "max")

{-# INLINE maxlength_ #-}
maxlength_ :: Text -> NodePatch el ctx st
maxlength_ = textProperty (T.pack "maxLength")

{-# INLINE minlength_ #-}
minlength_ :: Text -> NodePatch el ctx st
minlength_ = textProperty (T.pack "minLength")

{-# INLINE media_ #-}
media_ :: Text -> NodePatch el ctx st
media_ = textProperty (T.pack "media")

{-# INLINE method_ #-}
method_ :: Text -> NodePatch el ctx st
method_ = textProperty (T.pack "method")

{-# INLINE min_ #-}
min_ :: Text -> NodePatch el ctx st
min_ = textProperty (T.pack "min")

{-# INLINE multiple_ #-}
multiple_ :: Bool -> NodePatch el ctx st
multiple_ = boolProperty (T.pack "multiple")

{-# INLINE name_ #-}
name_ :: Text -> NodePatch el ctx st
name_ = textProperty (T.pack "name")

{-# INLINE novalidate_ #-}
novalidate_ :: Bool -> NodePatch el ctx st
novalidate_ = boolProperty (T.pack "noValidate")

{-# INLINE open_ #-}
open_ :: Bool -> NodePatch el ctx st
open_ = boolProperty (T.pack "open")

{-# INLINE optimum_ #-}
optimum_ :: Text -> NodePatch el ctx st
optimum_ = textProperty (T.pack "optimum")

{-# INLINE pattern_ #-}
pattern_ :: Text -> NodePatch el ctx st
pattern_ = textProperty (T.pack "pattern")

{-# INLINE ping_ #-}
ping_ :: Text -> NodePatch el ctx st
ping_ = textProperty (T.pack "ping")

{-# INLINE placeholder_ #-}
placeholder_ :: Text -> NodePatch el ctx st
placeholder_ = textProperty (T.pack "placeholder")

{-# INLINE preload_ #-}
preload_ :: Text -> NodePatch el ctx st
preload_ = textProperty (T.pack "preload")

{-# INLINE readonly_ #-}
readonly_ :: Bool -> NodePatch el ctx st
readonly_ = boolProperty (T.pack "readonly")

{-# INLINE rel_ #-}
rel_ :: Text -> NodePatch el ctx st
rel_ = textProperty (T.pack "rel")

{-# INLINE required_ #-}
required_ :: Bool -> NodePatch el ctx st
required_ = boolProperty (T.pack "required")

{-# INLINE reversed_ #-}
reversed_ :: Text -> NodePatch el ctx st
reversed_ = textProperty (T.pack "reversed")

{-# INLINE role_ #-}
role_ :: Text -> NodePatch el ctx st
role_ = textAttribute (T.pack "role")

{-# INLINE rows_ #-}
rows_ :: Text -> NodePatch el ctx st
rows_ = textProperty (T.pack "rows")

{-# INLINE rowspan_ #-}
rowspan_ :: Text -> NodePatch el ctx st
rowspan_ = textProperty (T.pack "rowSpan")

{-# INLINE sandbox_ #-}
sandbox_ :: Text -> NodePatch el ctx st
sandbox_ = textProperty (T.pack "sandbox")

{-# INLINE scope_ #-}
scope_ :: Text -> NodePatch el ctx st
scope_ = textProperty (T.pack "scope")

{-# INLINE selected_ #-}
selected_ :: Bool -> NodePatch el ctx st
selected_ = boolProperty (T.pack "selected")

{-# INLINE shape_ #-}
shape_ :: Text -> NodePatch el ctx st
shape_ = textProperty (T.pack "shape")

{-# INLINE size_ #-}
size_ :: Text -> NodePatch el ctx st
size_ = textProperty (T.pack "size")

{-# INLINE sizes_ #-}
sizes_ :: Text -> NodePatch el ctx st
sizes_ = textProperty (T.pack "sizes")

{-# INLINE spellcheck_ #-}
spellcheck_ :: Text -> NodePatch el ctx st
spellcheck_ = textProperty (T.pack "spellcheck")

{-# INLINE src_ #-}
src_ :: Text -> NodePatch el ctx st
src_ = textProperty (T.pack "src")

{-# INLINE srcdoc_ #-}
srcdoc_ :: Text -> NodePatch el ctx st
srcdoc_ = textProperty (T.pack "srcdoc")

{-# INLINE start_ #-}
start_ :: Text -> NodePatch el ctx st
start_ = textProperty (T.pack "start")

{-# INLINE step_ #-}
step_ :: Text -> NodePatch el ctx st
step_ = textProperty (T.pack "step")

{-# INLINE tabindex_ #-}
tabindex_ :: Text -> NodePatch el ctx st
tabindex_ = textProperty (T.pack "tabIndex")

{-# INLINE target_ #-}
target_ :: Text -> NodePatch el ctx st
target_ = textProperty (T.pack "target")

{-# INLINE type_ #-}
type_ :: Text -> NodePatch el ctx st
type_ = textProperty (T.pack "type")

{-# INLINE usemap_ #-}
usemap_ :: Text -> NodePatch el ctx st
usemap_ = textProperty (T.pack "useMap")

{-# INLINE value_ #-}
value_ :: Text -> NodePatch el ctx st
value_ = textProperty (T.pack "value")

{-# INLINE width_ #-}
width_ :: Text -> NodePatch el ctx st
width_ = textProperty (T.pack "width")

{-# INLINE wrap_ #-}
wrap_ :: Text -> NodePatch el ctx st
wrap_ = textProperty (T.pack "wrap")

{-# INLINE formAttr_ #-}
formAttr_ :: Text -> NodePatch el ctx st
formAttr_ = textProperty (T.pack "form")

{-# INLINE labelAttr_ #-}
labelAttr_ :: Text -> NodePatch el ctx st
labelAttr_ = textProperty (T.pack "label")

{-# INLINE spanAttr_ #-}
spanAttr_ :: Text -> NodePatch el ctx st
spanAttr_ = textProperty (T.pack "span")

{-# INLINE citeAttr_ #-}
citeAttr_ :: Text -> NodePatch el ctx st
citeAttr_ = textProperty (T.pack "cite")

{-# INLINE summaryAttr_ #-}
summaryAttr_ :: Text -> NodePatch el ctx st
summaryAttr_ = textProperty (T.pack "summary")
