{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
module Anapo.Component.Attributes where

import Anapo.JsComponent.Internal (property, attribute, rawAttribute, NodePatch(NPClasses))
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
accept_ = property (T.pack "accept")

{-# INLINE acceptCharset_ #-}
acceptCharset_ :: Text -> NodePatch el ctx st
acceptCharset_ = property (T.pack "acceptCharset")

{-# INLINE accesskey_ #-}
accesskey_ :: Text -> NodePatch el ctx st
accesskey_ = property (T.pack "accessKey")

{-# INLINE action_ #-}
action_ :: Text -> NodePatch el ctx st
action_ = property (T.pack "action")

{-# INLINE alt_ #-}
alt_ :: Text -> NodePatch el ctx st
alt_ = property (T.pack "alt")

{-# INLINE async_ #-}
async_ :: Text -> NodePatch el ctx st
async_ = property (T.pack "async")

{-# INLINE autocomplete_ #-}
autocomplete_ :: Text -> NodePatch el ctx st
autocomplete_ = property (T.pack "autocomplete")

{-# INLINE autofocus_ #-}
autofocus_ :: Bool -> NodePatch el ctx st
autofocus_ = property (T.pack "autofocus")

{-# INLINE autoplay_ #-}
autoplay_ :: Text -> NodePatch el ctx st
autoplay_ = property (T.pack "autoplay")

{-# INLINE challenge_ #-}
challenge_ :: Text -> NodePatch el ctx st
challenge_ = property (T.pack "challenge")

{-# INLINE charset_ #-}
charset_ :: Text -> NodePatch el ctx st
charset_ = property (T.pack "charset")

{-# INLINE checked_ #-}
checked_ :: Bool -> NodePatch el ctx st
checked_ = property (T.pack "checked")

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
cols_ = property (T.pack "cols")

{-# INLINE colspan_ #-}
colspan_ :: Text -> NodePatch el ctx st
colspan_ = property (T.pack "colSpan")

{-# INLINE content_ #-}
content_ :: Text -> NodePatch el ctx st
content_ = property (T.pack "content")

{-# INLINE contenteditable_ #-}
contenteditable_ :: Bool -> NodePatch el ctx st
contenteditable_ = property (T.pack "contentEditable")

{-# INLINE contextmenu_ #-}
contextmenu_ :: Text -> NodePatch el ctx st
contextmenu_ = attribute (T.pack "contextmenu")

{-# INLINE controls_ #-}
controls_ :: Text -> NodePatch el ctx st
controls_ = property (T.pack "controls")

{-# INLINE coords_ #-}
coords_ :: Text -> NodePatch el ctx st
coords_ = property (T.pack "coords")

{-# INLINE crossorigin_ #-}
crossorigin_ :: Text -> NodePatch el ctx st
crossorigin_ = property (T.pack "crossOrigin")

{-# INLINE data_ #-}
data_ :: Text -> Text -> NodePatch el ctx st
data_ name = attribute (T.pack "data-" <> name)

{-# INLINE datetime_ #-}
datetime_ :: Text -> NodePatch el ctx st
datetime_ = property (T.pack "dateTime")

{-# INLINE defer_ #-}
defer_ :: Text -> NodePatch el ctx st
defer_ = property (T.pack "defer")

{-# INLINE dir_ #-}
dir_ :: Text -> NodePatch el ctx st
dir_ = property (T.pack "dir")

{-# INLINE disabled_ #-}
disabled_ :: Bool -> NodePatch el ctx st
disabled_ = property (T.pack "disabled")

{-# INLINE download_ #-}
download_ :: Text -> NodePatch el ctx st
download_ = property (T.pack "download")

{-# INLINE draggable_ #-}
draggable_ :: Bool -> NodePatch el ctx st
draggable_ = property (T.pack "draggable")

{-# INLINE enctype_ #-}
enctype_ :: Text -> NodePatch el ctx st
enctype_ = property (T.pack "enctype")

{-# INLINE for_ #-}
for_ :: Text -> NodePatch el ctx st
for_ = property (T.pack "htmlFor")

{-# INLINE formaction_ #-}
formaction_ :: Text -> NodePatch el ctx st
formaction_ = property (T.pack "formAction")

{-# INLINE formenctype_ #-}
formenctype_ :: Text -> NodePatch el ctx st
formenctype_ = property (T.pack "formEnctype")

{-# INLINE formmethod_ #-}
formmethod_ :: Text -> NodePatch el ctx st
formmethod_ = property (T.pack "formMethod")

{-# INLINE formnovalidate_ #-}
formnovalidate_ :: Text -> NodePatch el ctx st
formnovalidate_ = property (T.pack "formNoValidate")

{-# INLINE formtarget_ #-}
formtarget_ :: Text -> NodePatch el ctx st
formtarget_ = property (T.pack "formTarget")

{-# INLINE headers_ #-}
headers_ :: Text -> NodePatch el ctx st
headers_ = property (T.pack "headers")

{-# INLINE height_ #-}
height_ :: Text -> NodePatch el ctx st
height_ = property (T.pack "height")

{-# INLINE hidden_ #-}
hidden_ :: Text -> NodePatch el ctx st
hidden_ = property (T.pack "hidden")

{-# INLINE high_ #-}
high_ :: Text -> NodePatch el ctx st
high_ = property (T.pack "high")

{-# INLINE href_ #-}
href_ :: Text -> NodePatch el ctx st
href_ = property (T.pack "href")

{-# INLINE hreflang_ #-}
hreflang_ :: Text -> NodePatch el ctx st
hreflang_ = property (T.pack "hreflang")

{-# INLINE httpEquiv_ #-}
httpEquiv_ :: Text -> NodePatch el ctx st
httpEquiv_ = property (T.pack "httpEquiv")

{-# INLINE id_ #-}
id_ :: Text -> NodePatch el ctx st
id_ = property (T.pack "id")

{-# INLINE integrity_ #-}
integrity_ :: Text -> NodePatch el ctx st
integrity_ = property (T.pack "integrity")

{-# INLINE ismap_ #-}
ismap_ :: Bool -> NodePatch el ctx st
ismap_ = rawAttribute (T.pack "ismap")

{-# INLINE item_ #-}
item_ :: Text -> NodePatch el ctx st
item_ = property (T.pack "item")

{-# INLINE itemprop_ #-}
itemprop_ :: Text -> NodePatch el ctx st
itemprop_ = attribute (T.pack "itemprop")

{-# INLINE keytype_ #-}
keytype_ :: Text -> NodePatch el ctx st
keytype_ = property (T.pack "keytype")

{-# INLINE lang_ #-}
lang_ :: Text -> NodePatch el ctx st
lang_ = property (T.pack "lang")

{-# INLINE list_ #-}
list_ :: Text -> NodePatch el ctx st
list_ = property (T.pack "list")

{-# INLINE loop_ #-}
loop_ :: Text -> NodePatch el ctx st
loop_ = property (T.pack "loop")

{-# INLINE low_ #-}
low_ :: Text -> NodePatch el ctx st
low_ = property (T.pack "low")

{-# INLINE manifest_ #-}
manifest_ :: Text -> NodePatch el ctx st
manifest_ = property (T.pack "manifest")

{-# INLINE max_ #-}
max_ :: Text -> NodePatch el ctx st
max_ = property (T.pack "max")

{-# INLINE maxlength_ #-}
maxlength_ :: Text -> NodePatch el ctx st
maxlength_ = property (T.pack "maxLength")

{-# INLINE minlength_ #-}
minlength_ :: Text -> NodePatch el ctx st
minlength_ = property (T.pack "minLength")

{-# INLINE media_ #-}
media_ :: Text -> NodePatch el ctx st
media_ = property (T.pack "media")

{-# INLINE method_ #-}
method_ :: Text -> NodePatch el ctx st
method_ = property (T.pack "method")

{-# INLINE min_ #-}
min_ :: Text -> NodePatch el ctx st
min_ = property (T.pack "min")

{-# INLINE multiple_ #-}
multiple_ :: Bool -> NodePatch el ctx st
multiple_ = property (T.pack "multiple")

{-# INLINE name_ #-}
name_ :: Text -> NodePatch el ctx st
name_ = property (T.pack "name")

{-# INLINE novalidate_ #-}
novalidate_ :: Bool -> NodePatch el ctx st
novalidate_ = property (T.pack "noValidate")

{-# INLINE open_ #-}
open_ :: Bool -> NodePatch el ctx st
open_ = property (T.pack "open")

{-# INLINE optimum_ #-}
optimum_ :: Text -> NodePatch el ctx st
optimum_ = property (T.pack "optimum")

{-# INLINE pattern_ #-}
pattern_ :: Text -> NodePatch el ctx st
pattern_ = property (T.pack "pattern")

{-# INLINE ping_ #-}
ping_ :: Text -> NodePatch el ctx st
ping_ = property (T.pack "ping")

{-# INLINE placeholder_ #-}
placeholder_ :: Text -> NodePatch el ctx st
placeholder_ = property (T.pack "placeholder")

{-# INLINE preload_ #-}
preload_ :: Text -> NodePatch el ctx st
preload_ = property (T.pack "preload")

{-# INLINE readonly_ #-}
readonly_ :: Bool -> NodePatch el ctx st
readonly_ = property (T.pack "readonly")

{-# INLINE rel_ #-}
rel_ :: Text -> NodePatch el ctx st
rel_ = property (T.pack "rel")

{-# INLINE required_ #-}
required_ :: Bool -> NodePatch el ctx st
required_ = property (T.pack "required")

{-# INLINE reversed_ #-}
reversed_ :: Text -> NodePatch el ctx st
reversed_ = property (T.pack "reversed")

{-# INLINE role_ #-}
role_ :: Text -> NodePatch el ctx st
role_ = attribute (T.pack "role")

{-# INLINE rows_ #-}
rows_ :: Text -> NodePatch el ctx st
rows_ = property (T.pack "rows")

{-# INLINE rowspan_ #-}
rowspan_ :: Text -> NodePatch el ctx st
rowspan_ = property (T.pack "rowSpan")

{-# INLINE sandbox_ #-}
sandbox_ :: Text -> NodePatch el ctx st
sandbox_ = property (T.pack "sandbox")

{-# INLINE scope_ #-}
scope_ :: Text -> NodePatch el ctx st
scope_ = property (T.pack "scope")

{-# INLINE selected_ #-}
selected_ :: Bool -> NodePatch el ctx st
selected_ = property (T.pack "selected")

{-# INLINE shape_ #-}
shape_ :: Text -> NodePatch el ctx st
shape_ = property (T.pack "shape")

{-# INLINE size_ #-}
size_ :: Text -> NodePatch el ctx st
size_ = property (T.pack "size")

{-# INLINE sizes_ #-}
sizes_ :: Text -> NodePatch el ctx st
sizes_ = property (T.pack "sizes")

{-# INLINE spellcheck_ #-}
spellcheck_ :: Text -> NodePatch el ctx st
spellcheck_ = property (T.pack "spellcheck")

{-# INLINE src_ #-}
src_ :: Text -> NodePatch el ctx st
src_ = property (T.pack "src")

{-# INLINE srcdoc_ #-}
srcdoc_ :: Text -> NodePatch el ctx st
srcdoc_ = property (T.pack "srcdoc")

{-# INLINE start_ #-}
start_ :: Text -> NodePatch el ctx st
start_ = property (T.pack "start")

{-# INLINE step_ #-}
step_ :: Text -> NodePatch el ctx st
step_ = property (T.pack "step")

{-# INLINE tabindex_ #-}
tabindex_ :: Text -> NodePatch el ctx st
tabindex_ = property (T.pack "tabIndex")

{-# INLINE target_ #-}
target_ :: Text -> NodePatch el ctx st
target_ = property (T.pack "target")

{-# INLINE type_ #-}
type_ :: Text -> NodePatch el ctx st
type_ = property (T.pack "type")

{-# INLINE usemap_ #-}
usemap_ :: Text -> NodePatch el ctx st
usemap_ = property (T.pack "useMap")

{-# INLINE value_ #-}
value_ :: Text -> NodePatch el ctx st
value_ = property (T.pack "value")

{-# INLINE width_ #-}
width_ :: Text -> NodePatch el ctx st
width_ = property (T.pack "width")

{-# INLINE wrap_ #-}
wrap_ :: Text -> NodePatch el ctx st
wrap_ = property (T.pack "wrap")

{-# INLINE formAttr_ #-}
formAttr_ :: Text -> NodePatch el ctx st
formAttr_ = property (T.pack "form")

{-# INLINE labelAttr_ #-}
labelAttr_ :: Text -> NodePatch el ctx st
labelAttr_ = property (T.pack "label")

{-# INLINE spanAttr_ #-}
spanAttr_ :: Text -> NodePatch el ctx st
spanAttr_ = property (T.pack "span")

{-# INLINE citeAttr_ #-}
citeAttr_ :: Text -> NodePatch el ctx st
citeAttr_ = property (T.pack "cite")

{-# INLINE summaryAttr_ #-}
summaryAttr_ :: Text -> NodePatch el ctx st
summaryAttr_ = property (T.pack "summary")
