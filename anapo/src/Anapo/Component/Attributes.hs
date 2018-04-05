module Anapo.Component.Attributes where

import Anapo.Component.Internal (property, attribute, rawAttribute, NodePatch)
import Anapo.Text (Text)
import qualified Anapo.Text as T
import Data.Monoid ((<>))

accept_ :: Text -> NodePatch el ctx st
accept_ = property (T.pack "accept")

acceptCharset_ :: Text -> NodePatch el ctx st
acceptCharset_ = property (T.pack "acceptCharset")

accesskey_ :: Text -> NodePatch el ctx st
accesskey_ = property (T.pack "accessKey")

action_ :: Text -> NodePatch el ctx st
action_ = property (T.pack "action")

alt_ :: Text -> NodePatch el ctx st
alt_ = property (T.pack "alt")

async_ :: Text -> NodePatch el ctx st
async_ = property (T.pack "async")

autocomplete_ :: Text -> NodePatch el ctx st
autocomplete_ = property (T.pack "autocomplete")

autofocus_ :: Bool -> NodePatch el ctx st
autofocus_ = property (T.pack "autofocus")

autoplay_ :: Text -> NodePatch el ctx st
autoplay_ = property (T.pack "autoplay")

challenge_ :: Text -> NodePatch el ctx st
challenge_ = property (T.pack "challenge")

charset_ :: Text -> NodePatch el ctx st
charset_ = property (T.pack "charset")

checked_ :: Bool -> NodePatch el ctx st
checked_ = property (T.pack "checked")

class_ :: Text -> NodePatch el ctx st
class_ = property (T.pack "className")

classes_ :: [Text] -> NodePatch el ctx st
classes_ = property (T.pack "className") . T.unwords

cols_ :: Text -> NodePatch el ctx st
cols_ = property (T.pack "cols")

colspan_ :: Text -> NodePatch el ctx st
colspan_ = property (T.pack "colSpan")

content_ :: Text -> NodePatch el ctx st
content_ = property (T.pack "content")

contenteditable_ :: Text -> NodePatch el ctx st
contenteditable_ = property (T.pack "contentEditable")

contextmenu_ :: Text -> NodePatch el ctx st
contextmenu_ = attribute (T.pack "contextmenu")

controls_ :: Text -> NodePatch el ctx st
controls_ = property (T.pack "controls")

coords_ :: Text -> NodePatch el ctx st
coords_ = property (T.pack "coords")

crossorigin_ :: Text -> NodePatch el ctx st
crossorigin_ = property (T.pack "crossOrigin")

data_ :: Text -> Text -> NodePatch el ctx st
data_ name = attribute (T.pack "data-" <> name)

datetime_ :: Text -> NodePatch el ctx st
datetime_ = property (T.pack "dateTime")

defer_ :: Text -> NodePatch el ctx st
defer_ = property (T.pack "defer")

dir_ :: Text -> NodePatch el ctx st
dir_ = property (T.pack "dir")

disabled_ :: Text -> NodePatch el ctx st
disabled_ = property (T.pack "disabled")

download_ :: Text -> NodePatch el ctx st
download_ = property (T.pack "download")

draggable_ :: Text -> NodePatch el ctx st
draggable_ = property (T.pack "draggable")

enctype_ :: Text -> NodePatch el ctx st
enctype_ = property (T.pack "enctype")

for_ :: Text -> NodePatch el ctx st
for_ = property (T.pack "htmlFor")

formaction_ :: Text -> NodePatch el ctx st
formaction_ = property (T.pack "formAction")

formenctype_ :: Text -> NodePatch el ctx st
formenctype_ = property (T.pack "formEnctype")

formmethod_ :: Text -> NodePatch el ctx st
formmethod_ = property (T.pack "formMethod")

formnovalidate_ :: Text -> NodePatch el ctx st
formnovalidate_ = property (T.pack "formNoValidate")

formtarget_ :: Text -> NodePatch el ctx st
formtarget_ = property (T.pack "formTarget")

headers_ :: Text -> NodePatch el ctx st
headers_ = property (T.pack "headers")

height_ :: Text -> NodePatch el ctx st
height_ = property (T.pack "height")

hidden_ :: Text -> NodePatch el ctx st
hidden_ = property (T.pack "hidden")

high_ :: Text -> NodePatch el ctx st
high_ = property (T.pack "high")

href_ :: Text -> NodePatch el ctx st
href_ = property (T.pack "href")

hreflang_ :: Text -> NodePatch el ctx st
hreflang_ = property (T.pack "hreflang")

httpEquiv_ :: Text -> NodePatch el ctx st
httpEquiv_ = property (T.pack "httpEquiv")

id_ :: Text -> NodePatch el ctx st
id_ = property (T.pack "id")

integrity_ :: Text -> NodePatch el ctx st
integrity_ = property (T.pack "integrity")

ismap_ :: Bool -> NodePatch el ctx st
ismap_ = rawAttribute (T.pack "ismap")

item_ :: Text -> NodePatch el ctx st
item_ = property (T.pack "item")

itemprop_ :: Text -> NodePatch el ctx st
itemprop_ = attribute (T.pack "itemprop")

keytype_ :: Text -> NodePatch el ctx st
keytype_ = property (T.pack "keytype")

lang_ :: Text -> NodePatch el ctx st
lang_ = property (T.pack "lang")

list_ :: Text -> NodePatch el ctx st
list_ = property (T.pack "list")

loop_ :: Text -> NodePatch el ctx st
loop_ = property (T.pack "loop")

low_ :: Text -> NodePatch el ctx st
low_ = property (T.pack "low")

manifest_ :: Text -> NodePatch el ctx st
manifest_ = property (T.pack "manifest")

max_ :: Text -> NodePatch el ctx st
max_ = property (T.pack "max")

maxlength_ :: Text -> NodePatch el ctx st
maxlength_ = property (T.pack "maxLength")

media_ :: Text -> NodePatch el ctx st
media_ = property (T.pack "media")

method_ :: Text -> NodePatch el ctx st
method_ = property (T.pack "method")

min_ :: Text -> NodePatch el ctx st
min_ = property (T.pack "min")

multiple_ :: Text -> NodePatch el ctx st
multiple_ = property (T.pack "multiple")

name_ :: Text -> NodePatch el ctx st
name_ = property (T.pack "name")

novalidate_ :: Text -> NodePatch el ctx st
novalidate_ = property (T.pack "noValidate")

open_ :: Text -> NodePatch el ctx st
open_ = property (T.pack "open")

optimum_ :: Text -> NodePatch el ctx st
optimum_ = property (T.pack "optimum")

pattern_ :: Text -> NodePatch el ctx st
pattern_ = property (T.pack "pattern")

ping_ :: Text -> NodePatch el ctx st
ping_ = property (T.pack "ping")

placeholder_ :: Text -> NodePatch el ctx st
placeholder_ = property (T.pack "placeholder")

preload_ :: Text -> NodePatch el ctx st
preload_ = property (T.pack "preload")

readonly_ :: Text -> NodePatch el ctx st
readonly_ = property (T.pack "readonly")

rel_ :: Text -> NodePatch el ctx st
rel_ = property (T.pack "rel")

required_ :: Text -> NodePatch el ctx st
required_ = property (T.pack "required")

reversed_ :: Text -> NodePatch el ctx st
reversed_ = property (T.pack "reversed")

role_ :: Text -> NodePatch el ctx st
role_ = attribute (T.pack "role")

rows_ :: Text -> NodePatch el ctx st
rows_ = property (T.pack "rows")

rowspan_ :: Text -> NodePatch el ctx st
rowspan_ = property (T.pack "rowSpan")

sandbox_ :: Text -> NodePatch el ctx st
sandbox_ = property (T.pack "sandbox")

scope_ :: Text -> NodePatch el ctx st
scope_ = property (T.pack "scope")

selected_ :: Text -> NodePatch el ctx st
selected_ = property (T.pack "selected")

shape_ :: Text -> NodePatch el ctx st
shape_ = property (T.pack "shape")

size_ :: Text -> NodePatch el ctx st
size_ = property (T.pack "size")

sizes_ :: Text -> NodePatch el ctx st
sizes_ = property (T.pack "sizes")

spellcheck_ :: Text -> NodePatch el ctx st
spellcheck_ = property (T.pack "spellcheck")

src_ :: Text -> NodePatch el ctx st
src_ = property (T.pack "src")

srcdoc_ :: Text -> NodePatch el ctx st
srcdoc_ = property (T.pack "srcdoc")

start_ :: Text -> NodePatch el ctx st
start_ = property (T.pack "start")

step_ :: Text -> NodePatch el ctx st
step_ = property (T.pack "step")

tabindex_ :: Text -> NodePatch el ctx st
tabindex_ = property (T.pack "tabIndex")

target_ :: Text -> NodePatch el ctx st
target_ = property (T.pack "target")

type_ :: Text -> NodePatch el ctx st
type_ = property (T.pack "type")

usemap_ :: Text -> NodePatch el ctx st
usemap_ = property (T.pack "useMap")

value_ :: Text -> NodePatch el ctx st
value_ = property (T.pack "value")

width_ :: Text -> NodePatch el ctx st
width_ = property (T.pack "width")

wrap_ :: Text -> NodePatch el ctx st
wrap_ = property (T.pack "wrap")

formAttr_ :: Text -> NodePatch el ctx st
formAttr_ = property (T.pack "form")

labelAttr_ :: Text -> NodePatch el ctx st
labelAttr_ = property (T.pack "label")

spanAttr_ :: Text -> NodePatch el ctx st
spanAttr_ = property (T.pack "span")

citeAttr_ :: Text -> NodePatch el ctx st
citeAttr_ = property (T.pack "cite")

summaryAttr_ :: Text -> NodePatch el ctx st
summaryAttr_ = property (T.pack "summary")
