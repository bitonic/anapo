module Anapo.Component.Elements where
    
import Anapo.Component.Internal (el, NodePatch, Node, IsElementChildren)
import qualified GHCJS.DOM.Types as DOM
import qualified Anapo.Text as T

a_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLAnchorElement ctx st] -> a -> Node ctx st
a_ = el (T.pack "a") DOM.HTMLAnchorElement

abbr_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
abbr_ = el (T.pack "abbr") DOM.HTMLElement

address_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
address_ = el (T.pack "address") DOM.HTMLElement

area_ :: [NodePatch DOM.HTMLAreaElement ctx st] -> Node ctx st
area_ x = el (T.pack "area") DOM.HTMLAreaElement x ()

article_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
article_ = el (T.pack "article") DOM.HTMLElement

aside_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
aside_ = el (T.pack "aside") DOM.HTMLElement

audio_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLAudioElement ctx st] -> a -> Node ctx st
audio_ = el (T.pack "audio") DOM.HTMLAudioElement

b_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
b_ = el (T.pack "b") DOM.HTMLElement

base_ :: [NodePatch DOM.HTMLBaseElement ctx st] -> Node ctx st
base_ x = el (T.pack "base") DOM.HTMLBaseElement x ()

bdo_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
bdo_ = el (T.pack "bdo") DOM.HTMLElement

blockquote_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLQuoteElement ctx st] -> a -> Node ctx st
blockquote_ = el (T.pack "blockquote") DOM.HTMLQuoteElement

body_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLBodyElement ctx st] -> a -> Node ctx st
body_ = el (T.pack "body") DOM.HTMLBodyElement

br_ :: [NodePatch DOM.HTMLBRElement ctx st] -> Node ctx st
br_ x = el (T.pack "br") DOM.HTMLBRElement x ()

button_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLButtonElement ctx st] -> a -> Node ctx st
button_ = el (T.pack "button") DOM.HTMLButtonElement

canvas_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLCanvasElement ctx st] -> a -> Node ctx st
canvas_ = el (T.pack "canvas") DOM.HTMLCanvasElement

caption_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableCaptionElement ctx st] -> a -> Node ctx st
caption_ = el (T.pack "caption") DOM.HTMLTableCaptionElement

cite_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
cite_ = el (T.pack "cite") DOM.HTMLElement

code_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
code_ = el (T.pack "code") DOM.HTMLElement

col_ :: [NodePatch DOM.HTMLTableColElement ctx st] -> Node ctx st
col_ x = el (T.pack "col") DOM.HTMLTableColElement x ()

colgroup_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableColElement ctx st] -> a -> Node ctx st
colgroup_ = el (T.pack "colgroup") DOM.HTMLTableColElement

datalist_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLDataListElement ctx st] -> a -> Node ctx st
datalist_ = el (T.pack "datalist") DOM.HTMLDataListElement

dd_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
dd_ = el (T.pack "dd") DOM.HTMLElement

del_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLModElement ctx st] -> a -> Node ctx st
del_ = el (T.pack "del") DOM.HTMLModElement

details_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLDetailsElement ctx st] -> a -> Node ctx st
details_ = el (T.pack "details") DOM.HTMLDetailsElement

dfn_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
dfn_ = el (T.pack "dfn") DOM.HTMLElement

div_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLDivElement ctx st] -> a -> Node ctx st
div_ = el (T.pack "div") DOM.HTMLDivElement

dl_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLDListElement ctx st] -> a -> Node ctx st
dl_ = el (T.pack "dl") DOM.HTMLDListElement

dt_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
dt_ = el (T.pack "dt") DOM.HTMLElement

em_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
em_ = el (T.pack "em") DOM.HTMLElement

embed_ :: [NodePatch DOM.HTMLEmbedElement ctx st] -> Node ctx st
embed_ x = el (T.pack "embed") DOM.HTMLEmbedElement x ()

fieldset_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLFieldSetElement ctx st] -> a -> Node ctx st
fieldset_ = el (T.pack "fieldset") DOM.HTMLFieldSetElement

figcaption_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
figcaption_ = el (T.pack "figcaption") DOM.HTMLElement

figure_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
figure_ = el (T.pack "figure") DOM.HTMLElement

footer_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
footer_ = el (T.pack "footer") DOM.HTMLElement

form_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLFormElement ctx st] -> a -> Node ctx st
form_ = el (T.pack "form") DOM.HTMLFormElement

h1_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHeadingElement ctx st] -> a -> Node ctx st
h1_ = el (T.pack "h1") DOM.HTMLHeadingElement

h2_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHeadingElement ctx st] -> a -> Node ctx st
h2_ = el (T.pack "h2") DOM.HTMLHeadingElement

h3_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHeadingElement ctx st] -> a -> Node ctx st
h3_ = el (T.pack "h3") DOM.HTMLHeadingElement

h4_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHeadingElement ctx st] -> a -> Node ctx st
h4_ = el (T.pack "h4") DOM.HTMLHeadingElement

h5_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHeadingElement ctx st] -> a -> Node ctx st
h5_ = el (T.pack "h5") DOM.HTMLHeadingElement

h6_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHeadingElement ctx st] -> a -> Node ctx st
h6_ = el (T.pack "h6") DOM.HTMLHeadingElement

head_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHeadElement ctx st] -> a -> Node ctx st
head_ = el (T.pack "head") DOM.HTMLHeadElement

header_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
header_ = el (T.pack "header") DOM.HTMLElement

hgroup_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
hgroup_ = el (T.pack "hgroup") DOM.HTMLElement

hr_ :: [NodePatch DOM.HTMLHRElement ctx st] -> Node ctx st
hr_ x = el (T.pack "hr") DOM.HTMLHRElement x ()

html_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLHtmlElement ctx st] -> a -> Node ctx st
html_ = el (T.pack "html") DOM.HTMLHtmlElement

i_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
i_ = el (T.pack "i") DOM.HTMLElement

iframe_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLIFrameElement ctx st] -> a -> Node ctx st
iframe_ = el (T.pack "iframe") DOM.HTMLIFrameElement

img_ :: [NodePatch DOM.HTMLImageElement ctx st] -> Node ctx st
img_ x = el (T.pack "img") DOM.HTMLImageElement x ()

input_ :: [NodePatch DOM.HTMLInputElement ctx st] -> Node ctx st
input_ x = el (T.pack "input") DOM.HTMLInputElement x ()

ins_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLModElement ctx st] -> a -> Node ctx st
ins_ = el (T.pack "ins") DOM.HTMLModElement

kbd_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
kbd_ = el (T.pack "kbd") DOM.HTMLElement

keygen_ :: [NodePatch DOM.HTMLKeygenElement ctx st] -> Node ctx st
keygen_ x = el (T.pack "keygen") DOM.HTMLKeygenElement x ()

label_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLLabelElement ctx st] -> a -> Node ctx st
label_ = el (T.pack "label") DOM.HTMLLabelElement

legend_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLLegendElement ctx st] -> a -> Node ctx st
legend_ = el (T.pack "legend") DOM.HTMLLegendElement

li_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLLIElement ctx st] -> a -> Node ctx st
li_ = el (T.pack "li") DOM.HTMLLIElement

link_ :: [NodePatch DOM.HTMLLinkElement ctx st] -> Node ctx st
link_ x = el (T.pack "link") DOM.HTMLLinkElement x ()

map_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLMapElement ctx st] -> a -> Node ctx st
map_ = el (T.pack "map") DOM.HTMLMapElement

main_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
main_ = el (T.pack "main") DOM.HTMLElement

mark_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
mark_ = el (T.pack "mark") DOM.HTMLElement

menu_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLMenuElement ctx st] -> a -> Node ctx st
menu_ = el (T.pack "menu") DOM.HTMLMenuElement

meta_ :: [NodePatch DOM.HTMLMetaElement ctx st] -> Node ctx st
meta_ x = el (T.pack "meta") DOM.HTMLMetaElement x ()

meter_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLMeterElement ctx st] -> a -> Node ctx st
meter_ = el (T.pack "meter") DOM.HTMLMeterElement

nav_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
nav_ = el (T.pack "nav") DOM.HTMLElement

noscript_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
noscript_ = el (T.pack "noscript") DOM.HTMLElement

object_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLObjectElement ctx st] -> a -> Node ctx st
object_ = el (T.pack "object") DOM.HTMLObjectElement

ol_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLOListElement ctx st] -> a -> Node ctx st
ol_ = el (T.pack "ol") DOM.HTMLOListElement

optgroup_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLOptGroupElement ctx st] -> a -> Node ctx st
optgroup_ = el (T.pack "optgroup") DOM.HTMLOptGroupElement

option_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLOptionElement ctx st] -> a -> Node ctx st
option_ = el (T.pack "option") DOM.HTMLOptionElement

output_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLOutputElement ctx st] -> a -> Node ctx st
output_ = el (T.pack "output") DOM.HTMLOutputElement

p_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLParagraphElement ctx st] -> a -> Node ctx st
p_ = el (T.pack "p") DOM.HTMLParagraphElement

param_ :: [NodePatch DOM.HTMLParamElement ctx st] -> Node ctx st
param_ x = el (T.pack "param") DOM.HTMLParamElement x ()

svg_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
svg_ = el (T.pack "svg") DOM.HTMLElement

pre_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLPreElement ctx st] -> a -> Node ctx st
pre_ = el (T.pack "pre") DOM.HTMLPreElement

progress_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLProgressElement ctx st] -> a -> Node ctx st
progress_ = el (T.pack "progress") DOM.HTMLProgressElement

q_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLQuoteElement ctx st] -> a -> Node ctx st
q_ = el (T.pack "q") DOM.HTMLQuoteElement

rp_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
rp_ = el (T.pack "rp") DOM.HTMLElement

rt_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
rt_ = el (T.pack "rt") DOM.HTMLElement

ruby_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
ruby_ = el (T.pack "ruby") DOM.HTMLElement

samp_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
samp_ = el (T.pack "samp") DOM.HTMLElement

script_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLScriptElement ctx st] -> a -> Node ctx st
script_ = el (T.pack "script") DOM.HTMLScriptElement

section_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
section_ = el (T.pack "section") DOM.HTMLElement

select_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLSelectElement ctx st] -> a -> Node ctx st
select_ = el (T.pack "select") DOM.HTMLSelectElement

small_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
small_ = el (T.pack "small") DOM.HTMLElement

source_ :: [NodePatch DOM.HTMLSourceElement ctx st] -> Node ctx st
source_ x = el (T.pack "source") DOM.HTMLSourceElement x ()

span_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLSpanElement ctx st] -> a -> Node ctx st
span_ = el (T.pack "span") DOM.HTMLSpanElement

strong_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
strong_ = el (T.pack "strong") DOM.HTMLElement

style_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLStyleElement ctx st] -> a -> Node ctx st
style_ = el (T.pack "style") DOM.HTMLStyleElement

sub_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
sub_ = el (T.pack "sub") DOM.HTMLElement

summary_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
summary_ = el (T.pack "summary") DOM.HTMLElement

sup_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
sup_ = el (T.pack "sup") DOM.HTMLElement

table_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableElement ctx st] -> a -> Node ctx st
table_ = el (T.pack "table") DOM.HTMLTableElement

tbody_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableSectionElement ctx st] -> a -> Node ctx st
tbody_ = el (T.pack "tbody") DOM.HTMLTableSectionElement

td_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableCellElement ctx st] -> a -> Node ctx st
td_ = el (T.pack "td") DOM.HTMLTableCellElement

textarea_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTextAreaElement ctx st] -> a -> Node ctx st
textarea_ = el (T.pack "textarea") DOM.HTMLTextAreaElement

tfoot_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableSectionElement ctx st] -> a -> Node ctx st
tfoot_ = el (T.pack "tfoot") DOM.HTMLTableSectionElement

th_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableCellElement ctx st] -> a -> Node ctx st
th_ = el (T.pack "th") DOM.HTMLTableCellElement

template_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTemplateElement ctx st] -> a -> Node ctx st
template_ = el (T.pack "template") DOM.HTMLTemplateElement

thead_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableSectionElement ctx st] -> a -> Node ctx st
thead_ = el (T.pack "thead") DOM.HTMLTableSectionElement

time_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTimeElement ctx st] -> a -> Node ctx st
time_ = el (T.pack "time") DOM.HTMLTimeElement

title_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTitleElement ctx st] -> a -> Node ctx st
title_ = el (T.pack "title") DOM.HTMLTitleElement

tr_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLTableRowElement ctx st] -> a -> Node ctx st
tr_ = el (T.pack "tr") DOM.HTMLTableRowElement

track_ :: [NodePatch DOM.HTMLTrackElement ctx st] -> Node ctx st
track_ x = el (T.pack "track") DOM.HTMLTrackElement x ()

ul_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLUListElement ctx st] -> a -> Node ctx st
ul_ = el (T.pack "ul") DOM.HTMLUListElement

var_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLElement ctx st] -> a -> Node ctx st
var_ = el (T.pack "var") DOM.HTMLElement

video_ :: IsElementChildren a ctx st => [NodePatch DOM.HTMLVideoElement ctx st] -> a -> Node ctx st
video_ = el (T.pack "video") DOM.HTMLVideoElement

wbr_ :: [NodePatch DOM.HTMLElement ctx st] -> Node ctx st
wbr_ x = el (T.pack "wbr") DOM.HTMLElement x ()

