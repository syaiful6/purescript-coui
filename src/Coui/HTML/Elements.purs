module Coui.HTML.Elements
  ( Node
  , Leaf
  , NoninteractiveNode
  , NoninteractiveLeaf
  , element
  , keyed
  , withKeys, withKeys_
  , a, a_
  , abbr, abbr_
  , address, address_
  , area
  , article, article_
  , aside, aside_
  , audio, audio_
  , b, b_
  , base
  , bdi, bdi_
  , bdo, bdo_
  , blockquote, blockquote_
  , body, body_
  , br, br_
  , button, button_
  , canvas
  , caption, caption_
  , cite, cite_
  , code, code_
  , col
  , colgroup, colgroup_
  , command
  , datalist, datalist_
  , dd, dd_
  , del, del_
  , details, details_
  , dfn, dfn_
  , dialog, dialog_
  , div, div_
  , dl, dl_
  , dt, dt_
  , em, em_
  , embed, embed_
  , fieldset, fieldset_
  , figcaption, figcaption_
  , figure, figure_
  , footer, footer_
  , form, form_
  , h1, h1_
  , h2, h2_
  , h3, h3_
  , h4, h4_
  , h5, h5_
  , h6, h6_
  , head, head_
  , header, header_
  , hr, hr_
  , html, html_
  , i, i_
  , iframe
  , img
  , input
  , ins, ins_
  , kbd, kbd_
  , keygen
  , label, label_
  , legend, legend_
  , li, li_
  , link
  , main, main_
  , map, map_
  , mark, mark_
  , menu, menu_
  , menuitem, menuitem_
  , meta
  , meter, meter_
  , nav, nav_
  , noscript, noscript_
  , object, object_
  , ol, ol_
  , optgroup, optgroup_
  , option, option_
  , output, output_
  , p, p_
  , param
  , pre, pre_
  , progress, progress_
  , q, q_
  , rp, rp_
  , rt, rt_
  , ruby, ruby_
  , samp, samp_
  , script, script_
  , section, section_
  , select, select_
  , small, small_
  , source
  , span, span_
  , strong, strong_
  , style, style_
  , sub, sub_
  , summary, summary_
  , sup, sup_
  , table, table_
  , tbody, tbody_
  , td, td_
  , textarea
  , tfoot, tfoot_
  , th, th_
  , thead, thead_
  , time, time_
  , title, title_
  , tr, tr_
  , track
  , u, u_
  , ul, ul_
  , var, var_
  , video, video_
  , wbr
  ) where

import Prelude (Unit)
import Data.Tuple (Tuple)

import Halogen.VDom as VDom

import Unsafe.Coerce (unsafeCoerce)

import Coui.Action.InputF (InputF)
import Coui.HTML.Core (HTML(..), Prop, ElemName(..))
import Coui.HTML.Core as Core
import Coui.HTML.Properties (I, IProp, GlobalProperties, InteractiveEvents)


-- | An HTML element that admits children.
type Node r i
   = Array (IProp (InteractiveEvents (GlobalProperties r)) i)
  -> Array (HTML i)
  -> HTML i

-- | A `Node` that doesn't support mouse events.
type NoninteractiveNode r i
   = Array (IProp (GlobalProperties r) i)
  -> Array (HTML i)
  -> HTML i

-- | An HTML element that does not admit children.
type Leaf r i
   = Array (IProp (InteractiveEvents (GlobalProperties r)) i)
  -> HTML i

-- | An `Leaf` that doesn't support mouse events.
type NoninteractiveLeaf r i
   = Array (IProp (GlobalProperties r) i)
  -> HTML i

-- | Creates an HTML element that expects indexed properties.
element :: forall r i. ElemName -> Array (IProp r i) -> Array (HTML i) -> HTML i
element = (unsafeCoerce :: (ElemName -> Array (Prop i) -> Array (HTML i) -> HTML i) -> ElemName -> Array (IProp r i) -> Array (HTML i) -> HTML i) Core.element

-- | Creates an HTML element that expects indexed properties, with keyed
-- | children.
keyed :: forall r i. ElemName -> Array (IProp r i) -> Array (Tuple String (HTML i)) -> HTML i
keyed = (unsafeCoerce :: (ElemName -> Array (Prop i) -> Array (Tuple String (HTML i)) -> HTML i) -> ElemName -> Array (IProp r i) -> Array (Tuple String (HTML i)) -> HTML i) Core.keyed

withKeys :: forall r i. (Array (IProp r i) -> Array (HTML i) -> HTML i) -> Array (IProp r i) -> Array (Tuple String (HTML i)) -> HTML i
withKeys ctor props children =
  case ctor props [] of
    HTML (VDom.Elem spec _) -> HTML (VDom.Keyed spec (coe children))
    h -> h
  where
  coe :: forall p. Array (Tuple String (HTML i)) -> Array (Tuple String (VDom.VDom (Array (Prop (InputF Unit i))) p))
  coe = unsafeCoerce

withKeys_ :: forall i. (Array (HTML i) -> HTML i) -> Array (Tuple String (HTML i)) -> HTML i
withKeys_ ctor children =
  case ctor [] of
    HTML (VDom.Elem spec _) -> HTML (VDom.Keyed spec (coe children))
    h -> h
  where
  coe :: forall p. Array (Tuple String (HTML i)) -> Array (Tuple String (VDom.VDom (Array (Prop (InputF Unit i))) p))
  coe = unsafeCoerce

a :: forall i. Node (download :: I, href :: I, hreflang :: I, mediate :: I, rel :: I, target :: I, mediaType :: I) i
a = element (ElemName "a")

a_ :: forall i. Array (HTML i) -> HTML i
a_ = a []

abbr :: forall i. Node () i
abbr = element (ElemName "abbr")

abbr_ :: forall i. Array (HTML i) -> HTML i
abbr_ = abbr []

address :: forall i. Node (onScroll :: I) i
address = element (ElemName "address")

address_ :: forall i. Array (HTML i) -> HTML i
address_ = address []

area :: forall i. Leaf (coords :: I, download :: I, href :: I, hreflang :: I, media :: I, rel :: I, shape :: I, target :: I, mediaType :: I) i
area props = element (ElemName "area") props []

article :: forall i. Node () i
article = element (ElemName "article")

article_ :: forall i. Array (HTML i) -> HTML i
article_ = article []

aside :: forall i. Node () i
aside = element (ElemName "aside")

aside_ :: forall i. Array (HTML i) -> HTML i
aside_ = aside []

audio :: forall i. Node (autoplay :: I, controls :: I, loop :: I, muted :: I, preload :: I, src :: I) i
audio = element (ElemName "audio")

audio_ :: forall i. Array (HTML i) -> HTML i
audio_ = audio []

b :: forall i. Node () i
b = element (ElemName "b")

b_ :: forall i. Array (HTML i) -> HTML i
b_ = b []

base :: forall i. NoninteractiveLeaf (href :: I, target :: I) i
base props = element (ElemName "base") props []

bdi :: forall i. Node () i
bdi = element (ElemName "bdi")

bdi_ :: forall i. Array (HTML i) -> HTML i
bdi_ = bdi []

bdo :: forall i. NoninteractiveNode () i
bdo = element (ElemName "bdo")

bdo_ :: forall i. Array (HTML i) -> HTML i
bdo_ = bdo []

blockquote :: forall i. Node (cite :: I, onScroll :: I) i
blockquote = element (ElemName "blockquote")

blockquote_ :: forall i. Array (HTML i) -> HTML i
blockquote_ = blockquote []

body :: forall i. Node (onBeforeUnload :: I, onHashChange :: I, onLoad :: I, onPageShow :: I, onPageHide :: I, onResize :: I, onScroll :: I, onUnload :: I) i
body = element (ElemName "body")

body_ :: forall i. Array (HTML i) -> HTML i
body_ = body []

br :: forall i. NoninteractiveLeaf () i
br props = element (ElemName "br") props []

br_ :: forall i. HTML i
br_ = br []

button :: forall i. Node (autofocus :: I, disabled :: I, form :: I, formaction :: I, formenctyp :: I, formmethod :: I, formnovalidate :: I, formtaget :: I, buttonType :: I, name :: I, value :: I) i
button = element (ElemName "button")

button_ :: forall i. Array (HTML i) -> HTML i
button_ = button []

canvas :: forall i. Leaf (width :: I, height :: I) i
canvas props = element (ElemName "canvas") props []

caption :: forall i. Node (align :: I, onScroll :: I) i
caption = element (ElemName "caption")

caption_ :: forall i. Array (HTML i) -> HTML i
caption_ = caption []

cite :: forall i. Node () i
cite = element (ElemName "cite")

cite_ :: forall i. Array (HTML i) -> HTML i
cite_ = cite []

code :: forall i. Node () i
code = element (ElemName "code")

code_ :: forall i. Array (HTML i) -> HTML i
code_ = code []

col :: forall i. Leaf () i
col props = element (ElemName "col") props []

colgroup :: forall i. Node (span :: I) i
colgroup = element (ElemName "colgroup")

colgroup_ :: forall i. Array (HTML i) -> HTML i
colgroup_ = colgroup []

command :: forall i. Leaf () i
command props = element (ElemName "command") props []

datalist :: forall i. Node () i
datalist = element (ElemName "datalist")

datalist_ :: forall i. Array (HTML i) -> HTML i
datalist_ = datalist []

dd :: forall i. Node (onScroll :: I) i
dd = element (ElemName "dd")

dd_ :: forall i. Array (HTML i) -> HTML i
dd_ = dd []

del :: forall i. Node (cite :: I, datetime :: I) i
del = element (ElemName "del")

del_ :: forall i. Array (HTML i) -> HTML i
del_ = del []

details :: forall i. Node (open :: I) i
details = element (ElemName "details")

details_ :: forall i. Array (HTML i) -> HTML i
details_ = details []

dfn :: forall i. Node () i
dfn = element (ElemName "dfn")

dfn_ :: forall i. Array (HTML i) -> HTML i
dfn_ = dfn []

dialog :: forall i. Node (open :: I) i
dialog = element (ElemName "dialog")

dialog_ :: forall i. Array (HTML i) -> HTML i
dialog_ = dialog []

div :: forall i. Node (onScroll :: I) i
div = element (ElemName "div")

div_ :: forall i. Array (HTML i) -> HTML i
div_ = div []

dl :: forall i. Node (onScroll :: I) i
dl = element (ElemName "dl")

dl_ :: forall i. Array (HTML i) -> HTML i
dl_ = dl []

dt :: forall i. Node (onScroll :: I) i
dt = element (ElemName "dt")

dt_ :: forall i. Array (HTML i) -> HTML i
dt_ = dt []

em :: forall i. Node () i
em = element (ElemName "em")

em_ :: forall i. Array (HTML i) -> HTML i
em_ = em []

embed :: forall i. Node (height :: I, src :: I, mediaType :: I, width :: I) i
embed = element (ElemName "embed")

embed_ :: forall i. Array (HTML i) -> HTML i
embed_ = embed []

fieldset :: forall i. Node (disabled :: I, form :: I, onScroll :: I, name :: I) i
fieldset = element (ElemName "fieldset")

fieldset_ :: forall i. Array (HTML i) -> HTML i
fieldset_ = fieldset []

figcaption :: forall i. Node () i
figcaption = element (ElemName "figcaption")

figcaption_ :: forall i. Array (HTML i) -> HTML i
figcaption_ = figcaption []

figure :: forall i. Node () i
figure = element (ElemName "figure")

figure_ :: forall i. Array (HTML i) -> HTML i
figure_ = figure []

footer :: forall i. Node () i
footer = element (ElemName "footer")

footer_ :: forall i. Array (HTML i) -> HTML i
footer_ = footer []

form :: forall i. Node (acceptCharset :: I, action :: I, autocomplete :: I, enctype :: I, method :: I, onReset :: I, novalidate :: I, onScroll :: I, onSubmit :: I, target :: I, name :: I) i
form = element (ElemName "form")

form_ :: forall i. Array (HTML i) -> HTML i
form_ = form []

h1 :: forall i. Node (onScroll :: I) i
h1 = element (ElemName "h1")

h1_ :: forall i. Array (HTML i) -> HTML i
h1_ = h1 []

h2 :: forall i. Node (onScroll :: I) i
h2 = element (ElemName "h2")

h2_ :: forall i. Array (HTML i) -> HTML i
h2_ = h1 []

h3 :: forall i. Node (onScroll :: I) i
h3 = element (ElemName "h3")

h3_ :: forall i. Array (HTML i) -> HTML i
h3_ = h1 []

h4 :: forall i. Node (onScroll :: I) i
h4 = element (ElemName "h4")

h4_ :: forall i. Array (HTML i) -> HTML i
h4_ = h1 []

h5 :: forall i. Node (onScroll :: I) i
h5 = element (ElemName "h5")

h5_ :: forall i. Array (HTML i) -> HTML i
h5_ = h1 []

h6 :: forall i. Node (onScroll :: I) i
h6 = element (ElemName "h6")

h6_ :: forall i. Array (HTML i) -> HTML i
h6_ = h1 []

head :: forall i. NoninteractiveNode () i
head = element (ElemName "head")

head_ :: forall i. Array (HTML i) -> HTML i
head_ = head []

header :: forall i. Node () i
header = element (ElemName "header")

header_ :: forall i. Array (HTML i) -> HTML i
header_ = header []

hr :: forall i. Leaf () i
hr props = element (ElemName "hr") props []

hr_ :: forall i. HTML i
hr_ = hr []

html :: forall i. NoninteractiveNode (manifest :: I, xmlns :: I, onScroll :: I) i
html = element (ElemName "html")

html_ :: forall i. Array (HTML i) -> HTML i
html_ = html []

i :: forall i. Node () i
i = element (ElemName "i")

i_ :: forall i. Array (HTML i) -> HTML i
i_ = i []

iframe :: forall i. NoninteractiveLeaf (onLoad :: I, sandbox :: I, scrolling :: I, src :: I, srcdoc :: I, width :: I, height :: I, name :: I) i
iframe props = element (ElemName "iframe") props []

img :: forall i. Leaf (alt :: I, crossorigin :: I, height :: I, ismap :: I, longdesc :: I, onAbort :: I, onError :: I, onLoad :: I, src :: I, usemap :: I, width :: I) i
img props = element (ElemName "img") props []

input :: forall i. Leaf (accept :: I, autocomplete :: I, autofocus :: I, checked :: I, disabled :: I, form :: I, formaction :: I, formenctype :: I, formmethod :: I, formnovalidate :: I, formtarget :: I, height :: I, list :: I, max :: I, min :: I, multiple :: I, onAbort :: I, onChange :: I, onError :: I, onInput :: I, onInvalid :: I, onLoad :: I, onSearch :: I, onSelect :: I, pattern :: I, placeholder :: I, readonly :: I, required :: I, size :: I, src :: I, step :: I, inputType :: I, value :: I, width :: I, name :: I) i
input props = element (ElemName "input") props []

ins :: forall i. Node (cite :: I, datetime :: I) i
ins = element (ElemName "ins")

ins_ :: forall i. Array (HTML i) -> HTML i
ins_ = ins []

kbd :: forall i. Node () i
kbd = element (ElemName "kbd")

kbd_ :: forall i. Array (HTML i) -> HTML i
kbd_ = kbd []

keygen :: forall i. Leaf (autofocus :: I, challenge :: I, disabled :: I, form :: I, keytype :: I, onChange :: I, onReset :: I, onSelect :: I, onSubmit :: I, name :: I) i
keygen props = element (ElemName "keygen") props []

label :: forall i. Node (for :: I, form :: I) i
label = element (ElemName "label")

label_ :: forall i. Array (HTML i) -> HTML i
label_ = label []

legend :: forall i. Node () i
legend = element (ElemName "legend")

legend_ :: forall i. Array (HTML i) -> HTML i
legend_ = legend []

li :: forall i. Node (value :: I, onScroll :: I) i
li = element (ElemName "li")

li_ :: forall i. Array (HTML i) -> HTML i
li_ = li []

link :: forall i. Leaf (crossorigin :: I, href :: I, hreflang :: I, media :: I, onLoad :: I, rel :: I, sizes :: I, mediaType :: I) i
link props = element (ElemName "link") props []

main :: forall i. Node () i
main = element (ElemName "main")

main_ :: forall i. Array (HTML i) -> HTML i
main_ = main []

map :: forall i. Node (name :: I) i
map = element (ElemName "map")

map_ :: forall i. Array (HTML i) -> HTML i
map_ = map []

mark :: forall i. Node () i
mark = element (ElemName "mark")

mark_ :: forall i. Array (HTML i) -> HTML i
mark_ = mark []

menu :: forall i. Node (label :: I, onScroll :: I, menuType :: I) i
menu = element (ElemName "menu")

menu_ :: forall i. Array (HTML i) -> HTML i
menu_ = menu []

menuitem :: forall i. Node (checked :: I, command :: I, default :: I, disabled :: I, icon :: I, label :: I, radiogroup :: I, menuitemType :: I) i
menuitem = element (ElemName "menuitem")

menuitem_ :: forall i. Array (HTML i) -> HTML i
menuitem_ = menuitem []

meta :: forall i. NoninteractiveLeaf (charset :: I, content :: I, httpEquiv :: I, name :: I) i
meta props = element (ElemName "meta") props []

meter :: forall i. Node (form :: I, high :: I, low :: I, max :: I, min :: I, optimum :: I, value :: I) i
meter = element (ElemName "meter")

meter_ :: forall i. Array (HTML i) -> HTML i
meter_ = meter []

nav :: forall i. Node () i
nav = element (ElemName "nav")

nav_ :: forall i. Array (HTML i) -> HTML i
nav_ = nav []

noscript :: forall i. Node () i
noscript = element (ElemName "noscript")

noscript_ :: forall i. Array (HTML i) -> HTML i
noscript_ = noscript []

object :: forall i. Node (data :: I, form :: I, height :: I, onError :: I, onScroll :: I, mediaType :: I, usemap :: I, width :: I, name :: I) i
object = element (ElemName "object")

object_ :: forall i. Array (HTML i) -> HTML i
object_ = object []

ol :: forall i. Node (onScroll :: I, reversed :: I, start :: I, olType :: I) i
ol = element (ElemName "ol")

ol_ :: forall i. Array (HTML i) -> HTML i
ol_ = ol []

optgroup :: forall i. Node (disabled :: I, label :: I) i
optgroup = element (ElemName "optgroup")

optgroup_ :: forall i. Array (HTML i) -> HTML i
optgroup_ = optgroup []

option :: forall i. Node (disabled :: I, label :: I, selected :: I, value :: I) i
option = element (ElemName "option")

option_ :: forall i. Array (HTML i) -> HTML i
option_ = option []

output :: forall i. Node (for :: I, form :: I, name :: I) i
output = element (ElemName "output")

output_ :: forall i. Array (HTML i) -> HTML i
output_ = output []

p :: forall i. Node (onScroll :: I) i
p = element (ElemName "p")

p_ :: forall i. Array (HTML i) -> HTML i
p_ = p []

param :: forall i. NoninteractiveLeaf (value :: I, name :: I) i
param props = element (ElemName "param") props []

pre :: forall i. Node (onScroll :: I) i
pre = element (ElemName "pre")

pre_ :: forall i. Array (HTML i) -> HTML i
pre_ = pre []

progress :: forall i. Node (max :: I, value :: I) i
progress = element (ElemName "progress")

progress_ :: forall i. Array (HTML i) -> HTML i
progress_ = progress []

q :: forall i. Node (cite :: I) i
q = element (ElemName "q")

q_ :: forall i. Array (HTML i) -> HTML i
q_ = q []

rp :: forall i. Node () i
rp = element (ElemName "rp")

rp_ :: forall i. Array (HTML i) -> HTML i
rp_ = rp []

rt :: forall i. Node () i
rt = element (ElemName "rt")

rt_ :: forall i. Array (HTML i) -> HTML i
rt_ = rt []

ruby :: forall i. Node () i
ruby = element (ElemName "ruby")

ruby_ :: forall i. Array (HTML i) -> HTML i
ruby_ = ruby []

samp :: forall i. Node () i
samp = element (ElemName "samp")

samp_ :: forall i. Array (HTML i) -> HTML i
samp_ = samp []

script :: forall i. NoninteractiveNode (async :: I, charset :: I, defer :: I, onError :: I, onLoad :: I, src :: I, mediaType :: I) i
script = element (ElemName "script")

script_ :: forall i. Array (HTML i) -> HTML i
script_ = script []

section :: forall i. Node () i
section = element (ElemName "section")

section_ :: forall i. Array (HTML i) -> HTML i
section_ = section []

select :: forall i. Node (autofocus :: I, disabled :: I, form :: I, multiple :: I, onChange :: I, onScroll :: I, required :: I, size :: I, value :: I, selectedIndex :: I, name :: I) i
select = element (ElemName "select")

select_ :: forall i. Array (HTML i) -> HTML i
select_ = select []

small :: forall i. Node () i
small = element (ElemName "small")

small_ :: forall i. Array (HTML i) -> HTML i
small_ = small []

source :: forall i. Leaf (media :: I, src :: I, mediaType :: I) i
source props = element (ElemName "source") props []

span :: forall i. Node () i
span = element (ElemName "span")

span_ :: forall i. Array (HTML i) -> HTML i
span_ = span []

strong :: forall i. Node () i
strong = element (ElemName "strong")

strong_ :: forall i. Array (HTML i) -> HTML i
strong_ = strong []

style :: forall i. NoninteractiveNode (media :: I, onError :: I, onLoad :: I, scoped :: I, mediaType :: I) i
style = element (ElemName "style")

style_ :: forall i. Array (HTML i) -> HTML i
style_ = style []

sub :: forall i. Node () i
sub = element (ElemName "sub")

sub_ :: forall i. Array (HTML i) -> HTML i
sub_ = sub []

summary :: forall i. Node () i
summary = element (ElemName "summary")

summary_ :: forall i. Array (HTML i) -> HTML i
summary_ = summary []

sup :: forall i. Node () i
sup = element (ElemName "sup")

sup_ :: forall i. Array (HTML i) -> HTML i
sup_ = sup []

table :: forall i. Node (sortable :: I) i
table = element (ElemName "table")

table_ :: forall i. Array (HTML i) -> HTML i
table_ = table []

tbody :: forall i. Node (onScroll :: I) i
tbody = element (ElemName "tbody")

tbody_ :: forall i. Array (HTML i) -> HTML i
tbody_ = tbody []

td :: forall i. Node (colSpan :: I, headers :: I, rowSpan :: I) i
td = element (ElemName "td")

td_ :: forall i. Array (HTML i) -> HTML i
td_ = td []

textarea :: forall i. Leaf (autofocus :: I, cols :: I, disabled :: I, form :: I, maxlength :: I, onChange :: I, onInput :: I, onScroll :: I, onSelect :: I, placeholder :: I, readonly :: I, required :: I, rows :: I, value :: I, wrap :: I, name :: I) i
textarea es = element (ElemName "textarea") es []

tfoot :: forall i. Node (onScroll :: I) i
tfoot = element (ElemName "tfoot")

tfoot_ :: forall i. Array (HTML i) -> HTML i
tfoot_ = tfoot []

th :: forall i. Node (abbr :: I, colSpan :: I, headers :: I, rowSpan :: I, scope :: I, sorted :: I) i
th = element (ElemName "th")

th_ :: forall i. Array (HTML i) -> HTML i
th_ = th []

thead :: forall i. Node () i
thead = element (ElemName "thead")

thead_ :: forall i. Array (HTML i) -> HTML i
thead_ = thead []

time :: forall i. Node (datetime :: I) i
time = element (ElemName "time")

time_ :: forall i. Array (HTML i) -> HTML i
time_ = time []

title :: forall i. NoninteractiveNode () i
title = element (ElemName "title")

title_ :: forall i. Array (HTML i) -> HTML i
title_ = title []

tr :: forall i. Node () i
tr = element (ElemName "tr")

tr_ :: forall i. Array (HTML i) -> HTML i
tr_ = tr []

track :: forall i. Leaf (default :: I, kind :: I, label :: I, src :: I, srclang :: I) i
track props = element (ElemName "track") props []

u :: forall i. Node () i
u = element (ElemName "u")

u_ :: forall i. Array (HTML i) -> HTML i
u_ = u []

ul :: forall i. Node (onScroll :: I) i
ul = element (ElemName "ul")

ul_ :: forall i. Array (HTML i) -> HTML i
ul_ = ul []

var :: forall i. Node () i
var = element (ElemName "var")

var_ :: forall i. Array (HTML i) -> HTML i
var_ = var []

video :: forall i. Node (autoplay :: I, controls :: I, height :: I, loop :: I, muted :: I, poster :: I, preload :: I, src :: I, width :: I) i
video = element (ElemName "video")

video_ :: forall i. Array (HTML i) -> HTML i
video_ = video []

wbr :: forall i. Leaf () i
wbr props = element (ElemName "wbr") props []
