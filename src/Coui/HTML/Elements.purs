module Coui.HTML.Elements
  ( Node
  , Leaf
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

import Data.Tuple (Tuple)

import DOM.HTML.Indexed as I

import Halogen.VDom as VDom

import Unsafe.Coerce (unsafeCoerce)

import Coui.HTML.Core (HTML(..), Prop, ElemName(..))
import Coui.HTML.Core as Core
import Coui.HTML.Properties (IProp)


-- | An HTML element that admits children.
type Node r i
   = Array (IProp r i)
  -> Array (HTML i)
  -> HTML i

-- | An HTML element that does not admit children.
type Leaf r i
   = Array (IProp r i)
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
  coe :: forall p. Array (Tuple String (HTML i)) -> Array (Tuple String (VDom.VDom (Array (Prop i)) p))
  coe = unsafeCoerce

withKeys_ :: forall i. (Array (HTML i) -> HTML i) -> Array (Tuple String (HTML i)) -> HTML i
withKeys_ ctor children =
  case ctor [] of
    HTML (VDom.Elem spec _) -> HTML (VDom.Keyed spec (coe children))
    h -> h
  where
  coe :: forall p. Array (Tuple String (HTML i)) -> Array (Tuple String (VDom.VDom (Array (Prop i)) p))
  coe = unsafeCoerce

a :: forall i. Node I.HTMLa i
a = element (ElemName "a")

a_ :: forall i. Array (HTML i) -> HTML i
a_ = a []

abbr :: forall i. Node I.HTMLabbr i
abbr = element (ElemName "abbr")

abbr_ :: forall i. Array (HTML i) -> HTML i
abbr_ = abbr []

address :: forall i. Node I.HTMLaddress i
address = element (ElemName "address")

address_ :: forall i. Array (HTML i) -> HTML i
address_ = address []

area :: forall i. Leaf I.HTMLarea i
area props = element (ElemName "area") props []

article :: forall i. Node I.HTMLarticle i
article = element (ElemName "article")

article_ :: forall i. Array (HTML i) -> HTML i
article_ = article []

aside :: forall i. Node I.HTMLaside i
aside = element (ElemName "aside")

aside_ :: forall i. Array (HTML i) -> HTML i
aside_ = aside []

audio :: forall i. Node I.HTMLaudio i
audio = element (ElemName "audio")

audio_ :: forall i. Array (HTML i) -> HTML i
audio_ = audio []

b :: forall i. Node I.HTMLb i
b = element (ElemName "b")

b_ :: forall i. Array (HTML i) -> HTML i
b_ = b []

base :: forall i. Leaf I.HTMLbase i
base props = element (ElemName "base") props []

bdi :: forall i. Node I.HTMLbdi i
bdi = element (ElemName "bdi")

bdi_ :: forall i. Array (HTML i) -> HTML i
bdi_ = bdi []

bdo :: forall i. Node I.HTMLbdo i
bdo = element (ElemName "bdo")

bdo_ :: forall i. Array (HTML i) -> HTML i
bdo_ = bdo []

blockquote :: forall i. Node I.HTMLblockquote i
blockquote = element (ElemName "blockquote")

blockquote_ :: forall i. Array (HTML i) -> HTML i
blockquote_ = blockquote []

body :: forall i. Node I.HTMLbody i
body = element (ElemName "body")

body_ :: forall i. Array (HTML i) -> HTML i
body_ = body []

br :: forall i. Leaf I.HTMLbr i
br props = element (ElemName "br") props []

br_ :: forall i. HTML i
br_ = br []

button :: forall i. Node I.HTMLbutton i
button = element (ElemName "button")

button_ :: forall i. Array (HTML i) -> HTML i
button_ = button []

canvas :: forall i. Leaf I.HTMLcanvas i
canvas props = element (ElemName "canvas") props []

caption :: forall i. Node I.HTMLcaption i
caption = element (ElemName "caption")

caption_ :: forall i. Array (HTML i) -> HTML i
caption_ = caption []

cite :: forall i. Node I.HTMLcite i
cite = element (ElemName "cite")

cite_ :: forall i. Array (HTML i) -> HTML i
cite_ = cite []

code :: forall i. Node I.HTMLcode i
code = element (ElemName "code")

code_ :: forall i. Array (HTML i) -> HTML i
code_ = code []

col :: forall i. Leaf I.HTMLcol i
col props = element (ElemName "col") props []

colgroup :: forall i. Node I.HTMLcolgroup i
colgroup = element (ElemName "colgroup")

colgroup_ :: forall i. Array (HTML i) -> HTML i
colgroup_ = colgroup []

command :: forall i. Leaf I.HTMLcommand i
command props = element (ElemName "command") props []

datalist :: forall i. Node I.HTMLdatalist i
datalist = element (ElemName "datalist")

datalist_ :: forall i. Array (HTML i) -> HTML i
datalist_ = datalist []

dd :: forall i. Node I.HTMLdd i
dd = element (ElemName "dd")

dd_ :: forall i. Array (HTML i) -> HTML i
dd_ = dd []

del :: forall i. Node I.HTMLdel i
del = element (ElemName "del")

del_ :: forall i. Array (HTML i) -> HTML i
del_ = del []

details :: forall i. Node I.HTMLdetails i
details = element (ElemName "details")

details_ :: forall i. Array (HTML i) -> HTML i
details_ = details []

dfn :: forall i. Node I.HTMLdfn i
dfn = element (ElemName "dfn")

dfn_ :: forall i. Array (HTML i) -> HTML i
dfn_ = dfn []

dialog :: forall i. Node I.HTMLdialog i
dialog = element (ElemName "dialog")

dialog_ :: forall i. Array (HTML i) -> HTML i
dialog_ = dialog []

div :: forall i. Node I.HTMLdiv i
div = element (ElemName "div")

div_ :: forall i. Array (HTML i) -> HTML i
div_ = div []

dl :: forall i. Node I.HTMLdl i
dl = element (ElemName "dl")

dl_ :: forall i. Array (HTML i) -> HTML i
dl_ = dl []

dt :: forall i. Node (I.HTMLdt) i
dt = element (ElemName "dt")

dt_ :: forall i. Array (HTML i) -> HTML i
dt_ = dt []

em :: forall i. Node I.HTMLem i
em = element (ElemName "em")

em_ :: forall i. Array (HTML i) -> HTML i
em_ = em []

embed :: forall i. Node I.HTMLembed i
embed = element (ElemName "embed")

embed_ :: forall i. Array (HTML i) -> HTML i
embed_ = embed []

fieldset :: forall i. Node I.HTMLfieldset i
fieldset = element (ElemName "fieldset")

fieldset_ :: forall i. Array (HTML i) -> HTML i
fieldset_ = fieldset []

figcaption :: forall i. Node I.HTMLfigcaption i
figcaption = element (ElemName "figcaption")

figcaption_ :: forall i. Array (HTML i) -> HTML i
figcaption_ = figcaption []

figure :: forall i. Node I.HTMLfigure i
figure = element (ElemName "figure")

figure_ :: forall i. Array (HTML i) -> HTML i
figure_ = figure []

footer :: forall i. Node I.HTMLfooter i
footer = element (ElemName "footer")

footer_ :: forall i. Array (HTML i) -> HTML i
footer_ = footer []

form :: forall i. Node I.HTMLform i
form = element (ElemName "form")

form_ :: forall i. Array (HTML i) -> HTML i
form_ = form []

h1 :: forall i. Node I.HTMLh1 i
h1 = element (ElemName "h1")

h1_ :: forall i. Array (HTML i) -> HTML i
h1_ = h1 []

h2 :: forall i. Node I.HTMLh2 i
h2 = element (ElemName "h2")

h2_ :: forall i. Array (HTML i) -> HTML i
h2_ = h2 []

h3 :: forall i. Node I.HTMLh3 i
h3 = element (ElemName "h3")

h3_ :: forall i. Array (HTML i) -> HTML i
h3_ = h3 []

h4 :: forall i. Node I.HTMLh4 i
h4 = element (ElemName "h4")

h4_ :: forall i. Array (HTML i) -> HTML i
h4_ = h4 []

h5 :: forall i. Node I.HTMLh5 i
h5 = element (ElemName "h5")

h5_ :: forall i. Array (HTML i) -> HTML i
h5_ = h5 []

h6 :: forall i. Node I.HTMLh6 i
h6 = element (ElemName "h6")

h6_ :: forall i. Array (HTML i) -> HTML i
h6_ = h6 []

head :: forall i. Node I.HTMLhead i
head = element (ElemName "head")

head_ :: forall i. Array (HTML i) -> HTML i
head_ = head []

header :: forall i. Node I.HTMLheader i
header = element (ElemName "header")

header_ :: forall i. Array (HTML i) -> HTML i
header_ = header []

hr :: forall i. Leaf I.HTMLhr i
hr props = element (ElemName "hr") props []

hr_ :: forall i. HTML i
hr_ = hr []

html :: forall i. Node I.HTMLhtml i
html = element (ElemName "html")

html_ :: forall i. Array (HTML i) -> HTML i
html_ = html []

i :: forall i. Node I.HTMLi i
i = element (ElemName "i")

i_ :: forall i. Array (HTML i) -> HTML i
i_ = i []

iframe :: forall i. Leaf I.HTMLiframe i
iframe props = element (ElemName "iframe") props []

img :: forall i. Leaf I.HTMLimg i
img props = element (ElemName "img") props []

input :: forall i. Leaf I.HTMLinput i
input props = element (ElemName "input") props []

ins :: forall i. Node I.HTMLins i
ins = element (ElemName "ins")

ins_ :: forall i. Array (HTML i) -> HTML i
ins_ = ins []

kbd :: forall i. Node I.HTMLkbd i
kbd = element (ElemName "kbd")

kbd_ :: forall i. Array (HTML i) -> HTML i
kbd_ = kbd []

label :: forall i. Node I.HTMLlabel i
label = element (ElemName "label")

label_ :: forall i. Array (HTML i) -> HTML i
label_ = label []

legend :: forall i. Node I.HTMLlegend i
legend = element (ElemName "legend")

legend_ :: forall i. Array (HTML i) -> HTML i
legend_ = legend []

li :: forall i. Node I.HTMLli i
li = element (ElemName "li")

li_ :: forall i. Array (HTML i) -> HTML i
li_ = li []

link :: forall i. Leaf I.HTMLlink i
link props = element (ElemName "link") props []

main :: forall i. Node I.HTMLmain i
main = element (ElemName "main")

main_ :: forall i. Array (HTML i) -> HTML i
main_ = main []

map :: forall i. Node I.HTMLmap i
map = element (ElemName "map")

map_ :: forall i. Array (HTML i) -> HTML i
map_ = map []

mark :: forall i. Node I.HTMLmark i
mark = element (ElemName "mark")

mark_ :: forall i. Array (HTML i) -> HTML i
mark_ = mark []

menu :: forall i. Node I.HTMLmenu i
menu = element (ElemName "menu")

menu_ :: forall i. Array (HTML i) -> HTML i
menu_ = menu []

menuitem :: forall i. Node I.HTMLmenuitem i
menuitem = element (ElemName "menuitem")

menuitem_ :: forall i. Array (HTML i) -> HTML i
menuitem_ = menuitem []

meta :: forall i. Leaf I.HTMLmeta i
meta props = element (ElemName "meta") props []

meter :: forall i. Node I.HTMLmeter i
meter = element (ElemName "meter")

meter_ :: forall i. Array (HTML i) -> HTML i
meter_ = meter []

nav :: forall i. Node I.HTMLnav i
nav = element (ElemName "nav")

nav_ :: forall i. Array (HTML i) -> HTML i
nav_ = nav []

noscript :: forall i. Node I.HTMLnoscript i
noscript = element (ElemName "noscript")

noscript_ :: forall i. Array (HTML i) -> HTML i
noscript_ = noscript []

object :: forall i. Node I.HTMLobject i
object = element (ElemName "object")

object_ :: forall i. Array (HTML i) -> HTML i
object_ = object []

ol :: forall i. Node I.HTMLol i
ol = element (ElemName "ol")

ol_ :: forall i. Array (HTML i) -> HTML i
ol_ = ol []

optgroup :: forall i. Node I.HTMLoptgroup i
optgroup = element (ElemName "optgroup")

optgroup_ :: forall i. Array (HTML i) -> HTML i
optgroup_ = optgroup []

option :: forall i. Node I.HTMLoption i
option = element (ElemName "option")

option_ :: forall i. Array (HTML i) -> HTML i
option_ = option []

output :: forall i. Node I.HTMLoutput i
output = element (ElemName "output")

output_ :: forall i. Array (HTML i) -> HTML i
output_ = output []

p :: forall i. Node I.HTMLp i
p = element (ElemName "p")

p_ :: forall i. Array (HTML i) -> HTML i
p_ = p []

param :: forall i. Leaf I.HTMLparam i
param props = element (ElemName "param") props []

pre :: forall i. Node I.HTMLpre i
pre = element (ElemName "pre")

pre_ :: forall i. Array (HTML i) -> HTML i
pre_ = pre []

progress :: forall i. Node I.HTMLprogress i
progress = element (ElemName "progress")

progress_ :: forall i. Array (HTML i) -> HTML i
progress_ = progress []

q :: forall i. Node I.HTMLq i
q = element (ElemName "q")

q_ :: forall i. Array (HTML i) -> HTML i
q_ = q []

rp :: forall i. Node I.HTMLrp i
rp = element (ElemName "rp")

rp_ :: forall i. Array (HTML i) -> HTML i
rp_ = rp []

rt :: forall i. Node I.HTMLrt i
rt = element (ElemName "rt")

rt_ :: forall i. Array (HTML i) -> HTML i
rt_ = rt []

ruby :: forall i. Node I.HTMLruby i
ruby = element (ElemName "ruby")

ruby_ :: forall i. Array (HTML i) -> HTML i
ruby_ = ruby []

samp :: forall i. Node I.HTMLsamp i
samp = element (ElemName "samp")

samp_ :: forall i. Array (HTML i) -> HTML i
samp_ = samp []

script :: forall i. Node I.HTMLscript i
script = element (ElemName "script")

script_ :: forall i. Array (HTML i) -> HTML i
script_ = script []

section :: forall i. Node I.HTMLsection i
section = element (ElemName "section")

section_ :: forall i. Array (HTML i) -> HTML i
section_ = section []

select :: forall i. Node I.HTMLselect i
select = element (ElemName "select")

select_ :: forall i. Array (HTML i) -> HTML i
select_ = select []

small :: forall i. Node I.HTMLsmall i
small = element (ElemName "small")

small_ :: forall i. Array (HTML i) -> HTML i
small_ = small []

source :: forall i. Leaf I.HTMLsource i
source props = element (ElemName "source") props []

span :: forall i. Node I.HTMLspan i
span = element (ElemName "span")

span_ :: forall i. Array (HTML i) -> HTML i
span_ = span []

strong :: forall i. Node I.HTMLstrong i
strong = element (ElemName "strong")

strong_ :: forall i. Array (HTML i) -> HTML i
strong_ = strong []

style :: forall i. Node I.HTMLstyle i
style = element (ElemName "style")

style_ :: forall i. Array (HTML i) -> HTML i
style_ = style []

sub :: forall i. Node I.HTMLsub i
sub = element (ElemName "sub")

sub_ :: forall i. Array (HTML i) -> HTML i
sub_ = sub []

summary :: forall i. Node I.HTMLsummary i
summary = element (ElemName "summary")

summary_ :: forall i. Array (HTML i) -> HTML i
summary_ = summary []

sup :: forall i. Node I.HTMLsup i
sup = element (ElemName "sup")

sup_ :: forall i. Array (HTML i) -> HTML i
sup_ = sup []

table :: forall i. Node I.HTMLtable i
table = element (ElemName "table")

table_ :: forall i. Array (HTML i) -> HTML i
table_ = table []

tbody :: forall i. Node I.HTMLtbody i
tbody = element (ElemName "tbody")

tbody_ :: forall i. Array (HTML i) -> HTML i
tbody_ = tbody []

td :: forall i. Node I.HTMLtd i
td = element (ElemName "td")

td_ :: forall i. Array (HTML i) -> HTML i
td_ = td []

textarea :: forall i. Leaf I.HTMLtextarea i
textarea es = element (ElemName "textarea") es []

tfoot :: forall i. Node I.HTMLtfoot i
tfoot = element (ElemName "tfoot")

tfoot_ :: forall i. Array (HTML i) -> HTML i
tfoot_ = tfoot []

th :: forall i. Node I.HTMLth i
th = element (ElemName "th")

th_ :: forall i. Array (HTML i) -> HTML i
th_ = th []

thead :: forall i. Node I.HTMLthead i
thead = element (ElemName "thead")

thead_ :: forall i. Array (HTML i) -> HTML i
thead_ = thead []

time :: forall i. Node I.HTMLtime i
time = element (ElemName "time")

time_ :: forall i. Array (HTML i) -> HTML i
time_ = time []

title :: forall i. Node I.HTMLtitle i
title = element (ElemName "title")

title_ :: forall i. Array (HTML i) -> HTML i
title_ = title []

tr :: forall i. Node I.HTMLtr i
tr = element (ElemName "tr")

tr_ :: forall i. Array (HTML i) -> HTML i
tr_ = tr []

track :: forall i. Leaf I.HTMLtrack i
track props = element (ElemName "track") props []

u :: forall i. Node I.HTMLu i
u = element (ElemName "u")

u_ :: forall i. Array (HTML i) -> HTML i
u_ = u []

ul :: forall i. Node I.HTMLul i
ul = element (ElemName "ul")

ul_ :: forall i. Array (HTML i) -> HTML i
ul_ = ul []

var :: forall i. Node I.HTMLvar i
var = element (ElemName "var")

var_ :: forall i. Array (HTML i) -> HTML i
var_ = var []

video :: forall i. Node I.HTMLvideo i
video = element (ElemName "video")

video_ :: forall i. Array (HTML i) -> HTML i
video_ = video []

wbr :: forall i. Leaf I.HTMLwbr i
wbr props = element (ElemName "wbr") props []
