module Coui.HTML.Elements.Keyed
  ( article, article_
  , colgroup, colgroup_
  , dialog, dialog_
  , div, div_
  , dl, dl_
  , fieldset, fieldset_
  , footer, footer_
  , form, form_
  , header, header_
  , menu, menu_
  , ol, ol_
  , table, table_
  , tbody, tbody_
  , tfoot, tfoot_
  , thead, thead_
  , tr, tr_
  , ul, ul_
  ) where

import Data.Tuple (Tuple)

import DOM.HTML.Indexed as I

import Coui.HTML.Core (ElemName(..), HTML)
import Coui.HTML.Elements (keyed)
import Coui.HTML.Properties (IProp)

type KeyedNode r i
   = Array (IProp r i)
  -> Array (Tuple String (HTML i))
  -> HTML i

article :: forall i. KeyedNode I.HTMLarticle i
article = keyed (ElemName "article")

article_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
article_ = article []

colgroup :: forall i. KeyedNode I.HTMLcolgroup i
colgroup = keyed (ElemName "colgroup")

colgroup_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
colgroup_ = colgroup []

dialog :: forall i. KeyedNode I.HTMLdialog i
dialog = keyed (ElemName "dialog")

dialog_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
dialog_ = dialog []

div :: forall i. KeyedNode I.HTMLdiv i
div = keyed (ElemName "div")

div_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
div_ = div []

dl :: forall i. KeyedNode I.HTMLdl i
dl = keyed (ElemName "dl")

dl_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
dl_ = dl []

fieldset :: forall i. KeyedNode I.HTMLfieldset i
fieldset = keyed (ElemName "fieldset")

fieldset_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
fieldset_ = fieldset []

footer :: forall i. KeyedNode I.HTMLfooter i
footer = keyed (ElemName "footer")

footer_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
footer_ = footer []

form :: forall i. KeyedNode I.HTMLform i
form = keyed (ElemName "form")

form_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
form_ = form []

header :: forall i. KeyedNode I.HTMLheader i
header = keyed (ElemName "header")

header_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
header_ = header []

menu :: forall i. KeyedNode I.HTMLmenu i
menu = keyed (ElemName "menu")

menu_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
menu_ = menu []

ol :: forall i. KeyedNode I.HTMLol i
ol = keyed (ElemName "ol")

ol_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
ol_ = ol []

table :: forall i. KeyedNode I.HTMLtable i
table = keyed (ElemName "table")

table_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
table_ = table []

tbody :: forall i. KeyedNode I.HTMLtbody i
tbody = keyed (ElemName "tbody")

tbody_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
tbody_ = tbody []

tfoot :: forall i. KeyedNode I.HTMLtfoot i
tfoot = keyed (ElemName "tfoot")

tfoot_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
tfoot_ = tfoot []

thead :: forall i. KeyedNode I.HTMLthead i
thead = keyed (ElemName "thead")

thead_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
thead_ = thead []

tr :: forall i. KeyedNode I.HTMLtr i
tr = keyed (ElemName "tr")

tr_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
tr_ = tr []

ul :: forall i. KeyedNode I.HTMLul i
ul = keyed (ElemName "ul")

ul_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
ul_ = ul []
