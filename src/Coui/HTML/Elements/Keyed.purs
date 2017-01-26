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

import Coui.HTML.Core (ElemName(..), HTML)
import Coui.HTML.Properties (IProp, I, GlobalProperties, InteractiveEvents)

import Coui.HTML.Elements (keyed)

type KeyedNode r i
   = Array (IProp (InteractiveEvents (GlobalProperties r)) i)
  -> Array (Tuple String (HTML i))
  -> HTML i

article :: forall i. KeyedNode () i
article = keyed (ElemName "article")

article_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
article_ = article []

colgroup :: forall i. KeyedNode (span :: I) i
colgroup = keyed (ElemName "colgroup")

colgroup_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
colgroup_ = colgroup []

dialog :: forall i. KeyedNode (open :: I) i
dialog = keyed (ElemName "dialog")

dialog_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
dialog_ = dialog []

div :: forall i. KeyedNode (onScroll :: I) i
div = keyed (ElemName "div")

div_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
div_ = div []

dl :: forall i. KeyedNode (onScroll :: I) i
dl = keyed (ElemName "dl")

dl_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
dl_ = dl []

fieldset :: forall i. KeyedNode (disabled :: I, form :: I, onScroll :: I) i
fieldset = keyed (ElemName "fieldset")

fieldset_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
fieldset_ = fieldset []

footer :: forall i. KeyedNode () i
footer = keyed (ElemName "footer")

footer_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
footer_ = footer []

form :: forall i. KeyedNode (acceptCharset :: I, action :: I, autocomplete :: I, enctype :: I, method :: I, onReset :: I, novalidate :: I, onScroll :: I, onSubmit :: I, target :: I) i
form = keyed (ElemName "form")

form_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
form_ = form []

header :: forall i. KeyedNode () i
header = keyed (ElemName "header")

header_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
header_ = header []

menu :: forall i. KeyedNode (label :: I, onScroll :: I, menuType :: I) i
menu = keyed (ElemName "menu")

menu_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
menu_ = menu []

ol :: forall i. KeyedNode (onScroll :: I, reversed :: I, start :: I, olType :: I) i
ol = keyed (ElemName "ol")

ol_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
ol_ = ol []

table :: forall i. KeyedNode (sortable :: I) i
table = keyed (ElemName "table")

table_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
table_ = table []

tbody :: forall i. KeyedNode (onScroll :: I) i
tbody = keyed (ElemName "tbody")

tbody_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
tbody_ = tbody []

tfoot :: forall i. KeyedNode (onScroll :: I) i
tfoot = keyed (ElemName "tfoot")

tfoot_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
tfoot_ = tfoot []

thead :: forall i. KeyedNode () i
thead = keyed (ElemName "thead")

thead_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
thead_ = thead []

tr :: forall i. KeyedNode () i
tr = keyed (ElemName "tr")

tr_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
tr_ = tr []

ul :: forall i. KeyedNode (onScroll :: I) i
ul = keyed (ElemName "ul")

ul_ :: forall i. Array (Tuple String (HTML i)) -> HTML i
ul_ = ul []
