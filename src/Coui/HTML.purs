module Coui.HTML
  ( module Coui.HTML.Core
  , module Coui.HTML.Elements
  , module Coui.HTML.Properties
  ) where


import Coui.HTML.Core (class IsProp, AttrName(..), ClassName(..), HTML(..), Namespace(..),
  PropName(..), ElemName(..), text, thunk, handler, className)
import Coui.HTML.Elements (Leaf, Node, withKeys, withKeys_, a, a_, abbr, abbr_, address,
  address_, area, article, article_, aside, aside_, audio, audio_, b, b_, base, bdi, bdi_,
  bdo, bdo_, blockquote, blockquote_, body, body_, br, br_, button, button_, canvas, caption,
  caption_, cite, cite_, code, code_, col, colgroup, colgroup_, command, datalist, datalist_,
  dd, dd_, del, del_, details, details_, dfn, dfn_, dialog, dialog_, div, div_, dl, dl_, dt, dt_,
  element, em, em_, embed, embed_, fieldset, fieldset_, figcaption, figcaption_, figure, figure_,
  footer, footer_, form, form_, h1, h1_, h2, h2_, h3, h3_, h4, h4_, h5, h5_, h6, h6_, head, head_,
  header, header_, hr, hr_, html, html_, i, i_, iframe, img, input, ins, ins_, kbd, kbd_,
  label, label_, legend, legend_, li, li_, link, main, main_, map, map_, mark, mark_, menu, menu_,
  menuitem, menuitem_, meta, meter, meter_, nav, nav_, noscript, noscript_, object, object_, ol, ol_,
  optgroup, optgroup_, option, option_, output, output_, p, p_, param, pre, pre_, progress, progress_,
  q, q_, rp, rp_, rt, rt_, ruby, ruby_, samp, samp_, script, script_, section, section_, select, select_,
  small, small_, source, span, span_, strong, strong_, style, style_, sub, sub_, summary, summary_, sup,
  sup_, table, table_, tbody, tbody_, td, td_, textarea, tfoot, tfoot_, th, th_, thead, thead_, time,
  time_, title, title_, tr, tr_, track, u, u_, ul, ul_, var, var_, video, video_, wbr)
import Coui.HTML.Properties (IProp, attr, prop, onRemoved, onCreated)
