module Coui
  ( HTML
  , Prop
  , module Coui.Aff
  , module Coui.Component
  , module Coui.HTML.Core
  , module Coui.Action
  ) where

import Coui.Aff (Driver)
import Coui.Component (Component(..), Component', ComponentHTML, Render, Action, Action',
  component, focus, focusState, match, split, foreach, withState, _render, _action, defaultRender)
import Coui.HTML.Core (AttrName(..), ClassName(..), className, Namespace(..), PropName(..), ElemName(..))
import Coui.HTML.Core as C
import Coui.Action (CoTransformer, defaultAction, writeState, modifyState, cotransform)

type HTML = ComponentHTML

type Prop = C.Prop
