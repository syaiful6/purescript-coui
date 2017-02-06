module Coui
  ( Prop
  , module Coui.Aff
  , module Coui.Component
  , module Coui.HTML.Core
  ) where

import Coui.Aff (Driver)
import Coui.Component (Component(..), Component', component, ixcomponent, overV, action, render,
  hoist, match, withState, Complet(..), Action, Handler, Render, complet, perform, update,
  effects,withEffects, defaultAction, defaultRender, foreach, ignore, both, terminate)
import Coui.HTML.Core (HTML(..), AttrName(..), ClassName(..), className, Namespace(..), PropName(..), ElemName(..))
import Coui.HTML.Core as C

type Prop = C.Prop
