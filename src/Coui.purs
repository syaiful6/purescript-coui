module Coui
  ( HTML
  , Prop
  , module Coui.Aff
  , module Coui.Component
  , module Coui.HTML.Core
  , module Coui.Action
  ) where

import Data.Void (Void)

import Coui.Aff (Driver)
import Coui.Component (Component, ComponentDSL, ComponentHTML, component)
import Coui.HTML.Core (AttrName(..), ClassName(..), Namespace(..), PropName(..), ElemName(..))
import Coui.HTML.Core as C
import Coui.Action (get, gets, lift, liftAff, liftEff, modify, put)

type HTML = (C.HTML Void)

type Prop = C.Prop
