module Coui
  ( HTML
  , Prop
  , module Coui.Aff
  , module Coui.Component
  , module Coui.HTML.Core
  , module Coui.Action
  ) where

import Coui.Aff (CouiIO)
import Coui.Component (Component, ComponentDSL, ComponentHTML, component)
import Coui.HTML.Core (AttrName(..), ClassName(..), Namespace(..), PropName(..), ElemName(..))
import Coui.HTML.Core as C
import Coui.Action (CoF(..), CoM(..), RefLabel(..), get, getHTMLElementRef,
  getRef, gets, lift, liftAff, liftEff, modify, put, raise)

type HTML p i = C.HTML p i

type Prop i = C.Prop i
