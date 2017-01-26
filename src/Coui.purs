module Coui
  ( CouiIO
  , module Coui.Component
  , module Coui.Util
  ) where

import Prelude

import Control.Coroutine as CR

import Coui.Component (UI, UI', ComponentDSL, ComponentSpec, Component, component
  , simpleComponent, lifecycleComponent, simplelifecycleComponent)
import Coui.Util (CoUIEff, awaitLoad, awaitBody, selectElement, runCoUIAff)


type CouiIO i o m =
  { dispatch :: i -> m Unit
  , subscribe :: CR.Consumer o m Unit -> m Unit
  }
