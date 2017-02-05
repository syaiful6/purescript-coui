module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Coui.Aff (CoreEffects, runCouiAff, awaitBody)
import Coui.VDom.Driver (runUI)

import Component.TaskList (taskList)
import Model (initialTaskListState)

main :: forall eff. Eff (CoreEffects eff) Unit
main = runCouiAff do
  body <- awaitBody
  runUI taskList initialTaskListState body
