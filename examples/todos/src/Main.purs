module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Coui.Aff (CoreEffects, runCouiAff, awaitBody)
import Coui.VDom.Driver (runUI)

import Component.TaskList (taskList)
import Model (initialTaskList)

main :: forall eff. Eff (CoreEffects eff) Unit
main = runCouiAff do
  body <- awaitBody
  runUI taskList initialTaskList body
