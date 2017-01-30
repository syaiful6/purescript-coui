module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Coui.Aff (CoreEffects, runCouiAff, awaitBody)
import Coui.VDom.Driver (runUI)

import Counter.Counter (counterTraced)

main :: forall eff. Eff (CoreEffects eff) Unit
main = runCouiAff do
  body <- awaitBody
  runUI counterTraced body
