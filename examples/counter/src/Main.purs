module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Coui.Aff (CoreEffects, runCouiAff, awaitBody)
import Coui.VDom.Driver (runUI)

import Counter.Counter (counter)

main :: forall eff. Eff (CoreEffects eff) Unit
main = runCouiAff do
  body <- awaitBody
  runUI counter 5 body
