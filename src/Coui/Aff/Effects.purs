module Coui.Aff.Effects where

import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)


type CoreEffects eff =
  ( ref :: REF
  , err :: EXCEPTION
  , dom :: DOM
  | eff
  )
