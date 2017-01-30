module Coui.Aff.Effects where

import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)


-- | A type alias for the basic row of effects involved in running Halogen with
-- | `Aff`-based drivers.
type CoreEffects eff =
  ( ref :: REF
  , err :: EXCEPTION
  , dom :: DOM
  | eff
  )
