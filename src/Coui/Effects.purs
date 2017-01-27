module Coui.Effects where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)


-- | A type alias for the basic row of effects involved in running Halogen with
-- | `Aff`-based drivers.
type CoreEffects eff =
  ( avar :: AVAR
  , ref :: REF
  , err :: EXCEPTION
  , dom :: DOM
  | eff
  )
