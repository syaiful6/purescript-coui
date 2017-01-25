module Coui.Aff.Driver.State where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef)

import Data.Foreign (Foreign)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Traversable (traverse_)

import Unsafe.Coerce (unsafeCoerce)

import Coui.Component (Component, UI')
import Coui.Aff.Effects (CoreEffects)


newtype DriverState h r w f i o eff = DriverState (DriverStateR h r w f i o eff)

type DriverStateR h r w f i o eff =
  { component :: Component h w f i o (Aff (CoreEffects eff))
  , space :: UI' w h i
  , refs :: SM.StrMap Foreign
  , selfRef :: Ref (DriverState h w f i o eff)
  , handler :: o -> Aff (CoreEffects eff) Unit
  , pendingRefs :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , pendingQueries :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , rendering :: Maybe (r i eff)
  }

initDriverState
  :: forall h r w f i o eff
   . Component h w f i o (Aff (CoreEffects eff))
  -> (o -> Aff (CoreEffects eff) Unit)
  -> Eff (CoreEffects eff) (Ref (DriverState h r w f i o eff))
initDriverState component handler = do
  selfRef <- newRef (unsafeCoerce {})
  pendingRefs <- newRef (Just Nil)
  pendingQueries <- newRef (component.initializer $> Nil)
  pendingOuts <- newRef (Just Nil)
  let
    ds =
      { component: component
      , space: component.space
      , refs: pendingRefs
      , handler: handler
      , pendingRefs: pendingRefs
      , pendingQueries: pendingQueries
      , pendingOuts: pendingOuts
      , rendering: Nothing
      }
  writeRef selfRef (DriverState ds)
  pure selfRef
