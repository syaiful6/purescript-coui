module Coui.Driver.Aff.State where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef)

import Data.Foreign (Foreign)
import Data.Functor.Pairing (Pairing, sym)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.List (List(..))
import Data.Traversable (traverse_)

import Unsafe.Coerce (unsafeCoerce)

import Coui.Effects (CoreEffects)
import Coui.Component (Component(..))

newtype DriverState h r w g f i o eff = DriverState (DriverStateR h r w g f i o eff)

type DriverStateR h r w g f i o eff =
  { component :: Component h w g f i o (Aff (CoreEffects eff))
  , state :: w (h Void (f Unit))
  , refs :: SM.StrMap Foreign
  , selfRef :: Ref (DriverState h r w g f i o eff)
  , handler :: o -> Aff (CoreEffects eff) Unit
  , pendingRefs :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , pendingQueries :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , rendering :: Maybe (r w f o eff)
  }

unDriverState
  :: forall h r w g f i o eff
   . DriverState h r w g f i o eff
  -> DriverStateR h r w g f i o eff
unDriverState (DriverState ds) = ds

initDriverState
  :: forall h r w g f i o eff
   . Component h w g f i o (Aff (CoreEffects eff))
  -> i
  -> (o -> Aff (CoreEffects eff) Unit)
  -> Eff (CoreEffects eff) (Ref (DriverState h r w g f i o eff))
initDriverState comp@(Component { ui }) input handler = do
  selfRef <- newRef (unsafeCoerce {})
  pendingRefs <- newRef (Just Nil)
  pendingQueries <- newRef (Just Nil)
  pendingOuts <- newRef (Just Nil)
  let
    ds =
      { component: comp
      , state: ui input
      , refs: SM.empty
      , selfRef
      , handler
      , pendingRefs
      , pendingQueries
      , pendingOuts
      , rendering: Nothing
      }
  writeRef selfRef (DriverState ds)
  pure $ selfRef
