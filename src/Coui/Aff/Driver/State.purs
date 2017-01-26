module Coui.Aff.Driver.State where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef)

import Data.Foreign (Foreign)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.StrMap as SM

import Unsafe.Coerce (unsafeCoerce)

import Coui.Component (Component, UI')
import Coui.Aff.Effects (CoreEffects)


newtype DriverState h r w f i z o eff = DriverState (DriverStateR h r w f i z o eff)

type DriverStateR h r w f i z o eff =
  { component :: Component h w f i o (Aff (CoreEffects eff))
  , space :: UI' w h i
  , refs :: SM.StrMap Foreign
  , selfRef :: Ref (DriverState h r w f i z o eff)
  , handler :: o -> Aff (CoreEffects eff) Unit
  , pendingRefs :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , pendingQueries :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  , rendering :: Maybe (r i eff)
  , prjQuery :: i -> Maybe z
  }

data DriverStateX
  (h :: * -> *)
  (r :: * -> # ! -> *)
  (i :: *)
  (eff :: # !)

mkDriverStateXRef
  :: forall h r w f i z o eff
   . Ref (DriverState h r w f i z o eff)
  -> Ref (DriverStateX h r i eff)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r i x eff
   . (forall w f z o. DriverStateR h r w f i z o eff -> x)
  -> DriverStateX h r i eff
  -> x
unDriverStateX = unsafeCoerce

initDriverState
  :: forall h r w f i z o eff
   . Component h w f i o (Aff (CoreEffects eff))
  -> (o -> Aff (CoreEffects eff) Unit)
  -> i -> Maybe z
  -> Eff (CoreEffects eff) (Ref (DriverStateX h r i eff))
initDriverState component handler prjQuery = do
  selfRef <- newRef (unsafeCoerce {})
  pendingRefs <- newRef (Just Nil)
  pendingQueries <- newRef (component.initializer $> Nil)
  pendingOuts <- newRef (Just Nil)
  let
    ds =
      { component: component
      , space: component.space
      , refs: SM.empty
      , selfRef
      , handler
      , pendingRefs: pendingRefs
      , pendingQueries: pendingQueries
      , pendingOuts: pendingOuts
      , rendering: Nothing
      , prjQuery
      }
  writeRef selfRef (DriverState ds)
  pure (mkDriverStateXRef selfRef)
