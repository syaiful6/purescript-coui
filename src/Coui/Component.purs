module Coui.Component where

import Prelude

import Data.Bifunctor (class Bifunctor, lmap)
import Data.Maybe (Maybe(..))
import Data.Functor.Pairing (Pairing, sym)
import Data.Functor.Pairing.Co (Co, pairCo)

import Coui.HTML.Core (HTML)
import Coui.Action.CoM (CoM)
import Coui.Action.CoM as CA


type ComponentDSL = CoM

type UI' w h i = w (h i)

type UI w i = UI' w HTML i

type ComponentSpec h w f i o m =
  { space :: UI' w h i
  , performAction :: i -> ComponentDSL f i o m Unit
  }

type LifecycleComponentSpec h w f i o m =
  { space :: UI' w h i
  , performAction :: i -> ComponentDSL f i o m Unit
  , initializer :: Maybe i
  , finalizer :: Maybe i
  }

type Component h w f i o m =
  { space :: UI' w h i
  , performAction :: i -> ComponentDSL f i o m Unit
  , pair :: Pairing f w
  , initializer :: Maybe i
  , finalizer :: Maybe i
  }

component
  :: forall h w f i o m
   . Pairing f w
  -> ComponentSpec h w f i o m
  -> Component h w f i o m
component pair spec =
  lifecycleComponent pair $
    { space: spec.space
    , performAction: spec.performAction
    , initializer: Nothing
    , finalizer: Nothing
    }

simpleComponent
  :: forall h w i o m. Functor w
  => ComponentSpec h w (Co w) i o m
  -> Component h w (Co w) i o m
simpleComponent = component (sym pairCo)

lifecycleComponent
  :: forall h w f i o m
   . Pairing f w
  -> LifecycleComponentSpec h w f i o m
  -> Component h w f i o m
lifecycleComponent pair spec =
  { space: spec.space
  , performAction: spec.performAction
  , pair: pair
  , initializer: spec.initializer
  , finalizer: spec.finalizer
  }

simplelifecycleComponent
  :: forall h w i o m. Functor w
  => LifecycleComponentSpec h w (Co w) i o m
  -> Component h w (Co w) i o m
simplelifecycleComponent = lifecycleComponent (sym pairCo)
