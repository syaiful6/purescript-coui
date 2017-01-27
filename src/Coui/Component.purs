module Coui.Component where

import Prelude

import Data.Functor.Pairing (Pairing, sym)
import Data.Functor.Pairing.Co (Co, pairCo)

import Coui.HTML.Core (HTML)
import Coui.Action.CoM (CoM)


-- | a component
-- | `h` is type that will be rendered by Component
-- | `w` is the Comonad to describe this Component machinery
-- | `g` is type that pair with the Comonad, used to move around the machine.
-- | `f` is the action supported by this Component
-- | `i` is initial input to produce the initial value of `w`
newtype Component h w g f i o m = Component
  { ui     :: i -> w (h Void (f Unit))
  , action :: f ~> ComponentDSL g f o m
  , pair   :: Pairing g w
  }

type ComponentDSL = CoM

type ComponentHTML f = HTML Void (f Unit)

component
  :: forall h w g f i o m
   . (f ~> ComponentDSL g f o m)
  -> Pairing g w
  -> (i -> w (h Void (f Unit)))
  -> Component h w g f i o m
component action pair ui = Component { action, pair, ui }

simpleComponent
  :: forall h w f i o m
   . Functor w
  => (f ~> ComponentDSL (Co w) f o m)
  -> (i -> w (h Void (f Unit)))
  -> Component h w (Co w) f i o m
simpleComponent action ui = Component { action, ui, pair: sym pairCo }
