module ComonadUI.Component where

import Prelude

import Data.Functor.Day (Day, day, runDay)
import Data.Functor.Pairing (Pairing, sym)
import Data.Functor.Pairing.Co (Co, pairCo, runCo, co)

import Coui.HTML.Core (HTML)
import Coui.Action.CoM (CoM)
import Coui.Action.CoM as CM


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
