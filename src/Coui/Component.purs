module Coui.Component where

import Prelude

import Coui.HTML.Core (HTML)
import Coui.Action.CoT (CoT)
import Coui.Action.CoM (CoM)


-- | a component
-- | `h` is type that will be rendered by Component
-- | `w` is the Comonad to describe this Component machinery
-- | `f` is the action supported by this Component
-- | `i` is initial input to produce the initial value of `w`
-- | `o` is output message
type Component' h w f i o m =
  { ui     :: i -> w (h Void f)
  , action :: f -> ComponentDSL w f o m
  }

newtype Component h w f i o m = Component
  { ui :: i -> w (h Void f)
  , action :: f -> ComponentDSL w f o m
  }

unComponent :: forall h w f i o m. Component h w f i o m -> Component' h w f i o m
unComponent (Component v) = v

type ComponentDSL w f o m = CoT w (CoM f o m) Unit

type ComponentHTML f = HTML Void f

component
  :: forall h w f i o m
   . (f -> ComponentDSL w f o m)
  -> (i -> w (h Void f))
  -> Component h w f i o m
component action ui = Component { action, ui }
