module Coui.Component where

import Prelude

import Coui.HTML.Core (HTML)
import Coui.Action.CoT (CoT)

-- | a component
-- | `w` is the Comonad to describe this Component machinery
-- | `m` the monad you are operating in.
-- | `h` is type that will be rendered by Component
-- | `f` is the action supported by this Component
type Component' w m h f =
  { ui     :: w (h f)
  , action :: f -> CoT w m Unit
  }

newtype Component w m h f = Component
  { ui :: w (h f)
  , action :: f -> CoT w m Unit
  }

unComponent :: forall w m h f. Component w m h f -> Component' w m h f
unComponent (Component v) = v

type ComponentDSL w m = CoT w m Unit

type ComponentHTML = (HTML Void)

component
  :: forall w m h f
   . (f -> ComponentDSL w m)
  -> (w (h f))
  -> Component w m h f
component action ui = Component { action, ui }
