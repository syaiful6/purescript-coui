module Coui.Action where

import Prelude


type Action f = Unit -> f Unit

action :: forall f. Action f -> f Unit
action act = act unit
