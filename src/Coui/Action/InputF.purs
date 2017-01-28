module Coui.Action.InputF where

import Prelude

import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype RefLabel = RefLabel String

derive instance newtypeRefLabel :: Newtype RefLabel _
derive newtype instance eqRefLabel :: Eq RefLabel
derive newtype instance ordRefLabel :: Ord RefLabel

data InputF i
  = RefUpdate RefLabel (Maybe Foreign)
  | Action i

instance functorInputF :: Functor InputF where
  map f (Action i) = Action (f i)
  map _ (RefUpdate label fo) = RefUpdate label fo
