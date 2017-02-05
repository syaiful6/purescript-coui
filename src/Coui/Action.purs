module Coui.Action
  ( module Exports
  , module Coui.Action
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Coroutine (CoTransformer, cotransform)
import Control.Coroutine (CoTransformer, cotransform) as Exports

import Data.Maybe (Maybe)

import Coui.Component (Action)

defaultAction :: forall eff f s t. Action eff f s t
defaultAction _ _ = pure unit

writeState :: forall eff s t. t -> CoTransformer (Maybe s) (s -> t) (Aff eff) (Maybe s)
writeState t = cotransform (const t)

modifyState :: forall eff s t. (s -> t) -> CoTransformer (Maybe s) (s -> t) (Aff eff) (Maybe s)
modifyState = cotransform
