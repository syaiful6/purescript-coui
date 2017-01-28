module Coui.Action
  ( getHTMLElementRef
  , module Exports
  , module Coui.Action.CoM
  ) where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..))
import Data.Either (either)
import Data.Foreign (Foreign)

import DOM.HTML.Types (HTMLElement, readHTMLElement)

import Coui.Action.CoM (CoM(..), CoF(..), getRef, raise)
import Coui.Action.InputF (RefLabel(..))

import Coui.Action.InputF (RefLabel(..)) as Exports
import Coui.Action.CoM (raise) as Exports
import Control.Monad.Aff.Class (liftAff) as Exports
import Control.Monad.Eff.Class (liftEff) as Exports
import Control.Monad.State.Class (get, gets, modify, put) as Exports
import Control.Monad.Trans.Class (lift) as Exports


getHTMLElementRef :: forall f o m. RefLabel -> CoM f o m (Maybe HTMLElement)
getHTMLElementRef = map (go =<< _) <<< getRef
  where
  go :: Foreign -> Maybe HTMLElement
  go = either (const Nothing) Just <<< runExcept <<< readHTMLElement
