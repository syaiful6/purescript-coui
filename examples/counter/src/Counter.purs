module Counter.Counter where

import Prelude

import Control.Comonad.Traced (Traced, traced)
import Control.Monad.Writer (tell)
import Data.Monoid.Additive (Additive(..))

import Coui as W
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP

data Action = Increment | Decrement

data Message = Incremented Boolean

type State = Int
-- | Use Traced Comonad
type Incremental = Traced (Additive State)

counterTraced :: forall m. W.Component Incremental m W.ComponentHTML Action
counterTraced = W.component performAction (traced ui)

ui :: Additive State -> W.ComponentHTML Action
ui (Additive s) =
  HH.div_
    [ HH.p_ [ HH.text "The state is: ", HH.text (show s) ]
    , HH.div
        [ HP.class_ $ HH.ClassName "btn-group" ]
        [ HH.button
            [ HP.class_ $ HH.ClassName "btn btn-success"
            , HE.onClick (HE.input_ Increment)
            ]
            [ HH.text "+"]
        , HH.button
            [ HP.class_ $ HH.ClassName "btn btn-danger"
            , HE.onClick (HE.input_ Decrement)
            ]
            [ HH.text "-"]
        ]
    ]

performAction :: forall m. Action -> W.ComponentDSL Incremental m
performAction Increment = do
  tell (Additive 1)
performAction Decrement = do
  tell (Additive (-1))
  pure unit
