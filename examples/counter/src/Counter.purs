module Counter.Counter where

import Prelude

import Control.Comonad (extend)
import Control.Comonad.Traced (Traced, traced, track)
import Control.Monad.Writer (tell)
import Data.Monoid.Additive (Additive(..))

import Coui as W
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP

data Action = Increment | Decrement

data Message = Incremented Boolean

type State = Int

type Incremental = Traced (Additive State)

counterTraced :: forall m. W.Component HH.HTML Incremental Action State Message m
counterTraced = W.component performAction build

-- | Build Incremental with specified initial state
build :: State -> Incremental (W.ComponentHTML Action)
build s = extend (\x -> track (Additive (s)) x) (traced ui)

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

performAction :: forall m. Action -> W.ComponentDSL Incremental Action Message m
performAction Increment = do
  tell (Additive 1)
  W.lift (W.raise (Incremented true))
performAction Decrement = do
  tell (Additive (-1))
  pure unit
