module Counter.Cofree where

import Prelude

import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Control.Monad.Aff (Aff, later')
import Control.Monad.Free (Free, liftF)
import Control.Monad.Trans.Class (lift)

import Coui as W

import Data.Functor.Pairing (Pairing, freeCofree)

import React.DOM as R
import React.DOM.Props as RP


-- work pairing directly
data UpDown a = Up a | Down a
derive instance functorUpDown :: Functor UpDown

data TwoButton a = TwoButton a a
derive instance functorTwo :: Functor TwoButton

pairUpDownTwoButton :: Pairing UpDown TwoButton
pairUpDownTwoButton f (Up a) (TwoButton b _) = f a b
pairUpDownTwoButton f (Down a) (TwoButton _ b) = f a b

-- | The Action
data Action = Increment | Decrement

ui :: Int -> W.UI (Cofree TwoButton) Action
ui initial = unfoldCofree initial render twoButton
  where
    twoButton :: Int -> TwoButton Int
    twoButton state = TwoButton (state + 1) (state - 1)

    render :: Int -> W.Render Action
    render state send =
      [ R.p' [ R.text "The state is: ", R.text (show state) ]
      , R.p [ RP.className "btn-group" ]
          [ R.button
              [ RP.className "btn btn-success"
              , RP.onClick \_ -> send Increment
              ]
              [ R.text "+" ]
          , R.button
              [ RP.className "btn btn-danger"
              , RP.onClick \_ -> send Decrement
              ]
              [ R.text "-" ]
          ]
      ]

performAction :: forall eff. W.PerformAction (Free UpDown) (Aff eff) Action
performAction Increment = do
  lift (delay 500)
  void $ W.explore (liftF (Up unit))
performAction Decrement = do
  lift (delay 500)
  void $ W.explore (liftF (Down unit))

-- | The component spec
counterCofree :: forall eff. Int -> W.Spec (Cofree TwoButton) (Free UpDown) (Aff eff) Action
counterCofree initial = W.spec performAction (freeCofree pairUpDownTwoButton) (ui initial)

delay :: forall eff. Int -> Aff eff Unit
delay ms = later' ms (pure unit)
