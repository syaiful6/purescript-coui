module Counter.Store where

import Prelude

import Control.Comonad.Store (Store, store)
import Control.Monad.State (modify)

import Data.Functor.Pairing.Co (Co)

import React.DOM as R
import React.DOM.Props as RP

import Coui as W

data Action = Increment | Decrement

ui :: Int -> W.UI (Store Int) Action
ui s = store render s
  where
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

performAction :: forall m. Monad m => W.PerformAction (Co (Store Int)) m Action
performAction Increment = void $ W.explore (modify (add 1))
performAction Decrement = void $ W.explore (modify (_ `sub` 1))

counterStore :: forall m. Monad m => Int -> W.Spec (Store Int) (Co (Store Int)) m Action
counterStore initial = W.simpleSpec performAction (ui initial)
