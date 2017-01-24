module Cofree.Store where

import Prelude

import Control.Comonad.Store (Store, store)
import Control.Monad.State (modify)

import Data.Functor.Pairing.Co (Co)

import React.DOM as R
import React.DOM.Props as RP

import Coui as C

data Action = Increment | Decrement

ui :: Int -> C.UI (Store Int) Action
ui s = store render s
  where
    render :: Int -> C.Render Action
    render state send =
      [ R.h1' [ R.text "Store Counter - Comonad UI" ]
      , R.p' [ R.text "The state is: "
             , R.text (show state)
             ]
      , R.p [ RP.className "btn-group" ]
            [ R.button [ RP.className "btn btn-success"
                       , RP.onClick \_ -> send Increment
                       ]
                       [ R.text "+" ]
            , R.button [ RP.className "btn btn-danger"
                       , RP.onClick \_ -> send Decrement
                       ]
                       [ R.text "-" ]
            ]
      ]

performAction :: forall m. Monad m => C.PerformAction (Co (Store Int)) m Action
performAction Increment = void $ C.explore (modify (add 1))
performAction Decrement = void $ C.explore (modify (_ `sub` 1))

counterStore :: forall m. Monad m => Int -> C.Spec (Store Int) (Co (Store Int)) m Action
counterStore initial = C.simpleSpec performAction (ui initial)
