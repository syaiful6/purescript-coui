module Main where

import Prelude

import Control.Monad.Aff (Aff, later')
import Control.Monad.Eff (Eff)
import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Trans.Class (lift)

import Data.Functor.Pairing (Pairing, freeCofree)
import Data.Maybe (Maybe(..))

import React.DOM as R
import React.DOM.Props as RP

import Coui as C

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

ui :: forall props. Int -> C.UI (Cofree TwoButton) props Action
ui initial = unfoldCofree initial render twoButton
  where
    twoButton :: Int -> TwoButton Int
    twoButton state = TwoButton (state + 1) (state - 1)

    render :: Int -> C.Render props Action
    render state _ send =
      [ R.h1' [ R.text "Cofree Counter - Comonad UI" ]
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

performAction :: forall eff. C.PerformAction eff (Free UpDown) Action
performAction Increment = do
  lift (delay 500)
  void $ C.explore (liftF (Up unit))
performAction Decrement = do
  lift (delay 500)
  void $ C.explore (liftF (Down unit))

-- | The component spec
counter :: forall eff props. Int -> C.Spec eff (Cofree TwoButton) (Free UpDown) props Action
counter initial = C.spec performAction (freeCofree pairUpDownTwoButton) (ui initial)

delay :: forall eff. Int -> Aff eff Unit
delay ms = later' ms (pure unit)

main :: forall eff. Eff (C.CoUIEff eff) Unit
main = C.runCoUIAff do
  C.awaitLoad
  v <- C.selectElement "#app"
  case v of
    Nothing -> throwError (error "could not find #app")
    Just el -> C.runUI (counter 0) {} el
