module Cofree.Counter where

import Prelude

import Control.Monad.Aff (Aff, later')
import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Trans.Class (lift)

import Data.Functor.Pairing (Pairing, freeCofree)

import React.DOM as R
import React.DOM.Props as RP

import Coui.Component as C

-- work directly
data Action a = Increment a | Decrement a
derive instance functorAction :: Functor Action

data TwoButton a = TwoButton a a
derive instance functorTwo :: Functor TwoButton

pairActionTwoButton :: Pairing Action TwoButton
pairActionTwoButton f (Increment a) (TwoButton b _) = f a b
pairActionTwoButton f (Decrement a) (TwoButton _ b) = f a b

counter :: forall eff props. C.Spec eff (Cofree TwoButton) (Free Action) props (Action Unit)
counter = C.simpleSpec eval (freeCofree pairActionTwoButton) (unfoldCofree 0 render twoButton)
  where
    twoButton :: Int -> TwoButton Int
    twoButton state = TwoButton (state + 1) (state - 1)

    render :: Int -> C.Render props (Action Unit)
    render state _ send =
      [ R.h1' [ R.text "Cofree - Comonad UI" ]
      , R.p' [ R.text "The state is: "
             , R.text (show state)
             ]
      , R.p [ RP.className "btn-group" ]
            [ R.button [ RP.className "btn btn-success"
                       , RP.onClick \_ -> send (Increment unit)
                       ]
                       [ R.text "+" ]
            , R.button [ RP.className "btn btn-danger"
                       , RP.onClick \_ -> send (Decrement unit)
                       ]
                       [ R.text "-" ]
            ]
      ]

    eval :: C.Eval eff (Action Unit) (Free Action)
    eval incr@(Increment _) = do
      lift (delay 500)
      void $ C.modifyState (liftF incr)
    eval decr@(Decrement _) = do
      lift (delay 500)
      void $ C.modifyState (liftF decr)

delay :: forall eff. Int -> Aff eff Unit
delay ms = later' ms (pure unit)
