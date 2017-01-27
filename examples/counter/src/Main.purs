module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))

import React.DOM as R

import Coui (CoUIEff, runCoUIAff, combine', runUI, awaitLoad, selectElement)

import Counter.Cofree (counterCofree)
import Counter.Store (counterStore)

main :: forall eff. Eff (CoUIEff eff) Unit
main = runCoUIAff do
  let
    testCombined i =
      combine' (\x y s ->
        [ R.h1' [ R.text "Cofree" ] ]
        <> x s
        <> [ R.h1' [ R.text "Store" ] ]
        <> y s) (counterCofree i) (counterStore i)
  awaitLoad
  v <- selectElement "#app"
  case v of
    Nothing -> throwError (error "could not find #app")
    Just el -> runUI (testCombined 0) el
