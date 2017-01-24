module Coui.Component
  ( PerformAction
  , defaultDispatcher
  , EventHandler
  , Render
  , UI
  , Spec(..)
  , combine
  , combine'
  , explore
  , spec
  , simpleSpec
  , createClass
  , createReactSpec
  , createReactSpec'
  , runUI
  , hoistPerformAction
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Comonad (class Comonad, extract, duplicate)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free.Trans (hoistFreeT, interpret)
import Control.Monad.Rec.Class (forever)

import Data.Either (Either(..))
import Data.Functor.Day (Day, day, runDay)
import Data.Functor.Pairing (Pairing, sym)
import Data.Functor.Pairing.Co (Co, pairCo, runCo, co)
import Data.Lens (Lens', lens)
import Data.Profunctor (lmap)

import Control.Comonad.Store (Store, store)
import Control.Monad.State (modify)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import React as R
import React.DOM as RD
import ReactDOM as RDOM


type PerformAction g m action =
  action -> CR.Producer (g Unit) m Unit

defaultDispatcher :: forall g m action. Monad m => PerformAction g m action
defaultDispatcher _ = pure unit

explore :: forall m o. Monad m => o -> CR.Producer o m Unit
explore = CR.emit

type EventHandler =
  forall eff refs.
    Eff ( props :: R.ReactProps
        , state :: R.ReactState R.ReadWrite
        , refs :: R.ReactRefs refs
        | eff
        ) Unit

type Render action = (action -> EventHandler) -> Array R.ReactElement

type UI w action = w (Render action)

newtype Spec w g m action = Spec
  { space          :: UI w action
  , performAction  :: PerformAction g m action
  , pair           :: Pairing g w
  }

-- | Manually create Spec by provides all spec field. See simpleSpec that use Co if you
-- | don't want use explicit Pairing
spec
  :: forall w g m action
   . PerformAction g m action
  -> Pairing g w
  -> UI w action
  -> Spec w g m action
spec performAction pair space = Spec { space, performAction, pair }

-- | An easy way to create spec by just provides UI and PerformAction.
simpleSpec
  :: forall w m action. Functor w
  => PerformAction (Co w) m action
  -> UI w action
  -> Spec w (Co w) m action
simpleSpec performAction space = Spec { space, performAction, pair: sym pairCo }

_performAction :: forall w g m f. Lens' (Spec w g m f) (PerformAction g m f)
_performAction = lens (\(Spec s) -> s.performAction) (\(Spec s) pa -> Spec (s { performAction = pa }))

_space :: forall w g m f. Lens' (Spec w g m f) (UI w f)
_space = lens (\(Spec s) -> s.space) (\(Spec s) sa -> Spec (s { space = sa }))

createClass
  :: forall w g action eff props. Comonad w
  => Spec w g (Aff eff) action -> R.ReactClass props
createClass spec' = R.createClass <<< _.spec $ createReactSpec spec'

createReactSpec
  :: forall w g props action eff. Comonad w
  => Spec w g (Aff eff) action
  -> { spec     :: R.ReactSpec props (UI w action) eff
     , dispatch :: R.ReactThis props (UI w action) -> action -> EventHandler
     }
createReactSpec = createReactSpec' RD.div'

createReactSpec'
  :: forall w g props action eff. Comonad w
  => (Array R.ReactElement -> R.ReactElement)
  -> Spec w g (Aff eff) action
  -> { spec     :: R.ReactSpec props (UI w action) eff
     , dispatch :: R.ReactThis props (UI w action) -> action -> EventHandler
     }
createReactSpec' wrap (Spec sp) =
  { spec: R.spec sp.space render
  , dispatch: dispatcher
  }
  where
    dispatcher :: R.ReactThis props (UI w action) -> action -> EventHandler
    dispatcher this action = void do
      let coerceEff :: forall eff1 a. Eff eff1 a -> Eff eff a
          coerceEff = unsafeCoerceEff

          consumer :: CR.Consumer (g Unit) (Aff eff) Unit
          consumer = forever do
            o <- CR.await
            lift $ liftEff $ coerceEff (R.transformState this (sp.pair (const id) o <<< duplicate))

          producer :: CR.Producer (g Unit) (Aff eff) Unit
          producer = sp.performAction action

      unsafeCoerceEff (launchAff (CR.runProcess (CR.connect producer consumer)))

    render :: R.Render props (UI w action) eff
    render this = do
      state <- R.readState this
      pure (wrap $ extract state (dispatcher this))

-- change the underlying monad of Component performAction
hoistPerformAction
  :: forall w g m n action. Functor n
  => (m ~> n)
  -> Spec w g m action
  -> Spec w g n action
hoistPerformAction nat (Spec s) = Spec $
  { space: s.space
  , pair: s.pair
  , performAction: hoistFreeT nat <<< s.performAction
  }

combine
  :: forall w1 w2 m act1 act2. (Comonad w1, Comonad w2, Monad m)
  => (forall x. Render x -> Render x -> Render x)
  -> Spec w1 (Co w1) m act1
  -> Spec w2 (Co w2) m act2
  -> Spec (Day w1 w2) (Co (Day w1 w2)) m (Either act1 act2)
combine cobuild (Spec s1) (Spec s2) =
  Spec { space: day build s1.space s2.space
       , pair: sym pairCo
       , performAction
       }
  where
    build :: Render act1 -> Render act2 -> Render (Either act1 act2)
    build x y = cobuild (lmap (_ `compose` Left) x) (lmap (_ `compose` Right) y)

    performAction :: PerformAction (Co (Day w1 w2)) m (Either act1 act2)
    performAction = case _ of
      Left a' -> interpret (\(CR.Emit o r) -> CR.Emit (introCoDay1 o) r) (s1.performAction a')
      Right b -> interpret (\(CR.Emit o r) -> CR.Emit (introCoDay2 o) r) (s2.performAction b)

introCoDay1 :: forall w w' a. (Functor w, Comonad w') => Co w a -> Co (Day w w') a
introCoDay1 a = co (runDay \f w w' -> runCo a (map (_ `f` extract w') w))

introCoDay2 :: forall w w' a. (Functor w, Comonad w') => Co w a -> Co (Day w' w) a
introCoDay2 a = co (runDay \f w' w -> runCo a (map (f (extract w')) w))

type Rec f g =
  { first :: f
  , second :: g
  }

combine'
  :: forall w1 w2 g1 g2 m act1 act2. (Comonad w1, Comonad w2, Monad m)
  => (forall x. Render x -> Render x -> Render x)
  -> Spec w1 g1 m act1
  -> Spec w2 g2 m act2
  -> Spec
      (Store (Rec (UI w1 act1) (UI w2 act2)))
      (Co (Store (Rec (UI w1 act1) (UI w2 act2))))
      m
      (Either act1 act2)
combine' cobuild (Spec s1) (Spec s2) =
  Spec { space: store build { first: s1.space, second: s2.space }
       , pair: sym pairCo
       , performAction
       }
  where
    build { first, second } =
      cobuild (lmap (_ `compose` Left) (extract first)) (lmap (_ `compose` Right) (extract second))

    performAction :: PerformAction (Co (Store (Rec (UI w1 act1) (UI w2 act2)))) m (Either act1 act2)
    performAction = case _ of
      Left a' ->
        interpret (\(CR.Emit o r) ->
          let modifyFirst = modify \rc -> rc { first = s1.pair (const id) o (duplicate rc.first) }
          in CR.Emit modifyFirst r)
          (s1.performAction a')
      Right b ->
        interpret (\(CR.Emit o r) ->
          let modifySecond = modify \rc -> rc { second = s2.pair (const id) o (duplicate rc.second) }
          in CR.Emit modifySecond r)
          (s2.performAction b)

runUI
  :: forall eff w g action. Comonad w
  => Spec w g (Aff eff) action
  -> HTMLElement
  -> Aff (dom :: DOM | eff) Unit
runUI space el = void do
  let ui = createClass space
  liftEff $ RDOM.render (R.createFactory ui {}) $ htmlElementToElement el
