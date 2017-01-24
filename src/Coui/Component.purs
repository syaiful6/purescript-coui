module Coui.Component
  ( PerformAction
  , defaultDispatcher
  , explore
  , EventHandler
  , Render
  , UI
  , Spec(..)
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
import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free.Trans (hoistFreeT, interpret, resume)
import Control.Monad.Rec.Class (Step(..), tailRecM, forever)

import Data.Bifunctor as B
import Data.Either (Either(..), either)
import Data.Functor.Day (Day, day, runDay, pairDay)
import Data.Functor.Pairing (Pairing, sym)
import Data.Functor.Pairing.Co (Co, pairCo, runCo, co)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lmap)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import React as R
import React.DOM as RD
import ReactDOM as RDOM


type PerformAction g m action =
  action -> CR.CoTransformer (Maybe (g Unit)) (g Unit) m Unit

defaultDispatcher :: forall g m action. Monad m => PerformAction g m action
defaultDispatcher _ = pure unit

explore :: forall g m. Monad m => g Unit -> CR.CoTransformer (Maybe (g Unit)) (g Unit) m (Maybe (g Unit))
explore = CR.cotransform

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

          step :: CR.CoTransformer (Maybe (g Unit)) (g Unit) (Aff eff) Unit
               -> Aff eff (Step (CR.CoTransformer (Maybe (g Unit)) (g Unit) (Aff eff) Unit) Unit)
          step cot = do
            e <- resume cot
            case e of
              Left _ -> pure (Done unit)
              Right (CR.CoTransform m' k) -> do
                st <- liftEff (coerceEff (R.readState this))
                let ns = sp.pair (const id) m' (duplicate st)
                makeAff \_ k1 -> unsafeCoerceEff do
                  void $ R.writeStateWithCallback this ns (unsafeCoerceEff (k1 ns))
                pure (Loop (k (Just m')))

          cotransformer :: CR.CoTransformer (Maybe (g Unit)) (g Unit) (Aff eff) Unit
          cotransformer = sp.performAction action

      unsafeCoerceEff (launchAff (tailRecM step cotransformer))

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

runUI
  :: forall w g action eff. Comonad w
  => Spec w g (Aff eff) action
  -> HTMLElement
  -> Aff (dom :: DOM | eff) Unit
runUI space el = void do
  let ui = createClass space
  liftEff $ RDOM.render (R.createFactory ui {}) $ htmlElementToElement el
