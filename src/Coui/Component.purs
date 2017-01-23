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
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Comonad (class Comonad, extract, duplicate)
import Control.Monad.Aff (Aff, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free.Trans (resume)
import Control.Monad.Rec.Class (Step(..), tailRecM)

import Data.Either (Either(..))
import Data.Functor.Pairing (Pairing, sym)
import Data.Functor.Pairing.Co (Co, pairCo)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import React as R
import React.DOM as RD
import ReactDOM as RDOM


type PerformAction eff m action =
  action -> CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) Unit

defaultDispatcher :: forall eff action m. PerformAction eff m action
defaultDispatcher _ = pure unit

explore :: forall eff m. m Unit -> CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) (Maybe (m Unit))
explore = CR.cotransform

type EventHandler =
  forall eff refs.
    Eff ( props :: R.ReactProps
        , state :: R.ReactState R.ReadWrite
        , refs :: R.ReactRefs refs
        | eff
        ) Unit

type Render props action = props -> (action -> EventHandler) -> Array R.ReactElement

type UI w props action = w (Render props action)

newtype Spec eff w m props action = Spec
  { space          :: UI w props action
  , performAction  :: PerformAction eff m action
  , pair           :: Pairing m w
  }

-- | Manually create Spec by provides all spec field. See simpleSpec that use Co if you
-- | don't want use explicit Pairing
spec
  :: forall eff w m props action
   . PerformAction eff m action
  -> Pairing m w
  -> UI w props action
  -> Spec eff w m props action
spec performAction pair space = Spec { space, performAction, pair }

-- | An easy way to create spec by just provides UI and PerformAction.
simpleSpec
  :: forall eff w props action. Functor w
  => PerformAction eff (Co w) action
  -> UI w props action
  -> Spec eff w (Co w) props action
simpleSpec performAction space = Spec { space, performAction, pair: sym pairCo }

createClass
  :: forall eff w m props action. Comonad w
  => Spec eff w m props action -> R.ReactClass props
createClass spec' = R.createClass <<< _.spec $ createReactSpec spec'

createReactSpec
  :: forall eff w m props action. Comonad w
  => Spec eff w m props action
  -> { spec :: R.ReactSpec props (UI w props action) eff
     , eval :: R.ReactThis props (UI w props action) -> action -> EventHandler
     }
createReactSpec = createReactSpec' RD.div'

createReactSpec'
  :: forall eff w m props action. Comonad w
  => (Array R.ReactElement -> R.ReactElement)
  -> Spec eff w m props action
  -> { spec :: R.ReactSpec props (UI w props action) eff
     , eval :: R.ReactThis props (UI w props action) -> action -> EventHandler
     }
createReactSpec' wrap (Spec sp) =
  { spec: R.spec sp.space render
  , eval: dispatcher
  }
  where
    dispatcher :: R.ReactThis props (UI w props action) -> action -> EventHandler
    dispatcher this action = void do
      let coerceEff :: forall eff1 a. Eff eff1 a -> Eff eff a
          coerceEff = unsafeCoerceEff

          step :: CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) Unit
               -> Aff eff (Step (CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) Unit) Unit)
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

          cotransformer :: CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) Unit
          cotransformer = sp.performAction action

      unsafeCoerceEff (launchAff (tailRecM step cotransformer))

    render :: R.Render props (UI w props action) eff
    render this = do
      state <- R.readState this
      props <- R.getProps this
      pure (wrap $ extract state props (dispatcher this))

runUI
  :: forall w m props action eff. Comonad w
  => Spec eff w m props action
  -> props
  -> HTMLElement
  -> Aff (dom :: DOM | eff) Unit
runUI space props el = void do
  let ui = createClass space
  liftEff $ RDOM.render (R.createFactory ui props) $ htmlElementToElement el
