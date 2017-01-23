module Coui.Component where

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
import Data.Functor.Pairing (Pairing)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML.Types (HTMLElement, htmlElementToElement)

import React as R
import React.DOM as RD
import ReactDOM as RDOM

type Eval eff action m =
  action -> CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) Unit

defaultDispatcher :: forall eff action m. Eval eff action m
defaultDispatcher _ = pure unit

modifyState :: forall eff m. m Unit -> CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) (Maybe (m Unit))
modifyState = CR.cotransform

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
  { space :: UI w props action
  , eval  :: Eval eff action m
  , pair  :: Pairing m w
  }

simpleSpec
  :: forall eff w m props action
   . Eval eff action m
  -> Pairing m w
  -> UI w props action
  -> Spec eff w m props action
simpleSpec eval pair space = Spec { space, eval, pair }

createClass
  :: forall eff w m props action. Comonad w
  => Spec eff w m props action -> R.ReactClass props
createClass spec = R.createClass <<< _.spec $ createReactSpec spec

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
createReactSpec' wrap (Spec spec) =
  { spec: R.spec spec.space render
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
                let ns = spec.pair (const id) m' (duplicate st)
                makeAff \_ k1 -> unsafeCoerceEff do
                  void $ R.writeStateWithCallback this ns (unsafeCoerceEff (k1 ns))
                pure (Loop (k (Just m')))

          cotransformer :: CR.CoTransformer (Maybe (m Unit)) (m Unit) (Aff eff) Unit
          cotransformer = spec.eval action

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
