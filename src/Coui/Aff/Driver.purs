module Coui.Aff.Driver
  ( CouiIO
  , RenderSpec
  , runUI
  , module Coui.Aff.Effects
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll, runAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef, newRef)
import Control.Parallel (parSequence_)

import Data.List as L
import Data.Either (Either(..))
import Data.Traversable (for_)
import Data.Map as M
import Data.Maybe (Maybe(..))

import Coui.Action.InputF (InputF(..))
import Coui.Component (Component)
import Coui.Aff.Driver.Eval (LifecycleHandlers, eval, handleLifecycle, queuingHandler)
import Coui.Aff.Driver.State (DriverState(..), initDriverState, unDriverState)
import Coui.Aff.Effects (CoreEffects)


type CouiIO f o m =
  { action :: f -> m Unit
  , subscribe :: CR.Consumer o m Unit -> m Unit
  }

type RenderSpec h r eff =
  { render
      :: forall f o
       . (InputF f -> Eff (CoreEffects eff) Unit)
      -> h Void f
      -> Maybe (r f o eff)
      -> Eff (CoreEffects eff) (r f o eff)
  }

runUI
  :: forall h r w f i o eff. Comonad w
  => RenderSpec h r eff
  -> Component h w f i o (Aff (CoreEffects eff))
  -> i
  -> Aff (CoreEffects eff) (CouiIO f o (Aff (CoreEffects eff)))
runUI renderSpec component j = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  runUI' lchs renderSpec component j

runUI'
  :: forall h r w f i o eff. Comonad w
  => Ref (LifecycleHandlers eff)
  -> RenderSpec h r eff
  -> Component h w f i o (Aff (CoreEffects eff))
  -> i
  -> Aff (CoreEffects eff) (CouiIO f o (Aff (CoreEffects eff)))
runUI' lchs renderSpec component i = do
  fresh <- liftEff $ newRef 0
  handleLifecycle lchs do
    listeners <- newRef M.empty
    runComponent (dispatcher listeners) i component
      >>= readRef
      >>= unDriverState >>> \st ->
        pure
          { subscribe: subscribe fresh listeners
          , action: evalF st.selfRef <<< Action
          }
  where

  dispatcher
    :: Ref (M.Map Int (AV.AVar o))
    -> o
    -> Aff (CoreEffects eff) Unit
  dispatcher ref message = do
    listeners <- liftEff $ readRef ref
    void $ forkAll $ map (\var -> AV.putVar var message) listeners

  evalF
    :: Ref (DriverState h r w f i o eff)
    -> InputF f
    -> Aff (CoreEffects eff) Unit
  evalF ref = eval lchs render ref

  subscribe
    :: Ref Int
    -> Ref (M.Map Int (AV.AVar o))
    -> CR.Consumer o (Aff (CoreEffects eff)) Unit
    -> Aff (CoreEffects eff) Unit
  subscribe fresh ref consumer = do
    inputVar <- AV.makeVar
    listenerId <- liftEff do
      listenerId <- readRef fresh
      modifyRef fresh (_ + 1)
      modifyRef ref (M.insert listenerId inputVar)
      pure listenerId
    let producer = CR.producer (Left <$> AV.takeVar inputVar)
    void $ forkAff do
      CR.runProcess (CR.connect producer consumer)
      liftEff $ modifyRef ref (M.delete listenerId)
      AV.killVar inputVar (error "ended")

  runComponent
    :: (o -> Aff (CoreEffects eff) Unit)
    -> i
    -> Component h w f i o (Aff (CoreEffects eff))
    -> Eff (CoreEffects eff) (Ref (DriverState h r w f i o eff))
  runComponent handler inp comp = do
    var <- initDriverState comp inp handler
    render <<< _.selfRef <<< unDriverState =<< readRef var
    squashLifecycles var
    pure var

  render
    :: Ref (DriverState h r w f i o eff)
    -> Eff (CoreEffects eff) Unit
  render var = readRef var >>= \(DriverState ds) -> do
    let
      handler :: InputF f -> Aff (CoreEffects eff) Unit
      handler = void <<< evalF ds.selfRef
      selfHandler :: InputF f -> Aff (CoreEffects eff) Unit
      selfHandler = queuingHandler handler ds.pendingRefs
    rendering <- renderSpec.render (handleAff <<< selfHandler) (extract ds.state) ds.rendering
    modifyRef var \(DriverState ds') ->
      DriverState
        { rendering: Just rendering
        , state: ds'.state
        , component: ds'.component
        , refs: ds'.refs
        , selfRef: ds'.selfRef
        , handler: ds'.handler
        , pendingRefs: ds'.pendingRefs
        , pendingQueries: ds'.pendingQueries
        , pendingOuts: ds'.pendingOuts
        }

  squashLifecycles
    :: Ref (DriverState h r w f i o eff)
    -> Eff (CoreEffects eff) Unit
  squashLifecycles var = readRef var >>= \(DriverState st) -> do
    modifyRef lchs \handlers ->
      { initializers: pure $ do
          queue <- liftEff (readRef st.pendingRefs)
          liftEff $ writeRef st.pendingRefs Nothing
          for_ queue parSequence_
          parSequence_ (L.reverse handlers.initializers)
          handlePending st.pendingQueries
          handlePending st.pendingOuts
      , finalizers: handlers.finalizers
      }

  handlePending
    :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
    -> Aff (CoreEffects eff) Unit
  handlePending ref = do
    queue <- liftEff (readRef ref)
    liftEff $ writeRef ref Nothing
    for_ queue (forkAll <<< L.reverse)

handleAff
  :: forall eff a
   . Aff (CoreEffects eff) a
  -> Eff (CoreEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))
