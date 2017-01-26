module Coui.Aff.Driver
  ( RenderSpec
  , runUI
  , module Coui
  , module Coui.Aff.Effects
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Comonad (class Comonad, extract)
import Control.Monad.Aff (Aff, forkAff, forkAll, runAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Parallel (parSequence_)

import Data.List as L
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, sequence_)

import Coui (CouiIO)
import Coui.Aff.Driver.Eval (LifecycleHandlers, eval, handleLifecycle, queuingHandler)
import Coui.Aff.Driver.State (DriverState(..), DriverStateX, initDriverState, unDriverStateX)
import Coui.Aff.Effects (CoreEffects)
import Coui.Action.InputF (InputF(..))
import Coui.Component (Component)


type RenderSpec h r eff =
  forall r i. (forall x. InputF x i -> Eff (CoreEffects eff) Unit)
  -> h i
  -> Maybe (r i eff)
  -> Eff (CoreEffects eff) (r i eff)

runUI
  :: forall h r w f i o eff. Comonad w
  => RenderSpec h r eff
  -> Component h w f i o (Aff (CoreEffects eff))
  -> Aff (CoreEffects eff) (CouiIO i o (Aff (CoreEffects eff)))
runUI renderSpec component = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  runUI' lchs renderSpec component

runUI'
  :: forall h r w f i o eff. Comonad w
   => Ref (LifecycleHandlers eff)
  -> RenderSpec h r eff
  -> Component h w f i o (Aff (CoreEffects eff))
  -> Aff (CoreEffects eff) (CouiIO i o (Aff (CoreEffects eff)))
runUI' lchs renderSpec component = do
  fresh <- liftEff $ newRef 0
  handleLifecycle lchs do
    listeners <- newRef M.empty
    runComponent (rootHandler listeners) component
      >>= readRef
      >>= unDriverStateX \st ->
        pure
          { dispatch: evalDriver st.selfRef
          , subscribe: subscribe fresh listeners
          }

  where
  evalDriver ref q = case prjQuery q of
      Just q' -> evalF ref (Query q')
      Nothing -> liftEff $ throwException (error "Halogen internal error: query projection failed in runUI'")

  evalF ref = eval lchs render ref

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

  rootHandler ref message = do
    listeners <- liftEff $ readRef ref
    void $ forkAll $ map (\var -> AV.putVar var message) listeners

  runComponent
    :: forall z f' i' o'
     . (o' -> Aff (CoreEffects eff) Unit)
    -> i'
    -> (i' -> Maybe z)
    -> Component h z i' o' (Aff (CoreEffects eff))
    -> Eff (HalogenEffects eff) (Ref (DriverStateX h r i' eff))
  runComponent handler c prjQuery = do
    var <- initDriverState c handler prjQuery
    unDriverStateX (render <<< _.selfRef) =<< readRef var
    squashChildInitializers =<< readRef var
    pure var

  render
    :: forall w' f' i' o'
     . Ref (DriverState h r w' f' i' o' eff)
    -> Eff (CoreEffects eff) Unit
  render var = readRef var >>= \(DriverState ds) -> do
    let
      handler = void <<< evalF ds.selfRef

      selfHandler = queuingHandler handler ds.pendingRefs

    rendering <- renderSpec (handleAff <<< selfHandler) (extract ds.space) ds.rendering
    modifyRef var \(DriverState ds') ->
      DriverState (ds' { rendering = Just rendering })

  squashChildInitializers = unDriverStateX \ds do
    let rootInitializer = evalF ds.selfRef <<< Query <$> ds.component.initializer
    modifyRef lchs \handlers ->
      { initializers: pure $ do
          queue <- liftEff (readRef ds.pendingRefs)
          liftEff $ writeRef ds.pendingRefs Nothing
          for_ queue parSequence_
          parSequence_ (L.reverse handlers.initializers)
          sequence_ rootInitializer
          handlePending ds.pendingQueries
          handlePending ds.pendingOuts
      , finalizers: handlers.finalizers
      }


handlePending
  :: forall eff. Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
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
