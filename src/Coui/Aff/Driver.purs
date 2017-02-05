module Coui.Aff.Driver
  ( Driver
  , RenderSpec
  , runUI
  , module Coui.Aff.Effects
  ) where

import Prelude

import Control.Monad.Aff (Aff, forkAll, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef, newRef)
import Control.Coroutine (CoTransformer, CoTransform(CoTransform))
import Control.Monad.Free.Trans (resume)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)

import Data.Either (Either(..))
import Data.Lens (view)
import Data.List as L
import Data.List ((:))
import Data.Traversable (for_, sequence_)
import Data.Maybe (Maybe(..))

import Coui.Component (Component', _render, _action)
import Coui.Aff.Effects (CoreEffects)


type Driver f m = f -> m Unit

type RenderSpec h r s eff =
  { render
      :: forall f
       . (f -> Eff (CoreEffects eff) Unit)
      -> Array (h f)
      -> Maybe (r s f eff)
      -> Eff (CoreEffects eff) (r s f eff)
  }

type LifecycleHandlers eff =
  { initializers :: L.List (Aff (CoreEffects eff) Unit)
  , finalizers :: L.List (Aff (CoreEffects eff) Unit)
  }

runUI
  :: forall h r s f eff
   . RenderSpec h r s eff
  -> Component' (CoreEffects eff) h f s
  -> s
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI renderSpec component s = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  runUI' lchs renderSpec component s

runUI'
  :: forall h r s f eff
   . Ref (LifecycleHandlers eff)
  -> RenderSpec h r s eff
  -> Component' (CoreEffects eff) h f s
  -> s
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI' lchs renderSpec component s = do
  fresh <- liftEff $ newRef 0
  handleLifecycle lchs do
    ref <- runComponent component s
    pure (handleAction ref)

  where

  handleAction
    :: Ref (DriverState h r f s eff)
    -> f
    -> Aff (CoreEffects eff) Unit
  handleAction r q = do
    st <- liftEff (readRef r)
    tailRecM (stepCoroutine r) $ (view _action st.component) st.state q
    pure unit

  stepCoroutine
    :: Ref (DriverState h r f s eff)
    -> CoTransformer (Maybe s) (s -> s) (Aff (CoreEffects eff)) Unit
    -> Aff
        (CoreEffects eff)
        (Step (CoTransformer (Maybe s) (s -> s) (Aff (CoreEffects eff)) Unit) Unit)
  stepCoroutine ds cot = do
    e <- resume cot
    case e of
      Left _ -> pure (Done unit)
      Right (CoTransform f k) -> do
        st <- liftEff (readRef ds)
        let newstate = f st.state
        liftEff $ modifyRef ds \st' ->
          st { state = newstate }
        handleLifecycle lchs $ render ds
        pure $ Loop $ k $ Just newstate

  runComponent
    :: Component' (CoreEffects eff) h f s
    -> s
    -> Eff (CoreEffects eff) (Ref (DriverState h r f s eff))
  runComponent comp i = do
    var <- initDriverState comp i
    render var
    squashLifecycles var
    pure var

  render
    :: Ref (DriverState h r f s eff)
    -> Eff (CoreEffects eff) Unit
  render var = readRef var >>= \ds -> do
    let
      handler :: f -> Aff (CoreEffects eff) Unit
      handler = void <<< handleAction var
      selfHandler :: f -> Aff (CoreEffects eff) Unit
      selfHandler = queuingHandler handler ds.pendingRefs
    rendering <- renderSpec.render
      (handleAff <<< selfHandler) (view _render ds.component $ ds.state) ds.rendering
    modifyRef var \ds' ->
      ds' { rendering = Just rendering }

  squashLifecycles
    :: Ref (DriverState h r f s eff)
    -> Eff (CoreEffects eff) Unit
  squashLifecycles var = readRef var >>= \st -> do
    modifyRef lchs \handlers ->
      { initializers: pure $ do
          queue <- liftEff (readRef st.pendingRefs)
          liftEff $ writeRef st.pendingRefs Nothing
          for_ queue parSequence_
          parSequence_ (L.reverse handlers.initializers)
          handlePending st.pendingQueries
      , finalizers: handlers.finalizers
      }

  handlePending
    :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
    -> Aff (CoreEffects eff) Unit
  handlePending ref = do
    queue <- liftEff (readRef ref)
    liftEff $ writeRef ref Nothing
    for_ queue (forkAll <<< L.reverse)

handleLifecycle
  :: forall eff a
   . Ref (LifecycleHandlers eff)
  -> Eff (CoreEffects eff) a
  -> Aff (CoreEffects eff) a
handleLifecycle lchs f = do
  liftEff $ writeRef lchs { initializers: L.Nil, finalizers: L.Nil }
  result <- liftEff f
  { initializers, finalizers } <- liftEff $ readRef lchs
  forkAll finalizers
  sequence_ initializers
  pure result

queuingHandler
  :: forall a eff
   . (a -> Aff (CoreEffects eff) Unit)
  -> Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
  -> a
  -> Aff (CoreEffects eff) Unit
queuingHandler handler ref message = do
  queue <- liftEff (readRef ref)
  case queue of
    Nothing ->
      handler message
    Just p ->
      liftEff $ writeRef ref (Just (handler message : p))

handleAff
  :: forall eff a
   . Aff (CoreEffects eff) a
  -> Eff (CoreEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))

type DriverState h r f s eff =
  { component :: Component' (CoreEffects eff) h f s
  , state :: s
  , pendingRefs :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
  , pendingQueries :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
  , rendering :: Maybe (r s f eff)
  }

initDriverState
  :: forall h r f s eff
   . Component' (CoreEffects eff) h f s
  -> s
  -> Eff (CoreEffects eff) (Ref (DriverState h r f s eff))
initDriverState component s = do
  pendingRefs <- newRef (Just L.Nil)
  pendingQueries <- newRef (Just L.Nil)
  pendingOuts <- newRef (Just L.Nil)
  let
    ds =
      { component: component
      , state: s
      , pendingRefs
      , pendingQueries
      , rendering: Nothing
      }
  ref <- newRef ds
  pure $ ref
