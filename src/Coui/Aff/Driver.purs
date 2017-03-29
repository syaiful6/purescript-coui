module Coui.Aff.Driver
  ( Driver
  , RenderSpec
  , runUI
  , module Coui.Aff.Effects
  ) where

import Prelude

import Control.Monad.Aff (Aff, forkAll, forkAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef, newRef)
import Data.Newtype (unwrap)
import Control.Parallel (parSequence_)

import Data.List as L
import Data.List ((:))
import Data.Traversable (for_, sequence_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import Coui.Component (Component', Complet(..), Action)
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
  -> Component' (Aff (CoreEffects eff)) h f s
  -> s
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI renderSpec component s = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  runUI' lchs renderSpec component s

runUI'
  :: forall h r s f eff
   . Ref (LifecycleHandlers eff)
  -> RenderSpec h r s eff
  -> Component' (Aff (CoreEffects eff)) h f s
  -> s
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI' lchs renderSpec component s = do
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
    case st.handler q of
      Tuple (Just newstate) message ->
        case unwrap st.component newstate of
          Complet (Tuple vi hd) -> do
            liftEff $ modifyRef r \st' ->
              st' { handler = hd }
            handleLifecycle lchs $ render r vi
            void $ forkAff $ message >>= maybe (pure unit) (step r)
      Tuple Nothing message ->
        void $ forkAff $ message >>= maybe (pure unit) (step r)

  step :: Ref (DriverState h r f s eff) -> f -> Aff (CoreEffects eff) Unit
  step var msg = do
    st <- liftEff (readRef var)
    queuingHandler (handleAction var) st.pendingQueries msg

  runComponent
    :: Component' (Aff (CoreEffects eff)) h f s
    -> s
    -> Eff (CoreEffects eff) (Ref (DriverState h r f s eff))
  runComponent comp i = do
    Tuple vi var <- initDriverState comp i
    render var vi *> squashLifecycles var *> pure var

  render
    :: Ref (DriverState h r f s eff)
    -> Array (h f)
    -> Eff (CoreEffects eff) Unit
  render var vi = readRef var >>= \ds -> do
    let
      handler :: f -> Aff (CoreEffects eff) Unit
      handler = handleAction var
    rendering <- renderSpec.render (handleAff <<< handler) vi ds.rendering
    modifyRef var \st ->
      st { rendering = Just rendering }

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
  { component :: Component' (Aff (CoreEffects eff)) h f s
  , handler :: Action (Aff (CoreEffects eff)) f s
  , pendingRefs :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
  , pendingQueries :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
  , rendering :: Maybe (r s f eff)
  }

initDriverState
  :: forall h r f s eff
   . Component' (Aff (CoreEffects eff)) h f s
  -> s
  -> Eff (CoreEffects eff) (Tuple (Array (h f)) (Ref (DriverState h r f s eff)))
initDriverState component s = do
  pendingRefs <- newRef (Just L.Nil)
  pendingQueries <- newRef (Just L.Nil)
  pendingOuts <- newRef (Just L.Nil)
  case unwrap component s of
    Complet (Tuple vi handler) -> do
      let
        ds =
          { component: component
          , handler
          , pendingRefs
          , pendingQueries
          , rendering: Nothing
          }
      ref <- newRef ds
      pure $ Tuple vi ref
