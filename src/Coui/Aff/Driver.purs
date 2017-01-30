module Coui.Aff.Driver
  ( Driver
  , RenderSpec
  , runUI
  , module Coui.Aff.Effects
  ) where

import Prelude

import Control.Comonad (class Comonad, extract, duplicate)
import Control.Monad.Aff (Aff, forkAll, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef, writeRef, newRef)
import Control.Parallel (parSequence_)

import Data.List as L
import Data.List ((:))
import Data.Traversable (for_, sequence_)
import Data.Maybe (Maybe(..))

import Unsafe.Coerce (unsafeCoerce)

import Coui.Action.CoT (pairCoTM_)
import Coui.Component (Component(..), Component')
import Coui.Aff.Effects (CoreEffects)


type Driver f m = f -> m Unit

type RenderSpec h r eff =
  { render
      :: forall f
       . (f -> Eff (CoreEffects eff) Unit)
      -> h Void f
      -> Maybe (r f eff)
      -> Eff (CoreEffects eff) (r f eff)
  }

type LifecycleHandlers eff =
  { initializers :: L.List (Aff (CoreEffects eff) Unit)
  , finalizers :: L.List (Aff (CoreEffects eff) Unit)
  }

runUI
  :: forall h r w f eff. Comonad w
  => RenderSpec h r eff
  -> Component w (Aff (CoreEffects eff)) h f
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI renderSpec component = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  runUI' lchs renderSpec component

runUI'
  :: forall h r w f eff. Comonad w
  => Ref (LifecycleHandlers eff)
  -> RenderSpec h r eff
  -> Component w (Aff (CoreEffects eff)) h f
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI' lchs renderSpec component = do
  fresh <- liftEff $ newRef 0
  handleLifecycle lchs do
    runComponent component
      >>= readRef
      >>= unDriverState >>> \st ->
        pure (handleAction st.selfRef)

  where

  handleAction
    :: Ref (DriverState h r w f eff)
    -> f
    -> Aff (CoreEffects eff) Unit
  handleAction r q = do
    DriverState st <- liftEff (readRef r)
    newstate <-  pairCoTM_ (const pure) (st.component.action q) (duplicate st.state)
    liftEff $
      modifyRef r \(DriverState st) ->
        DriverState st { state = newstate }
    handleLifecycle lchs (render r)
    pure unit

  runComponent
    :: Component w (Aff (CoreEffects eff)) h f
    -> Eff (CoreEffects eff) (Ref (DriverState h r w f eff))
  runComponent comp = do
    var <- initDriverState comp
    render <<< _.selfRef <<< unDriverState =<< readRef var
    squashLifecycles var
    pure var

  render
    :: Ref (DriverState h r w f eff)
    -> Eff (CoreEffects eff) Unit
  render var = readRef var >>= \(DriverState ds) -> do
    let
      handler :: f -> Aff (CoreEffects eff) Unit
      handler = void <<< handleAction ds.selfRef
      selfHandler :: f -> Aff (CoreEffects eff) Unit
      selfHandler = queuingHandler handler ds.pendingRefs
    rendering <- renderSpec.render (handleAff <<< selfHandler) (extract ds.state) ds.rendering
    modifyRef var \(DriverState ds') ->
      DriverState (ds' { rendering = Just rendering })

  squashLifecycles
    :: Ref (DriverState h r w f eff)
    -> Eff (CoreEffects eff) Unit
  squashLifecycles var = readRef var >>= \(DriverState st) -> do
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

newtype DriverState h r w f eff = DriverState (DriverStateR h r w f eff)

type DriverStateR h r w f eff =
  { component :: Component' w (Aff (CoreEffects eff)) h f
  , state :: w (h Void f)
  , selfRef :: Ref (DriverState h r w f eff)
  , pendingRefs :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
  , pendingQueries :: Ref (Maybe (L.List (Aff (CoreEffects eff) Unit)))
  , rendering :: Maybe (r f eff)
  }

unDriverState
  :: forall h r w f eff
   . DriverState h r w f eff
  -> DriverStateR h r w f eff
unDriverState (DriverState ds) = ds

initDriverState
  :: forall h r w f eff
   . Component w (Aff (CoreEffects eff)) h f
  -> Eff (CoreEffects eff) (Ref (DriverState h r w f eff))
initDriverState (Component comp@{ ui }) = do
  selfRef <- newRef (unsafeCoerce {})
  pendingRefs <- newRef (Just L.Nil)
  pendingQueries <- newRef (Just L.Nil)
  pendingOuts <- newRef (Just L.Nil)
  let
    ds =
      { component: comp
      , state: ui
      , selfRef
      , pendingRefs
      , pendingQueries
      , rendering: Nothing
      }
  writeRef selfRef (DriverState ds)
  pure $ selfRef
