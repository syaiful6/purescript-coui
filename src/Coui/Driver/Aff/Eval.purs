module Coui.Driver.Aff.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Comonad (class Comonad, duplicate)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (Ref, readRef, modifyRef, writeRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork (fork)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)

import Data.List (List, (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Traversable (sequence_)

import Coui.Action.EventSource as ES
import Coui.Action.ForkF as FF
import Coui.Action.CoM (CoM(..), CoF(..), CoAp(..))
import Coui.Action.InputF (InputF(..), RefLabel(..))
import Coui.Component (Component(..))
import Coui.Driver.Aff.State (DriverState(..))
import Coui.Effects (CoreEffects)


type LifecycleHandlers eff =
  { initializers :: List (Aff (CoreEffects eff) Unit)
  , finalizers :: List (Aff (CoreEffects eff) Unit)
  }

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

type Renderer h r w g f i o eff
  = Ref (DriverState h r w g f i o eff)
  -> Eff (CoreEffects eff) Unit

eval
  :: forall h r w g f i o a eff
   . Comonad w
  => Ref (LifecycleHandlers eff)
  -> Renderer h r w g f i o eff
  -> Ref (DriverState h r w g f i o eff)
  -> InputF a (f a)
  -> Aff (CoreEffects eff) a
eval lchs render r = case _ of
  RefUpdate (RefLabel p) el next -> do
    liftEff $ modifyRef r \(DriverState st) ->
      DriverState st { refs = SM.alter (const el) p st.refs }
    pure next
  Query q -> evalF r q

  where

  go
    :: Ref (DriverState h r w g f i o eff)
    -> CoF g f o (Aff (CoreEffects eff))
    ~> Aff (CoreEffects eff)
  go ref = case _ of
    Explore gx a -> do
      DriverState (st@{ state, component }) <- liftEff (readRef ref)
      case component of
        Component spec -> do
          let state' = spec.pair (const id) gx (duplicate state)
          liftEff $ writeRef ref (DriverState (st { state = state' }))
          handleLifecycle lchs (render ref)
          pure a
    Lift mx ->
      mx
    Subscribe es next -> do
      forkAff do
        { producer, done } <- ES.unEventSource es
        let
          consumer = do
            s <- lift <<< evalF ref =<< CR.await
            when (s == ES.Listening) consumer
        CR.runProcess (consumer `CR.pullFrom` producer)
        done
      pure next
    Halt msg ->
      throwError (error msg)
    Raise o a -> do
      DriverState { handler, pendingOuts } <- liftEff (readRef ref)
      queuingHandler handler pendingOuts o
      pure a
    Par (CoAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
    Fork f ->
      FF.unFork (\(FF.ForkF fx k) ->
        k <<< map unsafeCoerceAff <$> fork (evalM ref fx)) f
    GetRef (RefLabel p) k -> do
      DriverState { refs } <- liftEff (readRef ref)
      pure $ k $ SM.lookup p refs

  evalF
    :: Ref (DriverState h r w g f i o eff)
    -> f
    ~> Aff (CoreEffects eff)
  evalF ref q = do
    DriverState { component } <- liftEff (readRef ref)
    case component of Component spec -> case spec.action q of CoM fx -> foldFree (go ref) fx

  evalM
    :: Ref (DriverState h r w g f i o eff)
    -> CoM g f o (Aff (CoreEffects eff))
    ~> Aff (CoreEffects eff)
  evalM ref (CoM q) = foldFree (go ref) q

queuingHandler
  :: forall a eff
   . (a -> Aff (CoreEffects eff) Unit)
  -> Ref (Maybe (List (Aff (CoreEffects eff) Unit)))
  -> a
  -> Aff (CoreEffects eff) Unit
queuingHandler handler ref message = do
  queue <- liftEff (readRef ref)
  case queue of
    Nothing ->
      handler message
    Just p ->
      liftEff $ writeRef ref (Just (handler message : p))
