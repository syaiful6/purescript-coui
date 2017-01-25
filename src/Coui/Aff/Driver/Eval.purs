module Coui.Aff.Driver.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Comonad (class Comonad, extract, duplicate)
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
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))

import Coui.Aff.Driver.State (DriverState(..))
import Coui.Aff.Effects (CoreEffects)
import Coui.Action.EventSource as ES
import Coui.Action.ForkF as FF
import Coui.Action.HalogenM (CoM(..), CoF(..), CoAp(..))
import Coui.Action.InputF (InputF(..), RefLabel(..))


type LifecycleHandlers eff =
  { initializers :: List (Aff (CoreEffects eff) Unit)
  , finalizers :: List (Aff (CoreEffects eff) Unit)
  }

type Renderer h r eff
  = forall w f i o
   . Ref (DriverState h r w f i o eff)
  -> Eff (HalogenEffects eff) Unit

eval
  :: forall h r w f i o a eff
   . Ref (LifecycleHandlers eff)
  -> Renderer h r eff
  -> Ref (DriverState h r w f i o eff)
  -> InputF a i
  -> Aff (CoreEffects eff) a
eval lchs render r = case _ of
  RefUpdate (RefLabel p) el next -> do
    liftEff $ modifyRef r \(DriverState st) ->
      DriverState st { refs = SM.alter (const el) p st.refs }
    pure next
  Query q -> evalF r q
  where

    go
      :: forall w' f' i' o' eff
       . Ref (DriverState h' r' w' f' i' o' eff)
      -> CoF f' i' o' (Aff (HalogenEffects eff))
      ~> Aff (HalogenEffects eff)
    go ref = case _ of
      Explore fa next -> do
        DriverState (st@{ component, space }) <- liftEff (readRef ref)
        let newspace = component.pair (const id) fa (duplicate space)
        liftEff $ writeRef ref (DriverState (st { space = newspace }))
        handleLifecycle lchs (render ref)
        pure next
      Lift eff ->
        eff
      Subscribe es next -> do
        forkAff do
          { producer, done } <- ES.unEventSource es
          let
            consumer = do
              s <- CR.await
              case s of
                ES.More i -> lift (evalF ref i) *> consumer
                _ -> pure unit
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
      :: forall w' f' i' o'
       . Ref (DriverState h r w' f' i' o' eff)
      -> i'
      -> Aff (CoreEffects eff) a
    evalF ref i = do
      DriverState ds <- liftEff (readRef ref)
      case ds.component.performAction i of CoM fx -> foldFree (go ref) fx

    evalM
      :: forall w' f' i' o'
       . Ref (DriverState h r w' f' i' o' eff)
      -> CoM f' i' o' (Aff (CoreEffects eff))
      ~> Aff (CoreEffects eff)
    evalM ref (CoM fx) = foldFree (go ref) fx

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
