module Coui.Aff.Driver.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Comonad (class Comonad, duplicate)
import Control.Monad.Aff (Aff, forkAll)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (Ref, readRef, modifyRef, writeRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork (fork)
import Control.Monad.Free (foldFree)
import Control.Parallel (parallel, sequential)

import Data.List (List, (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Traversable (sequence_)

import Coui.Action.ForkF as FF
import Coui.Action.CoM (CoM(..), CoF(..), CoAp(..))
import Coui.Action.CoT (pairCoTM_)
import Coui.Action.InputF (InputF(..), RefLabel(..))
import Coui.Aff.Driver.State (DriverState(..))
import Coui.Aff.Effects (CoreEffects)


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

type Renderer h r w f i o eff
  = Ref (DriverState h r w f i o eff)
  -> Eff (CoreEffects eff) Unit

eval
  :: forall h r w f i o eff
   . Comonad w
  => Ref (LifecycleHandlers eff)
  -> Renderer h r w f i o eff
  -> Ref (DriverState h r w f i o eff)
  -> InputF f
  -> Aff (CoreEffects eff) Unit
eval lchs render r = case _ of
  RefUpdate (RefLabel p) el -> do
    liftEff $ modifyRef r \(DriverState st) ->
      DriverState st { refs = SM.alter (const el) p st.refs }
    pure unit
  Action q -> do
    -- move to new space
    DriverState st <- liftEff (readRef r)
    case pairCoTM_ (const pure) (st.component.action q) (duplicate st.state) of
      CoM fx -> do
        newstate <- foldFree (go r) fx
        liftEff $ modifyRef r \(DriverState st) ->
          DriverState st { state = newstate }
        handleLifecycle lchs (render r)
        pure unit

  where

  go
    :: Ref (DriverState h r w f i o eff)
    -> CoF f o (Aff (CoreEffects eff))
    ~> Aff (CoreEffects eff)
  go ref = case _ of
    Lift mx ->
      mx
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

  evalM
    :: Ref (DriverState h r w f i o eff)
    -> CoM f o (Aff (CoreEffects eff))
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
