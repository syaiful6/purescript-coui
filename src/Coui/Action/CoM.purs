module Coui.Action.CoM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, hoistFreeAp)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Parallel.Class (class Parallel)

import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over)

import Coui.Action.EventSource as ES
import Coui.Action.ForkF as FF
import Coui.Action.InputF (RefLabel)


data CoF f i o m a
  = Explore (f Unit) a
  | Lift (m a)
  | Subscribe (ES.EventSource m i) a
  | Halt String
  | Raise o a
  | Par (CoAp f i o m a)
  | Fork (FF.Fork (CoM f i o m) a)
  | GetRef RefLabel (Maybe Foreign -> a)

instance functorCoF :: Functor m => Functor (CoF f i o m) where
  map f = case _ of
    Explore fa a -> Explore fa (f a)
    Lift m -> Lift (map f m)
    Subscribe es a -> Subscribe es (f a)
    Halt s -> Halt s
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork fa -> Fork (map f fa)
    GetRef label k -> GetRef label (map f k)

newtype CoAp f i o m a = CoAp (FreeAp (CoM f i o m) a)

derive instance newtypeCoAp :: Newtype (CoAp f i o m a) _
derive newtype instance functorCoAp :: Functor (CoAp f i o m)
derive newtype instance applyCoAp :: Apply (CoAp f i o m)
derive newtype instance applicativeCoAp :: Applicative (CoAp f i o m)

newtype CoM f i o m a = CoM (Free (CoF f i o m) a)

instance functorCoM :: Functor (CoM f i o m) where
  map f (CoM fa) = CoM (map f fa)

instance applyCoM :: Apply (CoM f i o m) where
  apply (CoM fa) (CoM fb) = CoM (apply fa fb)

instance applicativeCoM :: Applicative (CoM f i o m) where
  pure a = CoM (pure a)

instance bindCoM :: Bind (CoM f i o m) where
  bind (CoM fa) f = CoM (fa >>= \x -> case f x of CoM fb -> fb)

instance monadCoM :: Monad (CoM f i o m)

instance monadEffCoM :: MonadEff eff m => MonadEff eff (CoM f i o m) where
  liftEff eff = CoM $ liftF $ Lift $ liftEff eff

instance monadAffCoM :: MonadAff eff m => MonadAff eff (CoM f i o m) where
  liftAff aff = CoM $ liftF $ Lift $ liftAff aff

instance parallelCoM :: Parallel (CoAp f i o m) (CoM f i o m) where
  parallel = CoAp <<< liftFreeAp
  sequential = CoM <<< liftF <<< Par

instance monadForkCoM :: MonadAff eff m => MonadFork Error (CoM f i o m) where
  fork a = map liftAff <$> CoM (liftF $ Fork $ FF.fork a)

instance monadTransCoM :: MonadTrans (CoM f i o) where
  lift m = CoM $ liftF $ Lift m

instance monadRecCoM :: MonadRec (CoM f i o m) where
  tailRecM k a = k a >>= go
    where
    go (Loop x) = tailRecM k x
    go (Done y) = pure y

instance monadStateCoM :: MonadState s m => MonadState s (CoM f i o m) where
  state = lift <<< state

instance monadAskCoM :: MonadAsk r m => MonadAsk r (CoM f i o m) where
  ask = lift ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (CoM f i o m) where
  tell = lift <<< tell

halt :: forall f i o m a. String -> CoM f i o m a
halt msg = CoM $ liftF $ Halt msg

getRef :: forall f i o m. RefLabel -> CoM f i o m (Maybe Foreign)
getRef p = CoM $ liftF $ GetRef p id

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` function.
subscribe :: forall f i o m. ES.EventSource m i -> CoM f i o m Unit
subscribe es = CoM $ liftF $ Subscribe es unit

raise :: forall f i o m. o -> CoM f i o m Unit
raise o = CoM $ liftF $ Raise o unit

hoist
  :: forall f i o m m'
   . Functor m'
  => (m ~> m')
  -> CoM f i o m
  ~> CoM f i o m'
hoist nat (CoM fa) = CoM (hoistFree go fa)
  where
  go :: CoF f i o m ~> CoF f i o m'
  go = case _ of
    Explore fx a -> Explore fx a
    Lift m -> Lift (nat m)
    Subscribe es a -> Subscribe (ES.hoist nat es) a
    Halt s -> Halt s
    Raise o a -> Raise o a
    Par p -> Par (over CoAp (hoistFreeAp (hoist nat)) p)
    Fork f -> Fork (FF.hoistFork (hoist nat) f)
    GetRef p k -> GetRef p k
