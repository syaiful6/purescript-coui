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


data CoF g (f :: * -> *) o m a
  = Explore (g Unit) a
  | Lift (m a)
  | Subscribe (ES.EventSource f m) a
  | Halt String
  | Raise o a
  | Par (CoAp g f o m a)
  | Fork (FF.Fork (CoM g f o m) a)
  | GetRef RefLabel (Maybe Foreign -> a)

instance functorCoF :: Functor m => Functor (CoF g f o m) where
  map f = case _ of
    Explore fa a -> Explore fa (f a)
    Lift m -> Lift (map f m)
    Subscribe es a -> Subscribe es (f a)
    Halt s -> Halt s
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork fa -> Fork (map f fa)
    GetRef label k -> GetRef label (map f k)

newtype CoAp g f o m a = CoAp (FreeAp (CoM g f o m) a)

derive instance newtypeCoAp :: Newtype (CoAp g f o m a) _
derive newtype instance functorCoAp :: Functor (CoAp g f o m)
derive newtype instance applyCoAp :: Apply (CoAp g f o m)
derive newtype instance applicativeCoAp :: Applicative (CoAp g f o m)

newtype CoM g f o m a = CoM (Free (CoF g f o m) a)

instance functorCoM :: Functor (CoM g f o m) where
  map f (CoM fa) = CoM (map f fa)

instance applyCoM :: Apply (CoM g f o m) where
  apply (CoM fa) (CoM fb) = CoM (apply fa fb)

instance applicativeCoM :: Applicative (CoM g f o m) where
  pure a = CoM (pure a)

instance bindCoM :: Bind (CoM g f o m) where
  bind (CoM fa) f = CoM (fa >>= \x -> case f x of CoM fb -> fb)

instance monadCoM :: Monad (CoM g f o m)

instance monadEffCoM :: MonadEff eff m => MonadEff eff (CoM g f o m) where
  liftEff eff = CoM $ liftF $ Lift $ liftEff eff

instance monadAffCoM :: MonadAff eff m => MonadAff eff (CoM g f o m) where
  liftAff aff = CoM $ liftF $ Lift $ liftAff aff

instance parallelCoM :: Parallel (CoAp g f o m) (CoM g f o m) where
  parallel = CoAp <<< liftFreeAp
  sequential = CoM <<< liftF <<< Par

instance monadForkCoM :: MonadAff eff m => MonadFork Error (CoM g f o m) where
  fork a = map liftAff <$> CoM (liftF $ Fork $ FF.fork a)

instance monadTransCoM :: MonadTrans (CoM f i o) where
  lift m = CoM $ liftF $ Lift m

instance monadRecCoM :: MonadRec (CoM g f o m) where
  tailRecM k a = k a >>= go
    where
    go (Loop x) = tailRecM k x
    go (Done y) = pure y

instance monadStateCoM :: MonadState s m => MonadState s (CoM g f o m) where
  state = lift <<< state

instance monadAskCoM :: MonadAsk r m => MonadAsk r (CoM g f o m) where
  ask = lift ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (CoM g f o m) where
  tell = lift <<< tell

halt :: forall g f o m a. String -> CoM g f o m a
halt msg = CoM $ liftF $ Halt msg

getRef :: forall g f o m. RefLabel -> CoM g f o m (Maybe Foreign)
getRef p = CoM $ liftF $ GetRef p id

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` function.
subscribe :: forall g f o m. ES.EventSource f m -> CoM g f o m Unit
subscribe es = CoM $ liftF $ Subscribe es unit

raise :: forall g f o m. o -> CoM g f o m Unit
raise o = CoM $ liftF $ Raise o unit

hoist
  :: forall g f o m m'
   . Functor m'
  => (m ~> m')
  -> CoM g f o m
  ~> CoM g f o m'
hoist nat (CoM fa) = CoM (hoistFree go fa)
  where
  go :: CoF g f o m ~> CoF g f o m'
  go = case _ of
    Explore fx a -> Explore fx a
    Lift m -> Lift (nat m)
    Subscribe es a -> Subscribe (ES.hoist nat es) a
    Halt s -> Halt s
    Raise o a -> Raise o a
    Par p -> Par (over CoAp (hoistFreeAp (hoist nat)) p)
    Fork f -> Fork (FF.hoistFork (hoist nat) f)
    GetRef p k -> GetRef p k
