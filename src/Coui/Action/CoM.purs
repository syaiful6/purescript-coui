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

import Coui.Action.ForkF as FF
import Coui.Action.InputF (RefLabel)


data CoF f o m a
  = Lift (m a)
  | Halt String
  | Raise o a
  | Par (CoAp f o m a)
  | Fork (FF.Fork (CoM f o m) a)
  | GetRef RefLabel (Maybe Foreign -> a)

instance functorCoF :: Functor m => Functor (CoF f o m) where
  map f = case _ of
    Lift m -> Lift (map f m)
    Halt s -> Halt s
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork fa -> Fork (map f fa)
    GetRef label k -> GetRef label (map f k)

newtype CoAp f o m a = CoAp (FreeAp (CoM f o m) a)

derive instance newtypeCoAp :: Newtype (CoAp f o m a) _
derive newtype instance functorCoAp :: Functor (CoAp f o m)
derive newtype instance applyCoAp :: Apply (CoAp f o m)
derive newtype instance applicativeCoAp :: Applicative (CoAp f o m)

newtype CoM f o m a = CoM (Free (CoF f o m) a)

instance functorCoM :: Functor (CoM f o m) where
  map f (CoM fa) = CoM (map f fa)

instance applyCoM :: Apply (CoM f o m) where
  apply (CoM fa) (CoM fb) = CoM (apply fa fb)

instance applicativeCoM :: Applicative (CoM f o m) where
  pure a = CoM (pure a)

instance bindCoM :: Bind (CoM f o m) where
  bind (CoM fa) f = CoM (fa >>= \x -> case f x of CoM fb -> fb)

instance monadCoM :: Monad (CoM f o m)

instance monadEffCoM :: MonadEff eff m => MonadEff eff (CoM f o m) where
  liftEff eff = CoM $ liftF $ Lift $ liftEff eff

instance monadAffCoM :: MonadAff eff m => MonadAff eff (CoM f o m) where
  liftAff aff = CoM $ liftF $ Lift $ liftAff aff

instance parallelCoM :: Parallel (CoAp f o m) (CoM f o m) where
  parallel = CoAp <<< liftFreeAp
  sequential = CoM <<< liftF <<< Par

instance monadForkCoM :: MonadAff eff m => MonadFork Error (CoM f o m) where
  fork a = map liftAff <$> CoM (liftF $ Fork $ FF.fork a)

instance monadTransCoM :: MonadTrans (CoM f o) where
  lift m = CoM $ liftF $ Lift m

instance monadRecCoM :: MonadRec (CoM f o m) where
  tailRecM k a = k a >>= go
    where
    go (Loop x) = tailRecM k x
    go (Done y) = pure y

instance monadStateCoM :: MonadState s m => MonadState s (CoM f o m) where
  state = lift <<< state

instance monadAskCoM :: MonadAsk r m => MonadAsk r (CoM f o m) where
  ask = lift ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (CoM f o m) where
  tell = lift <<< tell

halt :: forall f o m a. String -> CoM f o m a
halt msg = CoM $ liftF $ Halt msg

getRef :: forall f o m. RefLabel -> CoM f o m (Maybe Foreign)
getRef p = CoM $ liftF $ GetRef p id

raise :: forall f o m. o -> CoM f o m Unit
raise o = CoM $ liftF $ Raise o unit

hoist
  :: forall f o m m'
   . Functor m'
  => (m ~> m')
  -> CoM f o m
  ~> CoM f o m'
hoist nat (CoM fa) = CoM (hoistFree go fa)
  where
  go :: CoF f o m ~> CoF f o m'
  go = case _ of
    Lift m -> Lift (nat m)
    Halt s -> Halt s
    Raise o a -> Raise o a
    Par p -> Par (over CoAp (hoistFreeAp (hoist nat)) p)
    Fork f -> Fork (FF.hoistFork (hoist nat) f)
    GetRef p k -> GetRef p k
