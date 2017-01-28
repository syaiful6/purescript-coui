module Coui.Action.CoT
  ( CoT(..)
  , runCoT
  , lowerCoT
  , lowerCoT_
  , liftCoT
  , liftCoT_
  , liftCoTM
  , liftCoTM_
  , pairCoTM
  , pairCoTM_
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Env.Class (class ComonadAsk, class ComonadEnv, ask, local)
import Control.Comonad.Store.Class (class ComonadStore, peek, pos)
import Control.Comonad.Traced.Class (class ComonadTraced, track)
import Control.Extend (class Extend, (=>>))
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Tuple (Tuple(..))


newtype CoT w m a = CoT (forall r. w (a -> m r) -> m r)

-- | unwrap
runCoT :: forall w m a r. CoT w m a -> w (a -> m r) -> m r
runCoT (CoT cot) = cot

-- | lowerCoT so it return underlying monad with value on CoT itself
lowerCoT :: forall w m a s. (Functor w, Monad m) => CoT w m s -> w a -> m s
lowerCoT m = runCoT m <<< (pure <$ _)

-- | lower CoT so it return underlying monad with value on the Comonad
lowerCoT_ :: forall w m a. (Functor w, Monad m) => CoT w m Unit -> w a -> m a
lowerCoT_ m = runCoT m <<< map (const <<< pure)

pairCoTM :: forall w m. (Functor w) => (forall a b c. (a -> b -> m c) -> w a -> CoT w m b -> m c)
pairCoTM f w cow = runCoT cow (map f w)

pairCoTM_ :: forall w m. (Functor w) => (forall a b c. (a -> b -> m c) -> CoT w m a -> w b -> m c)
pairCoTM_ f ga fb = pairCoTM (flip f) fb ga

liftCoT :: forall w m s. Comonad w => (forall a. w a -> s) -> CoT w m s
liftCoT f = CoT (extract <*> f)

-- | lift a function that operate on the underlying comonad
liftCoT_ :: forall w m. (forall a. w a -> a) -> CoT w m Unit
liftCoT_ f = CoT (_ `f` unit)

liftCoTM :: forall w m s. (Comonad w, Monad m) => (forall a. w a -> m s) -> CoT w m s
liftCoTM f = CoT (\wa -> extract wa =<< f wa)

liftCoTM_ :: forall w m. (Comonad w, Monad m) => (forall a. w a -> m a) -> CoT w m Unit
liftCoTM_ f = CoT ((_ $ unit) <=< f)

instance functorCoT :: Functor w => Functor (CoT w m) where
  map f (CoT cot) = CoT \w -> cot (map (_ <<< f) w)

instance applyCoT :: Extend w => Apply (CoT w m) where
  apply (CoT f) (CoT a) = CoT \w -> f (w =>> \wf g -> a (map (_ <<< g) wf))

instance applicativeCoT :: Comonad w => Applicative (CoT w m) where
  pure a = CoT \w -> extract w a

instance bindCoT :: Extend w => Bind (CoT w m) where
  bind (CoT k) f = CoT \w -> k (w =>> \wa a -> runCoT (f a) wa)

instance monadCoT :: Comonad w => Monad (CoT w m)

instance monadAskCoT :: ComonadAsk e w => MonadAsk e (CoT w m) where
  ask = liftCoT (ask :: forall a. w a -> e)

instance monadReaderCoT :: ComonadEnv e w => MonadReader e (CoT w m) where
  local f (CoT x) = CoT (x <<< local f)

instance monadStateCoT :: ComonadStore s w => MonadState s (CoT w m) where
  state f = do
    s <- liftCoT pos
    case f s of
      Tuple a s1 -> CoT \w -> peek s1 w a

instance monadTellCoT :: ComonadTraced t w => MonadTell t (CoT w m) where
  tell t = CoT \w -> track t w unit

instance monadTransCoT :: Comonad w => MonadTrans (CoT w) where
  lift m = CoT (extract <<< map (m >>= _))

instance monadEffCoT :: (Comonad w, MonadEff eff m) => MonadEff eff (CoT w m) where
  liftEff = lift <<< liftEff
