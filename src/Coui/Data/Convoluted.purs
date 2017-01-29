module Coui.Data.Convoluted where

import Prelude

import Control.Comonad (class Comonad, extract)

import Control.Comonad.Env (EnvT(..))
import Control.Comonad.Store (StoreT(..))
import Control.Comonad.Traced (TracedT(..))
import Data.Functor.Day (Day, day, runDay)
import Data.Identity (Identity(..))
import Data.Profunctor (dimap)
import Data.Tuple (Tuple(..))
import Coui.Action.CoT (CoT(..), runCoT)

type Hoist s t a b = a ~> b -> s ~> t

convolveLeft :: forall a b c. Hoist (Day a c) (Day b c) a b
convolveLeft nat = runDay \get fx gx -> day get (nat fx) gx

convolveRight :: forall a b c. Hoist (Day c a) (Day c b) a b
convolveRight nat = runDay \get fx gx -> day get fx (nat gx)

hoistOf :: forall s t a b. Hoist s t a b -> a ~> b -> s ~> t
hoistOf blur f s = blur f s

introCoDay1 :: forall w w' m a. (Functor w, Comonad w') => CoT w m a -> CoT (Day w w') m a
introCoDay1 a = CoT (runDay \f w w' -> runCoT a (map (_ `f` extract w') w))

introCoDay2 :: forall w w' m a. (Functor w, Comonad w') => CoT w m a -> CoT (Day w' w) m a
introCoDay2 a = CoT (runDay \f w' w -> runCoT a (map (f (extract w')) w))

stored :: forall w s t
        . Functor w
       => Hoist (StoreT s w) (StoreT t w) (StoreT s Identity) (StoreT t Identity)
stored pab = dimap tomorrow today (convolveLeft pab)
  where
  tomorrow :: forall a. StoreT s w a -> Day (StoreT s Identity) w a
  tomorrow (StoreT (Tuple w s)) = day (#) (StoreT (Tuple (Identity id) s)) w

  today :: forall a. Day (StoreT t Identity) w a -> StoreT t w a
  today = runDay \f (StoreT (Tuple (Identity g) s)) w -> StoreT (Tuple (map (flip (f <<< g)) w) s)

-- --
traced :: forall w s t
        . Functor w
       => Hoist (TracedT s w) (TracedT t w) (TracedT s Identity) (TracedT t Identity)
traced pab = dimap tomorrow today (convolveLeft pab)
  where
  tomorrow :: forall a. TracedT s w a -> Day (TracedT s Identity) w a
  tomorrow (TracedT w) = day (#) (TracedT (Identity id)) w

  today :: forall a. Day (TracedT t Identity) w a -> TracedT t w a
  today = runDay \f (TracedT (Identity g)) w -> TracedT (map (flip (f <<< g)) w)

envied :: forall w s t
        . Functor w
       => Hoist (EnvT s w) (EnvT t w) (EnvT s Identity) (EnvT t Identity)
envied pab = dimap tomorrow today (convolveLeft pab)
  where
  tomorrow :: forall a. EnvT s w a -> Day (EnvT s Identity) w a
  tomorrow (EnvT (Tuple e w)) = day (const id) (EnvT (Tuple e (Identity unit))) w

  today :: forall a. Day (EnvT t Identity) w a -> EnvT t w a
  today = runDay \f (EnvT (Tuple e (Identity a))) w -> EnvT (Tuple e (map (f a) w))
