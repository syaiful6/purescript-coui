module Coui.Component
  ( Component(..)
  , Component'
  , Render
  , Handler
  , component
  , ixcomponent
  , foreach
  , overV
  , action
  , both
  , render
  , hoist
  , match
  , withState
  , module Exports
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)

import Data.Either (Either(..), either)
import Data.Lens (class Wander, Prism', Indexed(..), Lens', lens, traversed)
import Data.Lens.Indexed (positions)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (class Profunctor, rmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..), snd)

import Coui.Internal.Complet (Complet(..), Action, complet, matchAction, defaultRender, hoistM, ignore,
  update)
import Coui.Internal.Complet (Complet(..), Action, complet, perform,
  update, effects, withEffects, defaultAction, defaultRender, ignore, terminate) as Exports

-- | Our component just like Star profunctor.
newtype Component m h f a b = Component (a -> Complet m h f b)

derive instance newtypeComponent :: Newtype (Component m h f a b) _

instance profunctorComponent :: Profunctor (Component m h f) where
  dimap f g (Component fk) = Component (f >>> fk >>> map g)

instance strongComponent :: Strong (Component m h f) where
  first  (Component f) = Component \(Tuple s x) -> map (_ `Tuple` x) (f s)
  second (Component f) = Component \(Tuple x s) -> map (Tuple x) (f s)

instance choiceComponent :: Plus m => Choice (Component m h f) where
  left  (Component f) = Component $ either (map Left <<< f) (pure <<< Right)
  right (Component f) = Component $ either (pure <<< Left) (map Right <<< f)

instance wanderComponent :: Plus m => Wander (Component m h f) where
  wander t (Component k) = Component (t k)

instance semigroupComponent :: Alt m => Semigroup (Component m h f a a) where
  append (Component f) (Component g) = Component \s -> f s <|> g s

instance monoidComponent :: Plus m => Monoid (Component m h f a a) where
  mempty = Component \_ -> empty

type Component' m h f s = Component m h f s s

type Render h f s = s -> Array (h f)

type Handler m f s = s -> Action m f s

toComplet :: forall m h f s t. Component m h f s t -> s -> Complet m h f t
toComplet (Component k) a = k a

component :: forall m h f s. Render h f s -> Handler m f s -> Component' m h f s
component vi act = Component \s -> complet (vi s) (act s)

-- | Create static view, if the view can raise events then it should be handled
-- | by other component, to handle it append the component using semigroup instance
-- | of Component.
render :: forall m h f s. Plus m => Render h f s -> Component' m h f s
render v = Component \s -> complet (v s) (const ignore)

overV :: forall m h v f s. (Array (h f) -> Array (v f)) -> Component' m h f s -> Component' m v f s
overV f = Component <<< map (\(Complet (Tuple vi act)) -> complet (f vi) act) <<< unwrap

both
  :: forall m n h v f g s
   . (Array (h f) -> Array (v g))
  -> (Action m f s -> Action n g s)
  -> Component' m h f s
  -> Component' n v g s
both f g = Component <<< map (\(Complet (Tuple vi act)) -> complet (f vi) (g act)) <<< unwrap

action :: forall m h f s. Handler m f s -> Component' m h f s
action f = Component \s -> complet defaultRender (f s)

hoist :: forall m n h f s. (m ~> n) -> Component' m h f s -> Component' n h f s
hoist nat = Component <<< map (hoistM nat) <<< unwrap

withState :: forall m h f s. (s -> Component' m h f s) -> Component' m h f s
withState = Component <<< (unwrap =<< _)

match :: forall m h f g s. (Functor h, Plus m) => Prism' g f -> Component' m h f s -> Component' m h g s
match prism = Component <<< map (matchAction prism) <<< unwrap

ixcomponent
  :: forall i m h f a. Plus m
  => (i -> Component' m h f a) -> Indexed (Component m h f) i a a
ixcomponent u = Indexed $ rmap snd (Component \(Tuple i a) -> map (Tuple i) $ unwrap (u i) a)

foreach
  :: forall m h f t a. (Plus m, Functor h, Traversable t)
  => (Int -> Component' m h f a) -> Component' m h (Tuple Int f) (t a)
foreach f = positions traversed $ ixcomponent item
  where
  item :: Int -> Component' m h (Tuple Int f) a
  item i = withState \s ->
    let
        itemRender :: Array (h f) -> Array (h (Tuple Int f))
        itemRender vi = map (map (Tuple i)) vi

        handleEv :: Action m f a -> Action m (Tuple Int f) a
        handleEv hd (Tuple i' a)
          | i' == i   = map (map (map (Tuple i))) $ hd a
          | otherwise = update s
    in both itemRender handleEv $ f i

_component :: forall m h f s t. Lens' (Component m h f s t) (s -> Complet m h f t)
_component = lens (\(Component a) -> a) (\(Component _) f -> Component f)

_handler :: forall m h f s. Lens' (Complet m h f s) (Action m f s)
_handler =
  lens (\(Complet (Tuple _ act)) -> act) (\(Complet (Tuple vi _)) f -> Complet (Tuple vi f))
