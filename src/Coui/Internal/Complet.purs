module Coui.Internal.Complet
  ( Complet(..)
  , Action
  , complet
  , hoistM
  , ignore
  , perform
  , update
  , onlyEffect
  , withEffect
  , defaultAction
  , defaultRender
  , matchAction
  , terminate
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)

import Data.Bifunctor as BF
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Lens (Prism', review, matching)
import Data.Maybe (Maybe(..))
import Data.Maybe.Last (Last(..))
import Data.Newtype (class Newtype, ala)
import Data.Tuple (Tuple(..))

-- | Synonym for an action function that optionally returns new state and asyncronous operation
-- | that optionally return an action, this action will feed back to your application.
type Action m f s = f -> Tuple (Maybe s) (m (Maybe f))

-- | an applicative that build component, the first item on tuple is the renderable
-- | and the last one handle the event raised by the renderable/external sources, it
-- | return optionally new state for the application and asyncronous action.
newtype Complet m h f a = Complet (Tuple (Array (h f)) (Action m f a))

derive instance newtypeComplet :: Newtype (Complet m h f a) _

instance functorComplet :: Functor (Complet m h f) where
  map f (Complet rd) = Complet $ map (map (BF.lmap (map f))) rd

instance applyComplet :: Alt m => Apply (Complet m h f) where
  apply (Complet (Tuple v1 fk)) (Complet (Tuple v2 fa)) = Complet $ Tuple (v1 <> v2) go
    where
    go f =
      case fk f, fa f of
        Tuple s1 act1, Tuple s2 act2 ->
          Tuple (s1 <*> s2) (act1 <|> act2)

instance applicativeComplet :: Plus m => Applicative (Complet m h f) where
  pure a = Complet $ Tuple [] \_ -> Tuple (pure a) empty

instance altComplet :: Alt m => Alt (Complet m h f) where
  alt (Complet (Tuple v1 fk)) (Complet (Tuple v2 fa)) = Complet $ Tuple (v1 <> v2) go
    where
    -- pick the last one
    go f =
      case fk f, fa f of
        Tuple s1 act1, Tuple s2 act2 ->
          Tuple (ala Last foldMap [s1, s2]) (act1 <|> act2)

instance plusComponent :: Plus m => Plus (Complet m h f) where
  empty = Complet $ Tuple [] \_ -> Tuple Nothing empty

complet :: forall m h f a. Array (h f) -> Action m f a -> Complet m h f a
complet vi hd = Complet $ Tuple vi hd

defaultAction :: forall m f s. Plus m => Action m f s
defaultAction _ = Tuple Nothing empty

defaultRender :: forall h f. Array (h f)
defaultRender = []

perform :: forall m f s. Maybe s -> m (Maybe f) -> Tuple (Maybe s) (m (Maybe f))
perform s act = Tuple s act

update :: forall m f s. Plus m => s -> Tuple (Maybe s) (m (Maybe f))
update s = Tuple (pure s) empty

onlyEffect :: forall m f s. m (Maybe f) -> Tuple (Maybe s) (m (Maybe f))
onlyEffect eff = Tuple Nothing eff

withEffect :: forall m f s. s -> m (Maybe f) -> Tuple (Maybe s) (m (Maybe f))
withEffect s eff = Tuple (pure s) eff

-- | Ignore the event, dont update the state and empty async action.
ignore :: forall m f s. Plus m => Tuple (Maybe s) (m (Maybe f))
ignore = Tuple Nothing empty

-- | terminate the asyncronous action. So it will not fail. Use ignore if you want
-- | the parent component handle the event raised by child.
terminate :: forall m f s. Applicative m => Tuple (Maybe s) (m (Maybe f))
terminate = Tuple Nothing (pure Nothing)

matchAction :: forall m h f g a. (Functor h, Plus m) => Prism' g f -> Complet m h f a -> Complet m h g a
matchAction prism (Complet (Tuple render action)) = Complet $ Tuple (map (map (review prism)) render) go
  where
  go f =
    case matching prism f of
      Left _ -> ignore
      Right f' -> map (map (map (review prism))) $ action f'

hoistM :: forall m n h f. (m ~> n) -> Complet m h f ~> Complet n h f
hoistM nat (Complet (Tuple vi act)) = complet vi $ map (map nat) act
