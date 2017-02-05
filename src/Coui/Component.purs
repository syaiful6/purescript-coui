module Coui.Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Free.Trans as FT
import Control.Coroutine (CoTransform(CoTransform), CoTransformer, fuseWith, cotransform,
  transform, transformCoTransformL, transformCoTransformR)
import Control.Monad.Rec.Class (forever)

import Data.Either (either, Either(..))
import Data.Foldable (for_)
import Data.List (List(..), (!!), modifyAt)
import Data.Lens (Prism', Lens', review, matching, lens, view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..), fst, snd)

import Coui.HTML.Core (HTML)

type Render h f s = s -> Array (h f)

type Action eff f s t = s -> f -> CoTransformer (Maybe s) (s -> t) (Aff eff) Unit
type Action' eff f s = Action eff f s s

type ComponentHTML = HTML Void

data Component eff h f s t = Component (Render h f s) (Action eff f s t)

instance profunctorComponent :: Profunctor (Component eff h f) where
  dimap f g (Component render action) = Component
    (render <<< f)
    (\s -> FT.interpret (perform f g) <<< action (f s))

instance strongComponent :: Strong (Component eff h f) where
  first (Component render action) = Component
    (\(Tuple a _) -> render a)
    (\(Tuple s c) f -> FT.interpret (perform fst (_ `Tuple` c)) $ action s f)
  second (Component render action) = Component
    (\(Tuple _ a) -> render a)
    (\(Tuple c s) f -> FT.interpret (perform snd (Tuple c)) $ action s f)

instance choiceComponent :: Choice (Component eff h f) where
  left (Component render action) = Component render' action'
    where
      render' = either render mempty
      action' s f = case s of
        Right _ -> pure unit
        Left s' -> FT.interpret transform' (action s' f)
      transform'
        :: forall s t c
         . CoTransform (Maybe s) (s -> t)
        ~> CoTransform (Maybe (Either s c)) (Either s c -> Either t c)
      transform' (CoTransform o k) =
        CoTransform (either (Left <<< o) Right) (k <<< join <<< map (either Just (const Nothing)))
  right (Component render action) = Component render' action'
    where
      render' = either mempty render
      action' s f = case s of
        Left _ -> pure unit
        Right s' -> FT.interpret transform' (action s' f)
      transform'
        :: forall s t c
         . CoTransform (Maybe s) (s -> t)
        ~> CoTransform (Maybe (Either c s)) (Either c s -> Either c t)
      transform' (CoTransform o k) =
        CoTransform (either Left (Right <<< o)) (k <<< join <<< map (either (const Nothing) Just))

instance functorComponent :: Functor (Component eff h f s) where
  map = dimap id

instance applyComponent :: Apply (Component eff h f s) where
  apply (Component rd1 act1) (Component rd2 act2) = Component
    (\s -> rd1 s <> rd2 s)
    (\s f ->
      fuseWith
        (\zap (CoTransform ff kk) (CoTransform fa ka) ->
          CoTransform (\s' -> ff s' (fa s')) (\i -> zap (kk i) (ka i)))
      (act1 s f)
      (act2 s f))

instance applicativeComponent :: Applicative (Component eff h f s) where
  pure a = Component (\_ -> []) (\_ _ -> cotransform (const a) $> unit)

instance semigroupComponent :: Semigroup (Component eff h f s t) where
  append (Component r1 a1) (Component r2 a2) =
    Component (\s -> r1 s <> r2 s) (\s f -> a1 s f *> a2 s f)

instance monoidComponent :: Monoid (Component eff h f s t) where
  mempty = Component (\_ -> []) (\_ _ -> pure unit)

perform
  :: forall a b c d
   . (a -> b)
  -> (c -> d)
  -> CoTransform (Maybe b) (b -> c) ~> CoTransform (Maybe a) (a -> d)
perform f g (CoTransform o k) = CoTransform (g <<< o <<< f) (k <<< map f)

type Component' eff h f s = Component eff h f s s

_render :: forall eff h f s t. Lens' (Component eff h f s t) (Render h f s)
_render = lens (\(Component r _) -> r) (\(Component _ act) r -> Component r act)

_action :: forall eff h f s t. Lens' (Component eff h f s t) (Action eff f s t)
_action = lens (\(Component _ act) -> act) (\(Component r _) act -> Component r act)

defaultRender :: forall h f s. Render h f s
defaultRender _ = []

component
  :: forall eff h f s
   . Render h f s
  -> Action eff f s s
  -> Component' eff h f s
component rd act = Component rd act

withState
  :: forall eff h f s
   . (s -> Component' eff h f s)
  -> Component' eff h f s
withState f = component render action
  where
    render :: Render h f s
    render s = view _render (f s) s

    action :: Action' eff f s
    action s g = view _action (f s) s g

focus
  :: forall eff h f g s t
   . Functor h
  => Lens' s t
  -> Prism' g f
  -> Component' eff h f t
  -> Component' eff h g s
focus ls prism (Component render action) = ls $ component render' action'
  where
    render' :: Render h g t
    render' = map (map $ review prism) <<< render

    action' :: Action eff g t t
    action' s f = either (\_ -> pure unit) (action s) $ matching prism f

focusState
  :: forall eff h f s t
   . Lens' s t
  -> Component' eff h f t
  -> Component' eff h f s
focusState ls = ls

match
  :: forall eff h f g s
   . Functor h
  => Prism' g f
  -> Component' eff h f s
  -> Component' eff h g s
match prism = focus id prism

split
  :: forall eff h f s t
   . Prism' s t
  -> Component' eff h f t
  -> Component' eff h f s
split p = p

foreach
  :: forall eff h f s
   . Functor h
  => (Int -> Component' eff h f s)
  -> Component' eff h (Tuple Int f) (List s)
foreach f = component render action
  where
    render :: Render h (Tuple Int f) (List s)
    render sts =
      foldWithIndex (\i st els ->
        case f i of Component rd _ -> els <> (map (map (Tuple i)) (rd st))) sts []

    action :: Action eff (Tuple Int f) (List s) (List s)
    action sts (Tuple i a) =
      for_ (sts !! i) \st ->
        case f i of
          Component _ act ->
            forever (transform (_ >>= (_ !! i)))
            `transformCoTransformL` act st a
            `transformCoTransformR` forever (transform (modifying i))
      where
        modifying :: Int -> (s -> s) -> List s -> List s
        modifying j g sts' = fromMaybe sts' (modifyAt j g sts')

    foldWithIndex :: forall a r. (Int -> a -> r -> r) -> List a -> r -> r
    foldWithIndex g = go 0
      where
      go _ Nil         r = r
      go i (Cons x xs) r = go (i + 1) xs (g i x r)
