module Coui.HTML.Core
  ( HTML(..)
  , Thunk(..)
  , thunk
  , text
  , element
  , keyed
  , prop
  , attr
  , handler
  , ref
  , class IsProp
  , toPropValue
  , PropName(..)
  , AttrName(..)
  , ClassName(..)
  , module Exports
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Exists (Exists, mkExists, runExists)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

import DOM.Node.Types (Element)
import DOM.Event.Types (Event, EventType)

import Halogen.VDom as VDom
import Halogen.VDom.DOM.Prop (ElemRef(..), Prop(..), PropValue, propFromBoolean, propFromInt, propFromNumber, propFromString)

import Unsafe.Coerce (unsafeCoerce)

import Halogen.VDom (ElemName(..), Namespace(..)) as Exports
import Halogen.VDom.DOM.Prop (Prop(..), PropValue) as Exports

import Coui.Action.InputF (InputF)


newtype HTML i = HTML (VDom.VDom (Array (Prop (InputF Unit i))) (Exists (Thunk i)))

derive instance newtypeHTML :: Newtype (HTML i) _

instance functorHTML :: Functor HTML where
  map f (HTML vdom) =
    HTML (bimap (map (map (map f))) mapThunk' vdom)
    where
      mapThunk' = runExists (\e -> mkExists (mapThunk f e))

data Thunk i b = Thunk b (b -> HTML i)

mapThunk :: forall i i' b. (i -> i') -> Thunk i b -> Thunk i' b
mapThunk f (Thunk b th) = Thunk b (map (map f) th)

-- | A smart constructor for widget slots in the HTML.
thunk :: forall i a. (a -> HTML i) -> a -> HTML i
thunk render a = HTML $ VDom.Widget (mkExists (Thunk a render))

-- | Constructs a text node `HTML` value.
text :: forall i. String -> HTML i
text = HTML <<< VDom.Text

-- | A smart constructor for HTML elements.
element :: forall i. VDom.ElemName -> Array (Prop i) -> Array (HTML i) -> HTML i
element = coe (\name props children -> VDom.Elem (VDom.ElemSpec Nothing name props) children)
  where
  coe
    :: forall p. (VDom.ElemName -> Array (Prop i) -> Array (VDom.VDom (Array (Prop i)) p) -> VDom.VDom (Array (Prop i)) p)
    -> VDom.ElemName -> Array (Prop i) -> Array (HTML i) -> HTML i
  coe = unsafeCoerce

-- | A smart constructor for HTML elements with keyed children.
keyed :: forall i. VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML i)) -> HTML i
keyed = coe (\name props children -> VDom.Keyed (VDom.ElemSpec Nothing name props) children)
  where
  coe
    :: forall p. (VDom.ElemName -> Array (Prop i) -> Array (Tuple String (VDom.VDom (Array (Prop i)) p)) -> VDom.VDom (Array (Prop i)) p)
    -> VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML i)) -> HTML i
  coe = unsafeCoerce

-- | Create a HTML property.
prop :: forall value i. IsProp value => PropName value -> Maybe AttrName -> value -> Prop i
prop (PropName name) an v = Property name (toPropValue v)

-- | Create a HTML attribute.
attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute Nothing name

-- | Create an event handler.
handler :: forall i. EventType -> (Event -> Maybe i) -> Prop i
handler = Handler

ref :: forall i. (Maybe Element -> Maybe i) -> Prop i
ref f = Ref $ f <<< case _ of
  Created x -> Just x
  Removed _ -> Nothing

class IsProp a where
  toPropValue :: a -> PropValue

instance stringIsProp :: IsProp String where
  toPropValue = propFromString

instance intIsProp :: IsProp Int where
  toPropValue = propFromInt

instance numberIsProp :: IsProp Number where
  toPropValue = propFromNumber

instance booleanIsProp :: IsProp Boolean where
  toPropValue = propFromBoolean

-- | A type-safe wrapper for property names.
-- |
-- | The phantom type `value` describes the type of value which this property
-- | requires.
newtype PropName value = PropName String

derive instance newtypePropName :: Newtype (PropName value) _
derive newtype instance eqPropName :: Eq (PropName value)
derive newtype instance ordPropName :: Ord (PropName value)
derive instance genericPropName :: Generic (PropName value)

-- | A type-safe wrapper for attribute names.
newtype AttrName = AttrName String

derive instance newtypeAttrName :: Newtype AttrName _
derive newtype instance eqAttrName :: Eq AttrName
derive newtype instance ordAttrName :: Ord AttrName
derive instance genericAttrName :: Generic AttrName

-- | A wrapper for strings which are used as CSS classes.
newtype ClassName = ClassName String

derive instance newtypeClassName :: Newtype ClassName _
derive newtype instance eqClassName :: Eq ClassName
derive newtype instance ordClassName :: Ord ClassName
derive instance genericClassName :: Generic ClassName
