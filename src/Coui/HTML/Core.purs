module Coui.HTML.Core
  ( HTML(..)
  , ThunkF(..)
  , Thunk(..)
  , Graft(..)
  , unGraft
  , text
  , thunk
  , element
  , keyed
  , prop
  , attr
  , handler
  , onCreated
  , onRemoved
  , class IsProp
  , toPropValue
  , PropName(..)
  , AttrName(..)
  , ClassName(..)
  , className
  , module Exports
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Exists (Exists, mkExists, runExists)
import Data.Coyoneda (Coyoneda, liftCoyoneda)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple)

import DOM.Node.Types (Element)
import DOM.Event.Types (Event, EventType)
import DOM.HTML.Indexed.ButtonType (ButtonType, renderButtonType)
import DOM.HTML.Indexed.CrossOriginValue (CrossOriginValue, renderCrossOriginValue)
import DOM.HTML.Indexed.DirValue (DirValue, renderDirValue)
import DOM.HTML.Indexed.FormMethod (FormMethod, renderFormMethod)
import DOM.HTML.Indexed.InputType (InputType, renderInputType)
import DOM.HTML.Indexed.KindValue (KindValue, renderKindValue)
import DOM.HTML.Indexed.MenuitemType (MenuitemType, renderMenuitemType)
import DOM.HTML.Indexed.MenuType (MenuType, renderMenuType)
import DOM.HTML.Indexed.OnOff (OnOff, renderOnOff)
import DOM.HTML.Indexed.OrderedListType (OrderedListType, renderOrderedListType)
import DOM.HTML.Indexed.PreloadValue (PreloadValue, renderPreloadValue)
import DOM.HTML.Indexed.ScopeValue (ScopeValue, renderScopeValue)
import DOM.HTML.Indexed.StepValue (StepValue, renderStepValue)
import DOM.HTML.Indexed.WrapValue (WrapValue, renderWrapValue)

import Unsafe.Coerce (unsafeCoerce)

import Halogen.VDom as VDom
import Halogen.VDom.DOM.Prop (ElemRef(..), Prop(..), PropValue, propFromBoolean,
  propFromInt, propFromNumber, propFromString)
import Halogen.VDom (ElemName(..), Namespace(..)) as Exports
import Halogen.VDom.DOM.Prop (Prop(..), PropValue) as Exports

newtype HTML i = HTML (VDom.VDom (Array (Prop i)) (Thunk i))

derive instance newtypeHTML :: Newtype (HTML i) _

instance functorHTML :: Functor HTML where
  map f (HTML vdom) = HTML (bimap (map (map f)) (map f) vdom)

newtype Thunk i = Thunk (Coyoneda Graft i)

derive newtype instance functorThunk :: Functor Thunk

data ThunkF i a = ThunkF a (a -> HTML i)

newtype Graft i = Graft (Exists (ThunkF i))

instance functorGraft :: Functor Graft where
  map f (Graft g) =
    g # runExists \(ThunkF a rd) ->
      Graft $ mkExists $ ThunkF a (map f <<< rd)

unGraft :: forall i. Graft i -> Exists (ThunkF i)
unGraft (Graft g) = g

thunk :: forall s a. (s -> HTML a) -> s -> HTML a
thunk render a = HTML $ VDom.Widget $ Thunk $ liftCoyoneda $ Graft $ mkExists (ThunkF a render)

-- | Constructs a text node `HTML` value.
text :: forall i. String -> HTML i
text = HTML <<< VDom.Text

-- | A smart constructor for HTML elements.
element :: forall i. VDom.ElemName -> Array (Prop i) -> Array (HTML i) -> HTML i
element = coe (\name props children -> VDom.Elem (VDom.ElemSpec Nothing name props) children)
  where
  coe
    :: (VDom.ElemName -> Array (Prop i) -> Array (VDom.VDom (Array (Prop i)) (Thunk i)) -> VDom.VDom (Array (Prop i)) (Thunk i))
    -> VDom.ElemName -> Array (Prop i) -> Array (HTML i) -> HTML i
  coe = unsafeCoerce

-- | A smart constructor for HTML elements with keyed children.
keyed :: forall i. VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML i)) -> HTML i
keyed = coe (\name props children -> VDom.Keyed (VDom.ElemSpec Nothing name props) children)
  where
  coe
    :: (VDom.ElemName -> Array (Prop i) -> Array (Tuple String (VDom.VDom (Array (Prop i)) (Thunk i))) -> VDom.VDom (Array (Prop i)) (Thunk i))
    -> VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML i)) -> HTML i
  coe = unsafeCoerce

-- | Create a HTML property.
prop :: forall value i. IsProp value => PropName value -> value -> Prop i
prop (PropName name) = Property name <<< toPropValue

-- | Create a HTML attribute.
attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute Nothing name

-- | Create an event handler.
handler :: forall i. EventType -> (Event -> Maybe i) -> Prop i
handler = Handler

onCreated :: forall i. (Element -> Maybe i) -> Prop i
onCreated f = Ref $ case _ of
  Created x -> f x
  Removed _ -> Nothing

onRemoved :: forall i. (Element -> Maybe i) -> Prop i
onRemoved f = Ref $ case _ of
  Created _ -> Nothing
  Removed x -> f x

className :: String -> ClassName
className = ClassName

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

instance mediaTypeIsProp :: IsProp MediaType where
  toPropValue = propFromString <<< unwrap

instance buttonTypeIsProp :: IsProp ButtonType where
  toPropValue = propFromString <<< renderButtonType

instance crossOriginValueIsProp :: IsProp CrossOriginValue where
  toPropValue = propFromString <<< renderCrossOriginValue

instance dirValueIsProp :: IsProp DirValue where
  toPropValue = propFromString <<< renderDirValue

instance formMethodIsProp :: IsProp FormMethod where
  toPropValue = propFromString <<< renderFormMethod

instance inputTypeIsProp :: IsProp InputType where
  toPropValue = propFromString <<< renderInputType

instance kindValueIsProp :: IsProp KindValue where
  toPropValue = propFromString <<< renderKindValue

instance menuitemTypeIsProp :: IsProp MenuitemType where
  toPropValue = propFromString <<< renderMenuitemType

instance menuTypeIsProp :: IsProp MenuType where
  toPropValue = propFromString <<< renderMenuType

instance onOffIsProp :: IsProp OnOff where
  toPropValue = propFromString <<< renderOnOff

instance orderedListTypeIsProp :: IsProp OrderedListType where
  toPropValue = propFromString <<< renderOrderedListType

instance preloadValueIsProp :: IsProp PreloadValue where
  toPropValue = propFromString <<< renderPreloadValue

instance scopeValueIsProp :: IsProp ScopeValue where
  toPropValue = propFromString <<< renderScopeValue

instance stepValueIsProp :: IsProp StepValue where
  toPropValue = propFromString <<< renderStepValue

instance wrapValueIsProp :: IsProp WrapValue where
  toPropValue = propFromString <<< renderWrapValue

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
