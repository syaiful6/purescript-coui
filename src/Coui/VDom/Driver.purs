module Coui.VDom.Driver where

import Prelude

import Control.Comonad (class Comonad)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Exists (Exists, runExists)
import Data.Foldable (traverse_)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (HTMLElement, htmlElementToNode, htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild, removeChild, parentNode, replaceChild) as DOM
import DOM.Node.Types (Document, Element, Node) as DOM

import Halogen.VDom as V
import Halogen.VDom.Machine as V
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Util (refEq)

import Unsafe.Coerce (unsafeCoerce)

import Coui.Effects (CoreEffects)
import Coui.HTML.Core (HTML(..), Prop)
import Coui.Action.InputF (InputF)

type VHTML f = V.VDom (Array (Prop (InputF Unit f))) Void

newtype RenderState f eff =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (Eff (CoreEffects eff)) (VHTML f) DOM.Node
    }

mkSpec
  :: forall f eff
   . (InputF Unit f -> Eff (CoreEffects eff) Unit)
  -> DOM.Document
  -> V.VDomSpec
      (CoreEffects eff)
      (Array (VP.Prop (InputF Unit f)))
      Void
mkSpec handler document =
  V.VDomSpec { buildWidget: const (V.never), buildAttributes, document }

  where
    buildAttributes
      :: DOM.Element
      -> V.VDomMachine (CoreEffects eff) (Array (VP.Prop (InputF Unit f))) Unit
    buildAttributes = VP.buildProp handler

substInParent :: forall eff. DOM.Node -> DOM.Node -> Eff (dom :: DOM | eff) Unit
substInParent oldNode newNode = do
  npn <- DOM.parentNode oldNode
  traverse_ (\pn -> DOM.replaceChild newNode oldNode pn) (toMaybe npn)
