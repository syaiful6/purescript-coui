module Coui.VDom.Driver
  ( runUI
  , module Coui.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Foldable (traverse_)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (HTMLElement, htmlElementToNode, htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild, parentNode, replaceChild) as DOM
import DOM.Node.Types (Document, Element, Node) as DOM

import Halogen.VDom (VDom, VDomMachine, VDomSpec(VDomSpec), buildVDom) as V
import Halogen.VDom.Machine (Step, extract, never, step) as V
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Util (refEq)

import Coui.Aff.Driver (CoreEffects, Driver)
import Coui.Aff.Driver as AD
import Coui.Component (Component')
import Coui.HTML.Core (HTML(..))
import Coui.HTML as HH

type VHTML f = V.VDom (Array (VP.Prop f)) Void

newtype RenderState s f eff =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (Eff (CoreEffects eff)) (VHTML f) DOM.Node
    }

mkSpec
  :: forall f eff
   . (f -> Eff (CoreEffects eff) Unit)
  -> DOM.Document
  -> V.VDomSpec
      (CoreEffects eff)
      (Array (VP.Prop f))
      Void
mkSpec handler document =
  V.VDomSpec { buildWidget: const (V.never), buildAttributes, document }

  where
    buildAttributes
      :: DOM.Element
      -> V.VDomMachine (CoreEffects eff) (Array (VP.Prop f)) Unit
    buildAttributes = VP.buildProp handler

runUI
  :: forall f s eff
   . Component' (CoreEffects eff) (HTML Void) f s
  -> s
  -> DOM.HTMLElement
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI component s element = do
  document <- liftEff $ DOM.htmlDocumentToDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component s

renderSpec :: forall eff s. DOM.Document -> DOM.HTMLElement -> AD.RenderSpec (HTML Void) RenderState s eff
renderSpec document element = { render }
  where

  render
    :: forall f
     . (f -> Eff (CoreEffects eff) Unit)
    -> Array (HTML Void f)
    -> Maybe (RenderState s f eff)
    -> Eff (CoreEffects eff) (RenderState s f eff)
  render handler vdom = case _ of
    Nothing -> do
      let spec = mkSpec handler document
      machine <- V.buildVDom spec (wrap vdom)
      let node = V.extract machine
      DOM.appendChild node (DOM.htmlElementToNode element)
      pure $ RenderState { machine, node }
    Just (RenderState { machine, node }) -> do
      machine' <- V.step machine (wrap vdom)
      let newNode = V.extract machine'
      when (not (Fn.runFn2 refEq node newNode)) (substInParent node newNode)
      pure $ RenderState { machine: machine', node: newNode }

wrap :: forall f. Array (HTML Void f) -> VHTML f
wrap d = case HH.div_ d of HTML vdom -> vdom

substInParent :: forall eff. DOM.Node -> DOM.Node -> Eff (dom :: DOM | eff) Unit
substInParent oldNode newNode = do
  npn <- DOM.parentNode oldNode
  traverse_ (\pn -> DOM.replaceChild newNode oldNode pn) (toMaybe npn)
