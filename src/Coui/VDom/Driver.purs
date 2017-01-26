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
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Util (refEq)

import Unsafe.Coerce (unsafeCoerce)

import Coui.Aff.Driver (CouiIO)
import Coui.Aff.Driver as AD
import Coui.Aff.Effects (CoreEffects)
import Coui.Component (Component)
import Coui.HTML.Core (HTML(..), Prop, Thunk(..))
import Coui.Query.InputF (InputF)

type VHTML i eff = V.VDom (Array (Prop (InputF Unit i))) (Exists (Thunk i))

newtype RenderState i eff =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (Eff (CoreEffects eff)) (VHTML i) DOM.Node
    }

mkSpec
  :: forall i eff
   . (InputF Unit i -> Eff (CoreEffects eff) Unit)
  -> DOM.Document
  -> V.VDomSpec
      (CoreEffects eff)
      (Array (VP.Prop (InputF Unit i)))
      (Exists (Thunk i))
mkSpec handler document =
  V.VDomSpec { buildWidget, buildAttributes, document }

  where
    buildAttributes
      :: DOM.Element
      -> V.VDomMachine (CoreEffects eff) (Array (VP.Prop (InputF Unit i))) Unit
    buildAttributes = VP.buildProp handler

    buildWidget
      :: V.VDomSpec (CoreEffects eff) (Array (VP.Prop (InputF Unit i))) (Exists (Thunk i))
      -> V.VDomMachine (CoreEffects eff) (Exists (Thunk i)) DOM.Node
    buildWidget spec = render
      where
      render = runExists \(Thunk a render) -> do
        V.Step node m h <- V.buildVDom spec (render a)
        pure (V.Step node (Fn.runFn4 patch (unsafeCoerce a) node m h) h)

      patch = Fn.mkFn4 \a node step halt -> runExists \(Thunk b render) ->
        if Fn.runFn2 refEq a b
          then pure (V.Step node (Fn.runFn4 patch a node step halt) halt)
          else do
            V.Step node' m h <- step (render b)
            pure (V.Step node (Fn.runFn4 patch (unsafeCoerce b) node' m h) h)

runUI
  :: forall w f i o eff. Comonad w
  => Component HTML w f i o (Aff (CoreEffects eff))
  -> i
  -> DOM.HTMLElement
  -> Aff (CoreEffects eff) (CouiIO i o (Aff (CoreEffects eff)))
runUI component i element = do
  document <- liftEff $ DOM.htmlDocumentToDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component

renderSpec
  :: forall eff
   . DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec HTML RenderState eff
renderSpec document container handler (HTML vdom) = case _ of
  Nothing -> do
    let spec = mkSpec handler document
    machine <- V.buildVDom spec vdom
    let node = V.extract machine
    DOM.appendChild node (DOM.htmlElementToNode container)
    pure $ RenderState { machine, node }
  Just (RenderState { machine, node }) -> do
    machine' <- V.step machine vdom
    let newNode = V.extract machine'
    when (not (Fn.runFn2 refEq node newNode)) (substInParent node newNode)
    pure $ RenderState { machine: machine', node: newNode }

substInParent :: forall eff. DOM.Node -> DOM.Node -> Eff (dom :: DOM | eff) Unit
substInParent oldNode newNode = do
  npn <- DOM.parentNode oldNode
  traverse_ (\pn -> DOM.replaceChild newNode oldNode pn) (toMaybe npn)
