module Coui.VDom.Driver
  ( runUI
  , module Coui.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Coyoneda (lowerCoyoneda)
import Data.Exists (runExists)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (HTMLElement, htmlElementToNode, htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild, parentNode, nextSibling, insertBefore) as DOM
import DOM.Node.Types (Document, Element, Node) as DOM

import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Util (refEq)

import Unsafe.Coerce (unsafeCoerce)

import Coui.Aff.Driver (CoreEffects, Driver)
import Coui.Aff.Driver as AD
import Coui.Component (Component')
import Coui.HTML.Core (HTML(..), ThunkF(..), Thunk(..), unGraft)
import Coui.HTML as HH

type VHTML f = V.VDom (Array (VP.Prop f)) (Thunk f)

newtype RenderState s f eff =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (Eff (CoreEffects eff)) (VHTML f) DOM.Node
    }

mkSpec
  :: forall f eff
   . (f -> Eff (CoreEffects eff) Unit)
  -> DOM.Document
  -> V.VDomSpec (CoreEffects eff) (Array (VP.Prop f)) (Thunk f)
mkSpec handler document = V.VDomSpec { buildWidget, buildAttributes, document }
  where
    buildAttributes
      :: DOM.Element
      -> V.VDomMachine (CoreEffects eff) (Array (VP.Prop f)) Unit
    buildAttributes = VP.buildProp handler

    buildWidget
      :: V.VDomSpec (CoreEffects eff) (Array (VP.Prop f)) (Thunk f)
      -> V.VDomMachine (CoreEffects eff) (Thunk f) DOM.Node
    buildWidget spec = render
      where
        render = case _ of
          Thunk co ->
            unGraft (lowerCoyoneda co) # runExists \(ThunkF a rder) -> do
              V.Step node m h <- V.buildVDom spec (unwrap (rder a))
              pure (V.Step node (Fn.runFn4 patch (unsafeCoerce a) node m h) h)

        patch = Fn.mkFn4 \a node step halt -> case _ of
          Thunk co ->
            unGraft (lowerCoyoneda co) # runExists \(ThunkF b rder) ->
              if Fn.runFn2 refEq a b
                then pure (V.Step node (Fn.runFn4 patch a node step halt) halt)
              else do
                V.Step node' m h <- step (unwrap (rder b))
                pure (V.Step node' (Fn.runFn4 patch (unsafeCoerce b) node' m h) h)

runUI
  :: forall f s eff
   . Component' (Aff (CoreEffects eff)) HTML f s
  -> s
  -> DOM.HTMLElement
  -> Aff (CoreEffects eff) (Driver f (Aff (CoreEffects eff)))
runUI component s element = do
  document <- liftEff $ DOM.htmlDocumentToDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component s

renderSpec :: forall eff s. DOM.Document -> DOM.HTMLElement -> AD.RenderSpec HTML RenderState s eff
renderSpec document element = { render }
  where
  render
    :: forall f
     . (f -> Eff (CoreEffects eff) Unit)
    -> Array (HTML f)
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
      when (not (Fn.runFn2 refEq node newNode)) do
        parent <- DOM.parentNode node
        nextSib <- DOM.nextSibling node
        substInParent newNode (toMaybe nextSib) (toMaybe parent)
      pure $ RenderState { machine: machine', node: newNode }

wrap :: forall f. Array (HTML f) -> VHTML f
wrap d = case HH.div_ d of HTML vdom -> vdom

substInParent
  :: forall eff
   . DOM.Node
  -> Maybe DOM.Node
  -> Maybe DOM.Node
  -> Eff (dom :: DOM | eff) Unit
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pure unit
