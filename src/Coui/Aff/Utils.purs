module Coui.Aff.Util
  ( awaitLoad
  , awaitBody
  , selectElement
  , runCouiAff
  ) where

import Prelude

import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Nullable (toMaybe)
import Data.Foreign (toForeign)

import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML.Event.EventTypes (load)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, windowToEventTarget, htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)

import Coui.Aff.Effects (CoreEffects)

-- | Waits for the document to load.
awaitLoad :: forall eff. Aff (dom :: DOM | eff) Unit
awaitLoad = makeAff \_ callback -> liftEff $
  window
    >>= windowToEventTarget
    >>> addEventListener load (eventListener (\_ -> callback unit)) false

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: forall eff. Aff (dom :: DOM | eff) HTMLElement
awaitBody = do
  awaitLoad
  maybe (throwError (error "Could not find body")) pure =<< selectElement "body"

-- | Tries to find an element in the document.
selectElement
  :: forall eff
   . String
  -> Aff (dom :: DOM | eff) (Maybe HTMLElement)
selectElement query = do
  mel <- liftEff $
    toMaybe <$>
      ((querySelector query <<< htmlDocumentToParentNode <=< document) =<< window)
  pure case mel of
    Nothing -> Nothing
    Just el -> either (const Nothing) Just $ runExcept $ readHTMLElement (toForeign el)

runCouiAff
  :: forall eff x
   . Aff (CoreEffects eff) x
  -> Eff (CoreEffects eff) Unit
runCouiAff = void <<< runAff throwException (const (pure unit))
