module Main where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)

import Data.Either (either)
import Data.Maybe (Maybe(..))

import Network.HTTP.Affjax as AX

import Coui as Co
import Coui.Aff (CoreEffects, runCouiAff, awaitBody)
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP
import Coui.VDom.Driver (runUI)

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

type AppEffects eff = CoreEffects (ajax :: AX.AJAX | eff)

initialState :: State
initialState = { loading: false, username: "", result: Nothing }

data Action
  = SetUsername String
  | MakeRequest
  | ReceiveRequest String

ajaxComponent :: forall eff. Co.Component' (Aff (AppEffects eff)) Co.HTML Action State
ajaxComponent = Co.component render action
  where
    render :: Co.Render Co.HTML Action State
    render st =
      [ HH.div_ $
          [ HH.h1_ [ HH.text "Lookup GitHub user" ]
          , HH.label_
              [ HH.div_ [ HH.text "Enter username:" ]
              , HH.input
                  [ HP.value st.username
                  , HE.onValueInput (HE.input SetUsername)
                  ]
              ]
          , HH.button
              [ HP.disabled st.loading
              , HE.onClick (HE.input_ MakeRequest)
              ]
              [ HH.text "Fetch info" ]
          , HH.p_
              [ HH.text (if st.loading then "Working..." else "") ]
          , HH.div_
              case st.result of
                Nothing -> []
                Just res ->
                  [ HH.h2_
                      [ HH.text "Response:" ]
                  , HH.pre_
                      [ HH.code_ [ HH.text res ] ]
                  ]
          ]
      ]

    action :: Co.Handler (Aff (AppEffects eff)) Action State
    action s (SetUsername username) =
      Co.update $ s { username = username, result = Nothing :: Maybe String }
    action s MakeRequest = Co.withEffect (s { loading = true }) do
      req <- attempt $ AX.get ("https://api.github.com/users/" <> s.username)
      pure $ either (const Nothing) (Just <<< ReceiveRequest <<< _.response) req
    action s (ReceiveRequest res) =
      Co.update $ s { loading = false, result = Just res }

main :: forall eff. Eff (AppEffects eff) Unit
main = runCouiAff do
  body <- awaitBody
  runUI ajaxComponent initialState body
