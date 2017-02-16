module Component.Task where

import Prelude

import Control.Plus (class Plus)

import Coui as Co
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP

import Model (Task)

data TaskAction
  = ToggleCompleted Boolean
  | UpdateDescription String
  | RemoveTask

task :: forall m. Plus m => Co.Component' m Co.HTML TaskAction Task
task = Co.component (pure <<< HH.thunk render) action
  where
  render :: Task -> Co.HTML TaskAction
  render t =
    HH.tr_ <<< map (HH.td_ <<< pure) $
      [ HH.input
          [ HP.type_  HP.InputCheckbox
          , HP.title "Mark as completed"
          , HP.checked t.completed
          , HE.onChecked (HE.input ToggleCompleted)
          ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.placeholder "Task description"
          , HP.autofocus true
          , HP.value t.description
          , HE.onValueChange (HE.input UpdateDescription)
          ]
      , HH.button
          [ HP.title "Remove task"
          , HE.onClick (HE.input_ $ RemoveTask)
          ]
          [ HH.text "✖" ]
      ]

  action :: Co.Handler m TaskAction Task
  action s (UpdateDescription desc) = Co.update $ s { description = desc }
  action s (ToggleCompleted b) = Co.update $ s { completed = b }
  action _ _ = Co.ignore
