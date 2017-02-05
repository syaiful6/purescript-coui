module Component.Task where

import Prelude

import Coui as Co
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP

import Model (Task)

data TaskAction
  = ToggleCompleted Boolean
  | UpdateDescription String
  | RemoveTask

task :: forall eff. Co.Component' eff Co.HTML TaskAction Task
task = Co.component render action
  where
  render :: Co.Render Co.HTML TaskAction Task
  render t =
    [ HH.tr_ <<< map (HH.td_ <<< pure) $
        [ HH.input
            [ HP.inputType HP.InputCheckbox
            , HP.title "Mark as completed"
            , HP.checked t.completed
            , HE.onChecked (HE.input ToggleCompleted)
            ]
        , HH.input
            [ HP.inputType HP.InputText
            , HP.placeholder "Task description"
            , HP.autofocus true
            , HP.value t.description
            , HE.onValueChange (HE.input UpdateDescription)
            ]
        , HH.button
            [ HP.title "Remove task"
            , HE.onClick (HE.input_ RemoveTask)
            ]
            [ HH.text "✖" ]
        ]
    ]

  action :: Co.Action' eff TaskAction Task
  action _ (UpdateDescription desc) = void $ do
    Co.modifyState (_ { description = desc })
  action _ (ToggleCompleted b) = void $ do
    Co.modifyState (_ { completed = b })
  action _ _ = pure unit
