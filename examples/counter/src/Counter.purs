module Counter.Counter where

import Prelude

import Coui as Co
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP

data Action = Increment | Decrement

type State = Int

counter :: forall eff. Co.Component' eff Co.HTML Action State
counter = Co.component render action

render :: Co.Render Co.HTML Action State
render s =
  [ HH.p_ [ HH.text "The state is: ", HH.text (show s) ]
  , HH.div
      [ HP.class_ $ HH.ClassName "btn-group" ]
      [ HH.button
          [ HP.class_ $ HH.ClassName "btn btn-success"
          , HE.onClick (HE.input_ Increment)
          ]
          [ HH.text "+"]
      , HH.button
          [ HP.class_ $ HH.ClassName "btn btn-danger"
          , HE.onClick (HE.input_ Decrement)
          ]
          [ HH.text "-"]
      ]
  ]

action :: forall eff. Co.Action' eff Action State
action s Increment =
  Co.modifyState (add 1) $> unit
action s Decrement =
  Co.modifyState (add (-1)) $> unit
