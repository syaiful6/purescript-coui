module Counter.Counter where

import Prelude

import Coui as Co
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP

data Action = Increment | Decrement

type State = Int

counter :: forall m. Applicative m => Co.Component' m Co.HTML Action State
counter = Co.component render update
  where
  update :: Co.Handler m Action State
  update s Increment = Co.update (s + 1)
  update s Decrement = Co.update (s - 1)

  render :: Co.Render Co.HTML Action State
  render s =
    [ HH.p_ [ HH.text "The state is: ", HH.text (show s) ]
    , HH.div
        [ HP.class_ $ HH.className "btn-group" ]
        [ HH.button
            [ HP.class_ $ HH.className "btn btn-success"
            , HE.onClick (HE.input_ Increment)
            ]
            [ HH.text "+"]
        , HH.button
            [ HP.class_ $ HH.className "btn btn-danger"
            , HE.onClick (HE.input_ Decrement)
            ]
            [ HH.text "-"]
        ]
    ]
