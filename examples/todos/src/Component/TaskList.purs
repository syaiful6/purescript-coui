module Component.TaskList where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Alternative (class Alternative, class Plus)

import Data.Array ((:), length, filter, deleteAt)
import Data.Either (either, Either(..))
import Data.Foldable (fold)
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp)
import Data.Lens (Prism', Lens', lens, prism)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), uncurry)

import DOM.Event.Event as EE
import DOM.Event.KeyboardEvent as EK
import DOM.Event.Types (KeyboardEvent, keyboardEventToEvent)

import Coui as Co
import Coui.HTML as HH
import Coui.HTML.Events as HE
import Coui.HTML.Properties as HP

import Component.Task (TaskAction(..), task)
import Model (Task, TaskList, Filter(..), showFilter, initialTask)


data TaskListAction
  = NewTask String
  | SetFilter Filter
  | SetEditText String
  | TaskAction Int TaskAction

_TaskAction :: Prism' TaskListAction (Tuple Int TaskAction)
_TaskAction = prism (uncurry TaskAction) \ta ->
  case ta of
    TaskAction i a -> Right (Tuple i a)
    _ -> Left ta

_tasks :: Lens' TaskList (Array Task)
_tasks = lens _.tasks (_ { tasks = _ })

taskList :: forall m. Alternative m => Co.Component' m Co.HTML TaskListAction TaskList
taskList = container $ fold
  [ headers
  , table $ Co.withState \st ->
      _tasks $ Co.match _TaskAction $
        Co.foreach \_ -> applyFilter st.filter task
  , footer
  , listActions
  ]

container :: forall m f s. Co.Component' m Co.HTML f s -> Co.Component' m Co.HTML f s
container = Co.overV \vi ->
  [ HH.div [ HP.class_ $ HH.className "container" ] vi ]

headers :: forall m. Alternative m => Co.Component' m Co.HTML TaskListAction TaskList
headers = Co.component render action
  where
  render :: Co.Render Co.HTML TaskListAction TaskList
  render s =
    [ HH.h1_ [ HH.text "Todo list" ]
    , HH.div [ HP.class_ $ HH.className "btn-group" ] (map filterHTML_ [ All, Active, Completed ])
    ]
    where
    filterHTML_ :: Filter -> Co.HTML TaskListAction
    filterHTML_ f =
      HH.button
        [ HP.class_ $ HH.className (if f == s.filter then "btn toolbar active" else "btn toolbar")
        , HE.onClick (HE.input_ (SetFilter f))
        ]
        [ HH.text (showFilter f) ]

  action :: Co.Handler m TaskListAction TaskList
  action s (SetFilter fil) = Co.update $ s { filter = fil }
  action _ _ = Co.ignore

applyFilter
  :: forall m f
   . Filter -> Co.Component' m Co.HTML f Task -> Co.Component' m Co.HTML f Task
applyFilter filter comp = Co.withState \s ->
  Co.overV (\vi -> if matches filter s then vi else []) comp
  where
  matches All       _ = true
  matches Completed t = t.completed
  matches Active    t = not t.completed

handleKeyPress :: String -> KeyboardEvent -> Maybe TaskListAction
handleKeyPress key ev
  | key == "Escape" = Just $ SetEditText ""
  | key == "Enter"  = either (const Nothing) (Just <<< NewTask) $ runExcept $ readProp "value" $
                        toForeign $ EE.currentTarget (keyboardEventToEvent ev)
  | otherwise       = Nothing

table
  :: forall m
   . Co.Component' m Co.HTML TaskListAction TaskList
  -> Co.Component' m Co.HTML TaskListAction TaskList
table ts = Co.withState \s -> flip Co.overV ts (render s)
  where
  render s vi =
    [ HH.table
        [ HP.class_ $ HH.className "table table-striped" ]
        [ HH.thunk thead unit
        , HH.tbody_ $
            [ HH.tr_
                [ HH.td_ []
                , HH.td_
                    [ HH.input
                        [ HP.type_ HP.InputText
                        , HP.class_ $ HH.className "form-control"
                        , HP.placeholder "Create a new task"
                        , HP.value s.editText
                        , HE.onKeyUp (\e -> handleKeyPress (EK.code e) e)
                        , HE.onValueChange (HE.input SetEditText)
                        ]
                    ]
                ]
            ] <> vi
        ]
    ]

  thead :: forall i. Unit -> Co.HTML i
  thead _ =
    HH.thead_
      [ HH.tr_
          [ HH.th [ HP.class_ $ HH.className "col-md-1" ] []
          , HH.th [ HP.class_ $ HH.className "col-md-10" ] [ HH.text "Description" ]
          , HH.th [ HP.class_ $ HH.className "col-md-1"  ] []
          ]
      ]

footer :: forall m. Plus m => Co.Component' m Co.HTML TaskListAction TaskList
footer = Co.render render
  where
  render :: Co.Render Co.HTML TaskListAction TaskList
  render s =
    let footerText = show completed <> "/" <> show total <> " tasks completed."
        completed  = length $ filter _.completed s.tasks
        total      = length s.tasks
    in [ HH.p_ [ HH.text footerText ]]

listActions :: forall m. Alternative m => Co.Component' m Co.HTML TaskListAction TaskList
listActions = Co.action action
  where
  action :: Co.Handler m TaskListAction TaskList
  action st (TaskAction i RemoveTask) =
    Co.update $ st { tasks = fromMaybe st.tasks (deleteAt i st.tasks) }
  action st (SetEditText tx) =
    Co.update $ st { editText = tx }
  action st (NewTask nt) =
    Co.update $ st { tasks = initialTask nt : st.tasks, editText = "" }
  action _ _ = Co.terminate
