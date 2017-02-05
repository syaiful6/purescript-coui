module Component.TaskList where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Either (either, Either(..))
import Data.Foldable (fold)
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp)
import Data.Int (fromString)
import Data.List (List, snoc, length, filter, deleteAt)
import Data.Lens (Prism', Lens', lens, prism, over)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(Tuple), uncurry)

import DOM.Event.Types (KeyboardEvent, keyboardEventToEvent)
import DOM.Event.Event as EE

import Unsafe.Coerce (unsafeCoerce)

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
    TaskAction i t -> Right $ Tuple i t
    _ -> Left ta

_task :: Lens' TaskList (List Task)
_task = lens _.tasks (_ { tasks = _ })

taskList :: forall eff. Co.Component' eff Co.HTML TaskListAction TaskList
taskList = container $ fold
  [ headers
  , table $ Co.withState \st ->
      Co.focus _task _TaskAction $
        Co.foreach \_ -> applyFilter st.filter task
  , footer
  , listActions
  ]

container :: forall eff f s. Co.Component' eff Co.HTML f s -> Co.Component' eff Co.HTML f s
container = over Co._render \rd s ->
  [ HH.div [ HP.class_ $ HH.className "container" ] (rd s) ]

headers :: forall eff. Co.Component' eff Co.HTML TaskListAction TaskList
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

    action :: Co.Action' eff TaskListAction TaskList
    action _ (SetFilter fil) = void $ Co.modifyState (_ { filter = fil })
    action _ _ = pure unit

applyFilter :: forall eff f. Filter -> Co.Component' eff Co.HTML f Task -> Co.Component' eff Co.HTML f Task
applyFilter filter = over Co._render \render s ->
  if matches filter s
    then render s
    else []
  where
  matches All       _ = true
  matches Completed t = t.completed
  matches Active    t = not t.completed

handleKeyPress :: KeyboardEvent -> Maybe TaskListAction
handleKeyPress ev =
  case fromString (unsafeCoerce ev).keyCode of
    Nothing -> Nothing
    Just code
      | code == 13 ->
        either (const Nothing) (Just <<< NewTask)
          $ runExcept $ readProp "value" $ toForeign $ EE.currentTarget (keyboardEventToEvent ev)
      | code == 27 ->
          Just $ SetEditText ""
      | otherwise -> Nothing

table
  :: forall eff
   . Co.Component' eff Co.HTML TaskListAction TaskList
  -> Co.Component' eff Co.HTML TaskListAction TaskList
table = over Co._render \render s ->
  [ HH.table
      [ HP.class_ $ HH.className "table table-striped" ]
      [ HH.thead_
          [ HH.tr_
              [ HH.th [ HP.class_ $ HH.className "col-md-1" ] []
              , HH.th [ HP.class_ $ HH.className "col-md-10" ] [ HH.text "Description" ]
              , HH.th [ HP.class_ $ HH.className "col-md-1"  ] []
              ]
          ]
      , HH.tbody_ $
          [ HH.tr_
              [ HH.td_ []
              , HH.td_
                  [ HH.input
                      [ HP.inputType HP.InputText
                      , HP.class_ $ HH.className "form-control"
                      , HP.placeholder "Create a new task"
                      , HP.value s.editText
                      , HE.onKeyUp handleKeyPress
                      , HE.onValueChange (HE.input SetEditText)
                      ]
                  ]
              ]
          ] <> render s
      ]
  ]

footer :: forall eff. Co.Component' eff Co.HTML TaskListAction TaskList
footer = Co.component render Co.defaultAction
  where
    render :: Co.Render Co.HTML TaskListAction TaskList
    render s =
      let footerText = show completed <> "/" <> show total <> " tasks completed."
          completed  = length $ filter _.completed s.tasks
          total      = length s.tasks
      in [ HH.p_ [ HH.text footerText ] ]

listActions :: forall eff. Co.Component' eff Co.HTML TaskListAction TaskList
listActions = Co.component Co.defaultRender action
  where
    action :: Co.Action' eff TaskListAction TaskList
    action _ (TaskAction i RemoveTask) =
      void $ Co.modifyState \state -> state { tasks = fromMaybe state.tasks (deleteAt i state.tasks) }
    action _ (SetEditText s) =
      void $ Co.modifyState (_ { editText = s })
    action _ (NewTask s) =
      void $ Co.modifyState \st -> st { tasks = st.tasks `snoc` initialTask s, editText = "" }
    action _ _ = pure unit
