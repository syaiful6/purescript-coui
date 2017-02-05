module Model where

import Prelude

import Data.List (List(Nil))

type Task =
  { description :: String
  , completed :: Boolean
  }

initialTask :: String -> Task
initialTask s =
  { description: s
  , completed: false
  }

type TaskList =
  { tasks :: List Task
  , filter :: Filter
  , editText :: String
  }

initialTaskListState :: TaskList
initialTaskListState =
  { tasks: Nil
  , editText: ""
  , filter: All
  }

data Filter = All | Active | Completed

derive instance eqFilter :: Eq Filter

showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"
