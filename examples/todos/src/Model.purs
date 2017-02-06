module Model where

import Prelude

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
  { tasks :: Array Task
  , filter :: Filter
  , editText :: String
  }

initialTaskListState :: TaskList
initialTaskListState =
  { tasks: []
  , editText: ""
  , filter: All
  }

data Filter = All | Active | Completed

derive instance eqFilter :: Eq Filter

showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"
