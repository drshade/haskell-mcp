module Examples.Ruddr.Interface where

import           Data.Map  (fromList)
import           MCP.Types (DocTable)

data Prompt = NoPrompt

data Tool
    = GetProjects { search :: Maybe String }

promptDoc :: DocTable
promptDoc = fromList
  -- constructor docs
  [ ( "NoPrompt" , "No prompt" )

  -- field-level docs
  ]

toolDoc :: DocTable
toolDoc = fromList
  -- constructor docs
  [ ( "GetProjects",    "Get a list of projects in the Project Operations platform Ruddr currently in delivery" )

  -- field-level docs
  , ( "search",         "Matching this search criteria (optional, leave out for all)" )
  ]

