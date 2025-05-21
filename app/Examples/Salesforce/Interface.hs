module Examples.Salesforce.Interface where

import           Data.Map  (fromList)
import           MCP.Types (DocTable)

data Prompt = NoPrompt

data Tool
  = GetForecast

promptDoc :: DocTable
promptDoc = fromList
  -- constructor docs
  [ ( "NoPrompt" , "No prompt" )

  -- field-level docs
  ]

toolDoc :: DocTable
toolDoc = fromList
  -- constructor docs
  [ ( "GetForecast",     "Get the opportunities in the next 12 months" )

  -- field-level docs
  ]

