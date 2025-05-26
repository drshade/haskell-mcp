module Examples.Salesforce.Interface where

import           Data.Map  (fromList)
import           MCP.Types (DocTable)

data Prompt = NoPrompt

data Tool
  = GetForecast { startdate :: String, enddate :: String }
  | RunSoqlQuery { query :: String }
  | Approve
  | Reject

promptDoc :: DocTable
promptDoc = fromList
  -- constructor docs
  [ ( "NoPrompt" , "No prompt" )

  -- field-level docs
  , ( "startdate", "The start date for records in the request, in the form '2025-03-01'" )
  , ( "enddate"  , "The end date for records in the request, in the form '2025-03-01'" )
  ]

toolDoc :: DocTable
toolDoc = fromList
  -- constructor docs
  [ ( "GetForecast",     "Get the opportunities in the next 12 months" )

  -- field-level docs
  ]

