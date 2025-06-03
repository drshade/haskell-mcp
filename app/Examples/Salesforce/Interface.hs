module Examples.Salesforce.Interface where

import           Data.Map  (fromList)
import           MCP.Types (DocTable)

data Prompt = NoPrompt

data Tool
  = GetForecast { startdate :: String, enddate :: String }
  | GetRecords { objectType :: String, filters :: Maybe String, fields :: Maybe String, limit :: Maybe String }
  | RunSoqlQuery { query :: String }
  | TestTool { testField :: String }

promptDoc :: DocTable
promptDoc = fromList
  -- constructor docs
  [ ( "NoPrompt" , "No prompt" )

  -- field-level docs
  ]

toolDoc :: DocTable
toolDoc = fromList
  -- constructor docs
  [ ( "GetForecast",     "Get the opportunities in Salesforce between two dates (these are only OPEN deals - ie not won or lost)" )
  , ( "GetRecords",       "Retrieve records from any Salesforce object with optional filtering" )
  , ( "RunSoqlQuery",    "Run a SOQL query against the Salesforce database" )

  -- field-level docs
  , ( "startdate",    "The start date for records in the request, in the form '2025-03-01'" )
  , ( "enddate"  ,    "The end date for records in the request, in the form '2025-03-01'" )
  , ( "objectType",      "Salesforce object name (e.g., 'Account', 'Contact', 'Opportunity', 'Lead', 'Case', 'Task' or 'Event')" )
  , ( "filters",         "Optional WHERE clause conditions (e.g., 'Name LIKE \\'%Acme%\\' AND Industry = \\'Technology\\'')" )
  , ( "fields",          "Comma-separated list of fields to retrieve (defaults to common fields if not specified)" )
  , ( "limit",           "Maximum number of records to return (defaults to 100)" )
  , ( "query",        "The SOQL query to run" )
  ]

