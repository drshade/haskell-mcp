module Examples.Salesforce.Handlers where

import           Data.List                     (sortBy)
import           Data.Maybe                    (fromMaybe, isJust)
import           Data.Ord                      (comparing)
import           Data.Time                     (UTCTime (..), defaultTimeLocale,
                                                parseTimeM)
import           Data.Time.Calendar            (fromGregorian)
import           Examples.Salesforce.Demo      (Opportunity (..),
                                                getOpportunities)
import           Examples.Salesforce.Interface
import           MCP.Derive
import           MCP.Types                     (PromptDefinition,
                                                ToolDefinition)

$(deriveInvokePrompt ''Prompt)
$(deriveInvokeTool ''Tool)

prompts :: [PromptDefinition]
prompts = $(derivePrompts ''Prompt 'promptDoc)

tools :: [ToolDefinition]
tools = $(deriveTools ''Tool 'toolDoc)

executePrompt :: Prompt -> IO String
executePrompt NoPrompt = pure "No prompt provided"

executeTool :: Tool -> IO String
executeTool (GetForecast start end) = do
    opps <- getOpportunities
    startdate :: UTCTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d" start
    enddate :: UTCTime <- parseTimeM True defaultTimeLocale "%Y-%m-%d" end
    -- convert to csv string and return
    let csv =
            unlines
            $ map (\opp ->
                oppName opp ++ ","
                ++ oppOwnerName opp ++ ","
                ++ (show $ oppCloseDate opp) ++ ","
                ++ oppStageName opp ++ ","
                ++ "ZAR " ++ (show $ fromMaybe 0 $ oppAmount opp)
            )
            $ filter (\opp ->
                (isJust $ oppAmount opp)
                && (oppStageName opp /= "Closed Won")
                && (oppStageName opp /= "Closed Lost")
                && (oppCloseDate opp >= startdate)
                && (oppCloseDate opp <= enddate)
            )
            $ sortBy (comparing oppCloseDate)
            opps
    pure csv
executeTool (RunSOQL query) = pure "TBD: RunSOQL not implemented yet"
executeTool Approve = pure "Approved"
executeTool Reject = pure "Rejected"

