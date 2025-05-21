module Examples.Salesforce.Handlers where

import           Data.List                     (sortBy)
import           Data.Maybe                    (fromMaybe, isJust)
import           Data.Ord                      (comparing)
import           Data.Time                     (UTCTime (..))
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
executeTool GetForecast = do
    opps <- getOpportunities
    -- convert to csv string and return
    let csvString =
            unlines
            $ map (\opp ->
                oppName opp ++ ","
                ++ (show $ oppCloseDate opp) ++ ","
                ++ oppStageName opp ++ ","
                ++ "ZAR " ++ (show $ fromMaybe 0 $ oppAmount opp)
            )
            $ filter (\opp ->
                (isJust $ oppAmount opp)
                && (oppStageName opp /= "Closed Won")
                && (oppStageName opp /= "Closed Lost")
                && (oppCloseDate opp < (UTCTime (fromGregorian 2025 9 1) 0))
            )
            $ sortBy (comparing oppCloseDate)
            opps
    pure csvString


