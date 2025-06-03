module Examples.Salesforce.Handlers where

import           Control.Monad.Except          (ExceptT (..), runExceptT)
import           Control.Monad.Trans           (lift)
import           Data.Time                     (UTCTime (..), defaultTimeLocale,
                                                formatTime, parseTimeM)
import qualified Examples.Salesforce.API       as API
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
    token   <- API.getToken

    startdate <- parseTimeM True defaultTimeLocale "%Y-%m-%d" start
    enddate <- parseTimeM True defaultTimeLocale "%Y-%m-%d" end

    results <- API.query token $ "SELECT Id, Name, Owner.Name, CloseDate, StageName, CurrencyIsoCode, Amount \
                                 \FROM Opportunity \
                                 \WHERE CloseDate >= " <> formatDate startdate <>
                                 " AND CloseDate <= " <> formatDate enddate <>
                                 " AND StageName NOT IN ('Closed Won', 'Closed Lost')"

    pure $ either id unlines results
  where
    formatDate :: UTCTime -> String
    formatDate date = formatTime defaultTimeLocale "%Y-%m-%d" date

executeTool (RunSoqlQuery query) = do
    token   <- API.getToken
    results <- API.query token query
    pure $ either id unlines results

executeTool (GetRecords objectType mFilters mFields mLimit) = do
    result <- runExceptT $ do
      validObjType <- ExceptT $ pure $ API.validateObjectType objectType
      validFilters <- ExceptT $ pure $ API.validateFilters mFilters
      let soqlQuery = API.buildGetRecordsQuery validObjType validFilters mFields mLimit
      token <- lift API.getToken
      rows <- ExceptT $ API.query token soqlQuery
      let header = "Records from " ++ objectType ++ " (query: " ++ soqlQuery ++ ")"
          separator = replicate (length header) '-'
      pure $ unlines $ [header, separator] ++ rows

    case result of
      Left err     -> pure $ "Error: " ++ err
      Right output -> pure output
