{-# LANGUAGE OverloadedStrings #-}

module Examples.Ruddr.Handlers where

import qualified Data.Text                as T
import           Examples.Ruddr.API       (getProjectsCsvWithSearch,
                                           newRuddrClient, runRuddr)
import qualified Examples.Ruddr.API       as API
import           Examples.Ruddr.Interface
import           MCP.Derive
import           MCP.Types                (PromptDefinition, ToolDefinition)

$(deriveInvokePrompt ''Prompt)
$(deriveInvokeTool ''Tool)

prompts :: [PromptDefinition]
prompts = $(derivePrompts ''Prompt 'promptDoc)

tools :: [ToolDefinition]
tools = $(deriveTools ''Tool 'toolDoc)

executePrompt :: Prompt -> IO String
executePrompt NoPrompt = pure "No prompt provided"

executeTool :: Tool -> IO String
executeTool (GetProjects search) = do
    token <- API.readCredentials
    let client = newRuddrClient token
    result <- runRuddr client $ getProjectsCsvWithSearch (T.pack <$> search)
    pure $ either id unlines result
