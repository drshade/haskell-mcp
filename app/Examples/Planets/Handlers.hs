{-# LANGUAGE TemplateHaskell #-}

module Examples.Planets.Handlers where
import           Examples.Planets.Interface
import           MCP.Derive
import           MCP.Types                  (PromptDefinition, ToolDefinition)

$(deriveInvokePrompt ''Prompt)
$(deriveInvokeTool ''Tool)

prompts :: [PromptDefinition]
prompts = $(derivePrompts ''Prompt 'promptDoc)

tools :: [ToolDefinition]
tools = $(deriveTools ''Tool 'toolDoc)

executePrompt :: Prompt -> IO String
executePrompt (RobotStyle feel) = pure $ "Speak like a ROBOT with the following feel: " ++ feel

executeTool :: Tool -> IO String
executeTool GetAllGames                 = pure "List of all games: [Game1, Game2, Game3]"
executeTool (GetGameStatus gameid)      = pure $ "The status of game " ++ gameid ++ " is: RUNNING"
executeTool (MessagePlayers message)    = pure $ "Message sent to players: " ++ message

