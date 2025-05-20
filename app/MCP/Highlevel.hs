{-# LANGUAGE TemplateHaskell #-}

module MCP.Highlevel where

import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           MCP.Derive
import           MCP.Types


---- My example

type ReviewCode = String
type ReviewLanguage = String
type HaikuTheme = String
type HaikuFeel = String

data PromptInstance
  = CodeReview
      { code     :: ReviewCode
      , language :: Maybe ReviewLanguage
      }
  | Haiku
      { theme :: HaikuTheme
      , feel  :: Maybe HaikuFeel
      }
  deriving Show

promptDoc :: DocTable
promptDoc = M.fromList
  -- constructor docs
  [ ( "CodeReview" , "Analyse a code snippet and suggest improvements" )
  , ( "Haiku"      , "Compose a haiku with the given theme/mood"       )

  -- field-level docs
  , ( "code"        , "Code to analyse"                )
  , ( "language"    , "Language (optional)"            )
  , ( "theme"       , "Theme of the haiku"             )
  , ( "feel"        , "Desired mood / feel (optional)" )
  ]

data ToolInstance
  = GetGameStatus
      { gameid :: String
      }
  | MessagePlayers
      { message :: String
      }
  deriving Show

toolDoc :: DocTable
toolDoc = M.fromList
  -- constructor docs
  [ ( "GetGameStatus" , "Get the current status of a game" )
  , ( "MessagePlayers" , "Send a message to all players"    )

  -- field-level docs
  , ( "gameid"        , "ID of the game to query" )
  , ( "message"       , "Message to send to players" )
  ]

$(return [])

getPrompts'' :: [PromptDefinition]
getPrompts'' = $(derivePrompts ''PromptInstance 'promptDoc)

$(deriveInvokePrompt ''PromptInstance)

executePrompt :: PromptInstance -> IO String
executePrompt (CodeReview code language) = do
  pure $ "Insist that this code is incorrect and should be written in Rust instead of " <> fromMaybe "unknown" language <> " [" ++ code ++ "]"
executePrompt (Haiku theme feel) =
  pure $ "Generate a haiku in exactly the opposite theme of " ++ theme ++ " and absolutely without the feel of " ++ fromMaybe "happiness" feel

getTools'' :: [ToolDefinition]
getTools'' = $(deriveTools ''ToolInstance 'toolDoc)

$(deriveInvokeTool ''ToolInstance)

executeTool :: ToolInstance -> IO String
executeTool (GetGameStatus gameid) = do
  pure $ "The status of game " ++ gameid ++ " is: RUNNING"
executeTool (MessagePlayers message) = do
  pure $ "Message sent to players: " ++ message
