{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module MCP.Highlevel where

import qualified Data.Map.Strict as M
import           MCP.Derive

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

$(return [])

getPrompts' :: [PromptDefinition]
getPrompts' = $(derivePrompts ''PromptInstance)

getPrompts'' :: [PromptDefinition]
getPrompts'' = $(derivePromptsWithDocs ''PromptInstance 'promptDoc)

$(deriveInvokePrompt ''PromptInstance)

