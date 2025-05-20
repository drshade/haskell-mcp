module MCP.Types where

import           Data.Aeson     (ToJSON (..), object, (.=))
import qualified Data.Aeson     as A
import           Data.Aeson.Key (fromString)
import qualified Data.Map       as M

type ArgumentName = String
type ArgumentDescription = String
type ArgumentRequired = Bool

data PromptArgumentDefinition = MkPromptArgumentDefinition ArgumentName ArgumentDescription ArgumentRequired
  deriving (Show)

data ToolArgumentDefinition = MkToolArgumentDefinition ArgumentName ArgumentDescription ArgumentRequired
  deriving (Show)

data PromptDefinition = MkPromptDefinition String String [PromptArgumentDefinition]
  deriving (Show)

data ToolDefinition = MkToolDefinition String String [ToolArgumentDefinition]
  deriving (Show)

data ArgumentInvocation = ArgumentInvocation String String
  deriving (Show)

data PromptInvocation = PromptInvocation String [ArgumentInvocation]
  deriving (Show)

data ToolInvocation = ToolInvocation String [ArgumentInvocation]
  deriving (Show)

type DocTable = M.Map String String

instance ToJSON PromptArgumentDefinition where
  toJSON (MkPromptArgumentDefinition name desc required) = object
    [ fromString "name" .= name
    , fromString "description" .= desc
    , fromString "required" .= required
    ]

instance ToJSON PromptDefinition where
  toJSON (MkPromptDefinition name desc args) = object
    [ fromString "name" .= name
    , fromString "description" .= desc
    , fromString "arguments" .= args
    ]

instance ToJSON ToolDefinition where
  toJSON (MkToolDefinition name desc args) = object
    [ fromString "name"        .= name
    , fromString "description" .= desc
    , fromString "inputSchema" .= toolSchema args
    ]
    where
      toolSchema :: [ToolArgumentDefinition] -> A.Value
      toolSchema as = object
        [ fromString "type"       .= ("object" :: String)
        , fromString "properties" .= object (map argProp as)
        , fromString "required"   .= [ n | MkToolArgumentDefinition n _ True <- as ]
        ]
      argProp (MkToolArgumentDefinition n d _) =
        (fromString n) .= object
          [ fromString "type"        .= ("string" :: String)
          , fromString "description" .= d
          ]
