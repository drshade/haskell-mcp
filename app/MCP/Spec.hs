module MCP.Spec where

import           Data.Aeson    (Value, defaultOptions, fieldLabelModifier)
import           Data.Aeson.TH (deriveJSON)
import           Data.Char     (toLower)
import           Optics        (makeLenses)

data RpcRequest = RpcRequest
  { _rpcRequestJsonRpc :: String
  , _rpcRequestId      :: Value
  , _rpcRequestMethod  :: String
  , _rpcRequestParams  :: Maybe Value
  } deriving (Show)

makeLenses ''RpcRequest
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length ("_rpcRequest" :: String)) } ''RpcRequest

data RpcResponse = RpcResponse
  { _rpcResponseJsonRpc :: String
  , _rpcResponseId      :: Value
  , _rpcResponseResult  :: Maybe Value
  , _rpcResponseError   :: Maybe Value
  } deriving (Show)

makeLenses ''RpcResponse
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length ("_rpcResponse" :: String)) } ''RpcResponse

data RpcNotification = RpcNotification
  { _rpcNotificationJsonRpc :: String
  , _rpcNotificationMethod  :: String
  , _rpcNotificationParams  :: Maybe Value
  } deriving (Show)

makeLenses ''RpcNotification
deriveJSON defaultOptions { fieldLabelModifier = fmap toLower . drop (length ("_rpcNotification" :: String)) } ''RpcNotification

