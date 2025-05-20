{-# LANGUAGE OverloadedStrings #-}
module MCP.Server where

import           Control.Concurrent         (forkIO)
import           Control.Concurrent.STM     (TChan, atomically, newTChanIO,
                                             readTChan, writeTChan)
import           Control.Monad              (forever)
import           Data.Aeson                 (Value (String), decode, encode,
                                             object, (.!=), (.:), (.:?), (.=))
import           Data.Aeson.Types           (Parser)
import qualified Data.Aeson.Types           as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map                   as M
import qualified Data.Text                  as T (pack, unpack)
import           MCP.Derive                 (ArgumentInvocation (..),
                                             PromptInvocation (PromptInvocation),
                                             ToolInvocation (ToolInvocation))
import           MCP.Highlevel              (executePrompt, executeTool,
                                             getPrompts'', getTools'',
                                             invokePrompt, invokeTool)
import           MCP.Spec
import           Optics                     ((^.))
import           System.IO                  (BufferMode (LineBuffering),
                                             hPutStrLn, hSetBuffering, stderr,
                                             stdin, stdout)
-- | JSON structure for params of "prompts/get"
runMcpServerStdIn :: IO ()
runMcpServerStdIn = do
  -- Claude Desktop launches the binary and speaks line‑delimited JSON over
  -- stdio.  Line buffering keeps latency low and works cross‑platform.
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  -- Outgoing messages funnel through a single channel so multiple handler
  -- threads can write without colliding.
  outChan <- newTChanIO @BL.ByteString

  -- Writer thread: drains the channel and prints to stdout.
  _ <- forkIO $ writerLoop outChan

  -- Reader loop lives on the main thread.
  forever $ do
    line <- B.getLine
    let msg = BL.fromStrict line

    -- Attempt to decode as a request
    case decode msg :: Maybe RpcRequest of
      Just request -> handleRequest outChan request
      Nothing  ->
        case decode msg :: Maybe RpcNotification of
            Just notification -> handleNotification notification
            Nothing           -> error "What do we do?"

writerLoop :: TChan BL.ByteString -> IO ()
writerLoop ch = forever $ do
  out <- atomically $ readTChan ch
  BL.putStrLn out

sendResult :: TChan BL.ByteString -> Value -> Value -> IO ()
sendResult ch mid res = atomically $
  writeTChan ch $ encode $ object
    [ "jsonrpc" .= String "2.0"
    , "id"      .= mid
    , "result"  .= res
    ]

sendError :: TChan BL.ByteString -> Value -> String -> IO ()
sendError ch mid msg = atomically $
  writeTChan ch $ encode $ object
    [ "jsonrpc" .= String "2.0"
    , "id"      .= mid
    , "error"   .= object [ "code" .= (-32603 :: Int)
                           , "message" .= msg
                           , "data"    .= object []
                           ]
    ]

handleNotification :: RpcNotification -> IO ()
handleNotification _notification = do
  hPutStrLn stderr $ "Notification: " ++ show _notification
  -- Ignore notifications for now...
  pure ()

handleRequest :: TChan BL.ByteString -> RpcRequest -> IO ()
handleRequest outChan req = do
  hPutStrLn stderr $ "Received: " ++ show req
  case req ^. rpcRequestMethod of
    "initialize" ->
      sendResult outChan (req ^. rpcRequestId) $ object
        [ "protocolVersion" .= String "2024-11-05"
        , "serverInfo"      .= object
            [ "name"    .= String "Planet-MCP-Server"
            , "version" .= String "1.0.0"
            ]
        , "instructions" .= String "Provides access to Planets.nu (VGAPlanets) game state!"
        , "capabilities" .= object
            [ "logging" .= object []
            , "prompts" .= object []
            , "resources" .= object []
            , "tools"   .= object []
            ]
        ]

    "notifications/initialized" ->
        -- Ignore these...
        pure ()

    "resources/list" ->
        sendResult outChan (req ^. rpcRequestId) $ object
            [ "resources" .=
                [ object
                    [ "uri" .= String "planets://status"
                    , "name" .= String "Status"
                    , "description" .= String "Status of the game"
                    , "mimeType" .= String "application/json"
                    ]
                ]
            ]

    "tools/list" -> do
        sendResult outChan (req ^. rpcRequestId) $ object
            [ "tools"      .= getTools''
            ]

    "tools/call" -> do
        case req ^. rpcRequestParams of
          Just paramsVal
            | Just (tname, argMap) <- parseToolParams paramsVal -> do
                let argList = [ ArgumentInvocation k v | (k,v) <- M.toList argMap ]
                    tinv    = ToolInvocation tname argList       -- ToolInvocation iso
                case invokeTool tinv of
                  Left err    -> sendError outChan (req ^. rpcRequestId) err
                  Right tInst -> do
                    resultStr <- executeTool tInst
                    let resultVal = String (T.pack resultStr) :: Value
                    sendResult outChan (req ^. rpcRequestId) $ object
                        [ "content" .=
                            [ object [ "type" .= String "text", "text" .= resultVal ] ]
                        , "isError" .= False
                        ]
            | otherwise ->
                sendError outChan (req ^. rpcRequestId) "Invalid params"
          Nothing ->
                sendError outChan (req ^. rpcRequestId) "Missing params"

    "prompts/list" ->
        sendResult outChan (req ^. rpcRequestId) $ object
            [ "prompts" .= getPrompts''
            ]

    "prompts/get" -> do
        -- Extract params value

        case req ^. rpcRequestParams of
          Just paramsVal
            | Just (pname, argMap) <- parsePromptParams paramsVal -> do
                let argList = [ ArgumentInvocation k v | (k,v) <- M.toList argMap ]
                    pinv    = PromptInvocation pname argList
                case invokePrompt pinv of
                  Left err    -> sendError outChan (req ^. rpcRequestId) err
                  Right pInst -> do
                    resultStr <- executePrompt pInst
                    let resultVal = String (T.pack resultStr) :: Value
                    sendResult outChan (req ^. rpcRequestId) $ object
                        [ "description" .= String "Prompt result"
                        , "messages" .=
                            [ object
                                [ "role" .= String "user"
                                , "content" .= object
                                    [ "type" .= String "text"
                                    , "text" .= resultVal
                                    ]
                                ]
                            ]
                        ]
            | otherwise ->
                sendError outChan (req ^. rpcRequestId) "Invalid params"
          Nothing ->
            sendError outChan (req ^. rpcRequestId) "Missing params"

    _ -> sendError outChan (req ^. rpcRequestId) "method not implemented"

------------------------------------------------------------------
-- | Extract (name, arguments) using plain Aeson combinators.
------------------------------------------------------------------
-- | Extract (promptName, arguments) from the JSON‑RPC params object.
--   Fails if "name" is missing or if any argument value is not a string.
parsePromptParams :: A.Value -> Maybe (String, M.Map String String)
parsePromptParams = A.parseMaybe $ A.withObject "params" $ \o -> do
  name :: String <- o .: "name"
  args :: M.Map String A.Value <- o .:? "arguments" .!= mempty
  argMap <- traverse valueToString args
  pure (name, argMap)
  where
    valueToString :: A.Value -> Parser String
    valueToString (A.String t) = pure (T.unpack t)
    valueToString _            = fail "expected string for argument value"

------------------------------------------------------------------
-- | Extract (toolName, arguments) from JSON‑RPC params.
------------------------------------------------------------------
parseToolParams :: A.Value -> Maybe (String, M.Map String String)
parseToolParams = parsePromptParams  -- identical structure
