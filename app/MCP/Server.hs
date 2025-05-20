{-# LANGUAGE OverloadedStrings #-}

module MCP.Server where

import           Control.Concurrent         (forkIO)
import           Control.Concurrent.STM     (TChan, atomically, newTChanIO,
                                             readTChan, writeTChan)
import           Control.Monad              (forever)
import           Data.Aeson                 (Value (String), decode, encode,
                                             object, (.=))
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           MCP.Spec
import           Optics                     ((^.))
import           System.IO                  (BufferMode (LineBuffering),
                                             hSetBuffering, stdin, stdout)

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
  -- hPutStrLn stderr $ "Received: " ++ show env
  -- Ignore notifications for now...
  pure ()

handleRequest :: TChan BL.ByteString -> RpcRequest -> IO ()
handleRequest outChan req = do
  -- hPutStrLn stderr $ "Received: " ++ show env
  case req ^. rpcRequestMethod of
    "initialize" ->
      sendResult outChan (req ^. rpcRequestId) $ object
        [ "protocolVersion" .= String "2025-03-26"
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

    "tools/list" ->
        sendResult outChan (req ^. rpcRequestId) $ object
            [ "tools" .=
                [ object
                    [ "name" .= String "get_status"
                    , "description" .= String "Get the current status of the game"
                    , "inputSchema" .= object
                        [ "type" .= String "object"
                        , "properties" .= object
                            [ "gameid" .= object
                                [ "type" .= String "string"
                                , "description" .= String "Id of the game"
                                ]
                            ]
                        -- required is a list of strings
                        , "required" .= ["gameid" :: String]
                        ]
                    ]
                ]
            ]

    "prompts/list" ->
        sendResult outChan (req ^. rpcRequestId) $ object
            [ "prompts" .= object []
            ]

    -- handle: {"method":"tools/call","params":{"name":"get_status","arguments":{"gameid":"PLACEHOLDER"}},"jsonrpc":"2.0","id":5}
    "tools/call" -> do
        sendResult outChan (req ^. rpcRequestId) $ object
            [ "content" .=
                [ object
                    [ "type" .= String "text"
                    , "text" .= String "Game is running and you are winning against all the other players!"
                    ]
                ]
            ]

    _ -> sendError outChan (req ^. rpcRequestId) "method not implemented"
