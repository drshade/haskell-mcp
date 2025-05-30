{-# LANGUAGE OverloadedStrings #-}

module Examples.Salesforce.API where

import           Control.Exception    (SomeException, displayException, try)
import           Data.Aeson           (Value (..), withObject, (.:), (.:?))
import qualified Data.Aeson.KeyMap    as KM
import           Data.Aeson.Types     (parseEither)
import           Data.ByteString      (ByteString)
import           Data.List            (intercalate)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Network.HTTP.Req     (header)
import           Network.HTTP.Req     hiding (header)
import           Network.OAuth.OAuth2 (AccessToken (..))
import           System.Directory     (getCurrentDirectory)
import           System.Environment   (lookupEnv)
import           System.IO.Error      (tryIOError)
import qualified Text.URI             as URI
import           Text.URI             (URI)

getCredentials :: IO (Text, Text, Text, Text)
getCredentials = do
    mKey      <- lookupEnv "SALESFORCE_KEY"
    mSecret   <- lookupEnv "SALESFORCE_SECRET"
    mUsername <- lookupEnv "SALESFORCE_USERNAME"
    mPassword <- lookupEnv "SALESFORCE_PASSWORD"
    case (mKey, mSecret, mUsername, mPassword) of
      (Just key, Just secret, Just username, Just password) ->
        pure (T.pack key, T.pack secret, T.pack username, T.pack password)
      _ -> do
        let filename = "credentials/salesforce"
        credentials <- tryIOError $ readFile filename
        case credentials of
          Left _ -> do
            cwd <- getCurrentDirectory
            error $ "Attempting to read " <> filename <> " file from working directory: " ++ cwd
          Right contents -> case lines contents of
            key : secret : username : password : _ -> pure (T.pack key, T.pack secret, T.pack username, T.pack password)
            _ -> error $ "Expected 4 lines in a file named " <> filename <> " - key, secret, username, password"

getToken :: IO AccessToken
getToken = do
  (key, secret, username, password) <- getCredentials
  let loginUrl = "login.salesforce.com"
      creds =
               "grant_type"     =: ("password" :: Text)
            <> "client_id"      =: key
            <> "client_secret"  =: secret
            <> "username"       =: username
            <> "password"       =: password
  r <- runReq defaultHttpConfig $
         req POST (https loginUrl /: "services" /: "oauth2" /: "token")
             (ReqBodyUrlEnc creds) jsonResponse mempty
  let body = responseBody r :: Value
  case parseEither (withObject "Token" (.: "access_token")) body of
    Right tokTxt -> pure (AccessToken tokTxt)
    Left err     -> fail ("OAuth response parse failed: " <> err)

query :: AccessToken -> String -> IO (Either String [String])
query tok soql = do
  -- Catch any Req-level exceptions (network errors, non‑2xx, etc.)
  result <- try $ runReq defaultHttpConfig (go initialUri queryOpt [])
  case result of
    Left  (ex :: SomeException) -> pure (Left (displayException ex))
    Right res                   -> pure res
  where
    apiVer      = "v60.0" :: Text
    domain      = "https://synthesis3.my.salesforce.com" :: Text

    baseUriText = domain <> "/services/data/" <> apiVer <> "/query"
    initialUri  = case URI.mkURI baseUriText of
                    Just u  -> u
                    Nothing -> error $ "Invalid base URI: " <> T.unpack baseUriText

    queryOpt    = "q" =: T.pack soql

    tokenBs :: AccessToken -> ByteString
    tokenBs (AccessToken t) = TE.encodeUtf8 t

    -- | Turn a record (JSON object) into a single CSV line.
    --   * Primitive values are rendered via 'valueToText'.
    --   * Nested objects are flattened depth‑first; their primitive leaves
    --     are appended in field order, ignoring any \"attributes\" node that
    --     Salesforce adds for sub‑objects.
    recordToCSV :: Value -> String
    recordToCSV (Object o) = intercalate "," (concatMap flatten (KM.elems o'))
      where
        -- Drop the auto‑generated "attributes" wrapper field
        o' = KM.delete "attributes" o

        flatten :: Value -> [String]
        flatten (Object inner) = concatMap flatten (KM.elems (KM.delete "attributes" inner))
        flatten v              = [valueToText v]

    -- Fallback for non‑object values (shouldn't normally happen).
    recordToCSV v = valueToText v

    valueToText :: Value -> String
    valueToText (String t) = T.unpack t
    valueToText (Number n) = show n
    valueToText (Bool b)   = show b
    valueToText _          = ""

    -- | Walk the pagination chain.  Accumulates rows or bails out with Left.
    go :: URI -> Option 'Https -> [String] -> Req (Either String [String])
    go uri extraOpts acc =
      case useHttpsURI uri of
        Nothing -> pure (Left "Malformed URI")
        Just (url', optsFromUri) -> do
          let authHdr      = "Bearer " <> tokenBs tok
              combinedOpts = optsFromUri <> extraOpts <> header "Authorization" authHdr
          r <- req GET url' NoReqBody jsonResponse combinedOpts

          let body = responseBody r :: Value
          case parseEither
                 ( withObject "QueryResponse" $ \o -> do
                     rs  <- o .:  "records"
                     nxt <- o .:? "nextRecordsUrl"
                     pure (rs :: [Value], nxt :: Maybe Text)
                 ) body of
            Left err -> pure (Left ("SOQL parse error: " <> err))
            Right (recs, more) -> do
              let rowStrings = map recordToCSV recs
                  acc'       = acc ++ rowStrings
              case more of
                Nothing       -> pure (Right acc')
                Just nextPath ->
                  let nextUriText = domain <> nextPath
                  in case URI.mkURI nextUriText of
                       Just nextUri -> go nextUri mempty acc'
                       Nothing      -> pure (Left "Malformed nextRecordsUrl")

demo :: IO ()
demo = do
  putStrLn "🔐  Fetching token..."
  token <- getToken
  putStrLn "📥  Querying Opportunity rows..."
  opps  <- query token "SELECT Id, Name, Owner.Name, CloseDate, StageName, Amount \
                                 \FROM Opportunity \
                                 \WHERE CloseDate >= 2025-05-01 \
                                 \ AND CloseDate <= 2025-05-31 \
                                 \ AND StageName NOT IN ('Closed Won', 'Closed Lost')"
  case opps of
    Left err   -> putStrLn ("❌  Query failed: " <> err)
    Right rows -> do
      putStrLn $ "✅  Pulled " ++ show (length rows) ++ " rows"
      print rows
