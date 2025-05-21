{-# LANGUAGE OverloadedStrings #-}

module Examples.Salesforce.Demo where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON (..), Value, withObject, (.:),
                                         (.:?))
import           Data.Aeson.Types       (parseEither)
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Network.HTTP.Req       (header)
import           Network.HTTP.Req       hiding (header)
import           Network.OAuth.OAuth2   (AccessToken (..))
import           System.Directory       (getCurrentDirectory)
import           System.IO.Error        (tryIOError)
import qualified Text.URI               as URI
import           Text.URI               (URI)

----------------------------------------------------------------------
-- 0.  Data type for the rows you need
----------------------------------------------------------------------

readCredentials :: IO (Text, Text, Text, Text)
readCredentials = do
    let filename :: String
        filename = ".credential-salesforce"
    putStrLn $ "🔐  Reading credentials from file " <> filename
    credentials <- tryIOError $ readFile filename
    case credentials of
        Left _ -> do
            cwd <- getCurrentDirectory
            error $ "Attempting to read " <> filename <> " file from working directory: " ++ cwd
        Right contents -> case lines contents of
            key : secret : username : password : _ -> pure (T.pack key, T.pack secret, T.pack username, T.pack password)
            _ -> error $ "Expected 4 lines in a file named " <> filename <> " - key, secret, username, password"


data Opportunity = Opportunity
  { oppId     :: !Text
  , oppName   :: !Text
  , closeDate :: !Text
  , stageName :: !Text
  , amount    :: !(Maybe Double)
  } deriving Show

instance FromJSON Opportunity where
  parseJSON = withObject "Opportunity" $ \v ->
    Opportunity <$> v .:  "Id"
                <*> v .:  "Name"
                <*> v .:  "CloseDate"
                <*> v .:  "StageName"
                <*> v .:? "Amount"

----------------------------------------------------------------------
-- 1.  Hit Salesforce's token endpoint (username-password flow)
----------------------------------------------------------------------

getToken :: IO AccessToken
getToken = do
  (key, secret, username, password) <- readCredentials
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

----------------------------------------------------------------------
-- 2.  Run SOQL, follow nextRecordsUrl if present
----------------------------------------------------------------------

soqlForYear :: Int -> Text
soqlForYear yr =
  "SELECT Id, Name, CloseDate, StageName, Amount \
  \FROM Opportunity \
  \WHERE CALENDAR_YEAR(CloseDate) = " <> T.pack (show yr)

----------------------------------------------------------------------
-- Helper: turn AccessToken into ByteString for the HTTP header
----------------------------------------------------------------------

tokenBs :: AccessToken -> ByteString
tokenBs (AccessToken t) = TE.encodeUtf8 t

----------------------------------------------------------------------
-- 2.  Run SOQL, follow nextRecordsUrl if present
----------------------------------------------------------------------

queryAll :: AccessToken -> Int -> IO [Opportunity]
queryAll tok yr = runReq defaultHttpConfig (go initialUri queryOpt [])
  where
    apiVer  = "v60.0" :: Text
    domain  = "https://synthesis3.my.salesforce.com" :: Text

    -- Base URI without any query string—this parses cleanly
    baseUriText = domain <> "/services/data/" <> apiVer <> "/query" :: Text
    initialUri  = case URI.mkURI baseUriText of
                    Just u  -> u
                    Nothing -> error $ "Invalid base URI: " <> T.unpack baseUriText

    -- Attach the SOQL as a query parameter via Req’s Option mechanism
    queryOpt = "q" =: soqlForYear yr

    ------------------------------------------------------------
    -- Inner loop: follow nextRecordsUrl for pagination
    ------------------------------------------------------------
    go :: URI -> Option 'Https -> [Opportunity] -> Req [Opportunity]
    go uri extraOpts acc =
      case useHttpsURI uri of
        Nothing -> liftIO (fail "Malformed URI")
        Just (url', optsFromUri) -> do
          let authHdr      = "Bearer " <> tokenBs tok
              combinedOpts = optsFromUri <> extraOpts <> header "Authorization" authHdr
          r <- req GET url' NoReqBody jsonResponse combinedOpts

          let body = responseBody r :: Value
          (recs, more) <-
            liftIO $ case parseEither
                               ( withObject "QueryResponse" $ \o -> do
                                   rs  <- o .:  "records"
                                   nxt <- o .:? "nextRecordsUrl"
                                   pure (rs :: [Opportunity], nxt :: Maybe Text)
                               ) body of
                       Left err     -> fail ("SOQL parse error: " <> err)
                       Right parsed -> pure parsed

          let acc' = acc ++ recs
          case more of
            Nothing       -> pure acc'
            Just nextPath -> do
              let nextUriText = domain <> nextPath
              case URI.mkURI nextUriText of
                Just nextUri -> go nextUri mempty acc'
                Nothing      -> liftIO (fail "Malformed nextRecordsUrl")

----------------------------------------------------------------------
-- 3.  Entrypoint
----------------------------------------------------------------------

demo :: IO ()
demo = do
  let year = 2025
  putStrLn "🔐  Fetching token..."
  token <- getToken
  putStrLn "📥  Querying Opportunity rows..."
  opps  <- queryAll token year
  putStrLn $ "✅  Pulled " ++ show (length opps) ++ " rows"
  putStrLn $ show opps
  -- Later: pass 'opps' to your grouping/ MCP-publishing code
