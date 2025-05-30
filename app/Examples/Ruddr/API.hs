{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Examples.Ruddr.API where

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Data.Aeson
import qualified Data.ByteString.Char8     as BS8
import           Data.List                 (intercalate)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           GHC.Generics              (Generic)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status (statusCode)
import           Network.URI               (escapeURIString, isUnreserved)
import           System.Directory          (getCurrentDirectory)
import           System.IO.Error           (tryIOError)

-- | Configuration for Ruddr API client
data RuddrConfig = RuddrConfig
  { ruddrApiKey  :: Text
  , ruddrBaseUrl :: Text
  } deriving (Show, Eq)

-- | Client environment for API operations
newtype RuddrClient = RuddrClient RuddrConfig

-- | Create a new Ruddr client with default base URL
newRuddrClient :: Text -> RuddrClient
newRuddrClient apiKey = RuddrClient $ RuddrConfig
  { ruddrApiKey = apiKey
  , ruddrBaseUrl = "https://www.ruddr.io/api/workspace"
  }

-- | Monad for Ruddr API operations
type RuddrM = ReaderT RuddrClient IO

-- | Run a Ruddr API operation
runRuddr :: RuddrClient -> RuddrM a -> IO a
runRuddr client action = runReaderT action client

-- | Client information embedded in project responses
data Client = Client
  { clientId   :: Text
  , clientName :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Client where
  parseJSON = withObject "Client" $ \o -> Client
    <$> o .: "id"
    <*> o .: "name"

-- | Project data structure based on actual Ruddr API response
data Project = Project
  { projectId               :: Text
  , projectName             :: Text
  , projectKey              :: Maybe Text
  , projectCode             :: Maybe Text
  , projectNotes            :: Maybe Text
  , projectStatusId         :: Maybe Text
  , projectRecordStatusId   :: Maybe Text
  , projectBillingTypeId    :: Maybe Text
  , projectClient           :: Maybe Client
  , projectStart            :: Maybe Text
  , projectEnd              :: Maybe Text
  , projectCurrency         :: Maybe Text
  , projectIsBillable       :: Maybe Bool
  , projectIsProductive     :: Maybe Bool
  , projectCreatedAt        :: Text
  , projectUseBudget        :: Maybe Bool
  , projectUseMonthlyBudget :: Maybe Bool
  , projectUseRoles         :: Maybe Bool
  , projectRequiresNotes    :: Maybe Bool
  , projectRequiresTasks    :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \o -> Project
    <$> o .: "id"
    <*> o .: "name"
    <*> o .:? "key"
    <*> o .:? "code"
    <*> o .:? "notes"
    <*> o .:? "statusId"
    <*> o .:? "recordStatusId"
    <*> o .:? "billingTypeId"
    <*> o .:? "client"
    <*> o .:? "start"
    <*> o .:? "end"
    <*> o .:? "currency"
    <*> o .:? "isBillable"
    <*> o .:? "isProductive"
    <*> o .: "createdAt"
    <*> o .:? "useBudget"
    <*> o .:? "useMonthlyBudget"
    <*> o .:? "useRoles"
    <*> o .:? "requiresNotes"
    <*> o .:? "requiresTasks"

-- | Response wrapper for API responses
data ApiResponse a = ApiResponse
  { responseData    :: a
  , responseHasMore :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON = withObject "ApiResponse" $ \o ->
    ApiResponse <$> o .: "results"
                <*> o .:? "hasMore"

readCredentials :: IO Text
readCredentials = do
    let filename :: String
        filename = "credentials/ruddr"
    -- putStrLn $ "🔐  Reading credentials from file " <> filename
    credentials <- tryIOError $ readFile filename
    case credentials of
        Left _ -> do
            cwd <- getCurrentDirectory
            error $ "Attempting to read " <> filename <> " file from working directory: " ++ cwd
        Right contents -> case lines contents of
            key : _ -> pure $ T.pack key
            _ -> error $ "Expected 1 line in a file named " <> filename <> " - key"


-- | Convert a project to CSV row
projectToCsvRow :: Project -> [String]
projectToCsvRow Project{..} =
  [ T.unpack projectId
  , T.unpack projectName
  , maybe "" T.unpack projectKey
  , maybe "" T.unpack projectCode
  , maybe "" (T.unpack . T.take 100) projectNotes  -- Truncate notes for CSV
  , maybe "" T.unpack projectStatusId
  , maybe "" T.unpack projectRecordStatusId
  , maybe "" T.unpack projectBillingTypeId
  , maybe "" (T.unpack . clientName) projectClient
  , maybe "" (T.unpack . clientId) projectClient
  , maybe "" T.unpack projectStart
  , maybe "" T.unpack projectEnd
  , maybe "" T.unpack projectCurrency
  , maybe "" show projectIsBillable
  , maybe "" show projectIsProductive
  , T.unpack projectCreatedAt
  , maybe "" show projectUseBudget
  , maybe "" show projectUseMonthlyBudget
  , maybe "" show projectUseRoles
  , maybe "" show projectRequiresNotes
  , maybe "" show projectRequiresTasks
  ]

-- | CSV header for projects
projectCsvHeader :: [String]
projectCsvHeader =
  [ "id"
  , "name"
  , "key"
  , "code"
  , "notes"
  , "statusId"
  , "recordStatusId"
  , "billingTypeId"
  , "clientName"
  , "clientId"
  , "start"
  , "end"
  , "currency"
  , "isBillable"
  , "isProductive"
  , "createdAt"
  , "useBudget"
  , "useMonthlyBudget"
  , "useRoles"
  , "requiresNotes"
  , "requiresTasks"
  ]

-- | Convert list of strings to CSV line
toCsvLine :: [String] -> String
toCsvLine = intercalate "," . map escapeCsvField
  where
    escapeCsvField field
      | any (`elem` field) [',', '"', '\n', '\r'] = "\"" ++ concatMap escapeQuote field ++ "\""
      | otherwise = field
    escapeQuote '"' = "\"\""
    escapeQuote c   = [c]

-- | Convert projects to CSV format
projectsToCsv :: [Project] -> [String]
projectsToCsv projects =
  toCsvLine projectCsvHeader : map (toCsvLine . projectToCsvRow) projects

-- | Create an authenticated HTTP request with optional query parameters
createRequest :: Text -> [(Text, Text)] -> RuddrM Request
createRequest endpoint queryParams = do
  RuddrClient config <- ask
  let baseUrl = T.unpack $ ruddrBaseUrl config <> "/" <> endpoint
      queryString = if null queryParams
                   then ""
                   else "?" <> T.intercalate "&" (map formatParam queryParams)
      fullUrl = baseUrl <> T.unpack queryString
      authHeader = "Bearer " <> encodeUtf8 (ruddrApiKey config)
  request <- liftIO $ parseRequest fullUrl
  return $ setRequestHeaders
    [ ("Authorization", authHeader)
    , ("Content-Type", "application/json")
    , ("Accept", "application/json")
    ] request
  where
    formatParam (key, value) = key <> "=" <> T.pack (escapeURIString isUnreserved (T.unpack value))

-- | Execute an HTTP request and handle the response
executeRequest :: FromJSON a => Request -> RuddrM (Either String a)
executeRequest request = do
  response <- liftIO $ httpBS request
  let status = statusCode $ getResponseStatus response
      body = getResponseBody response

  case status of
    200 -> case eitherDecodeStrict body of
      Left err     -> return $ Left $ "JSON parsing error: " <> err
      Right result -> return $ Right result
    401 -> return $ Left "Authentication failed: Invalid API key"
    403 -> return $ Left "Access denied: Insufficient permissions"
    404 -> return $ Left "Not found: The requested resource does not exist"
    429 -> return $ Left "Rate limit exceeded: Too many requests"
    _ -> return $ Left $ "HTTP error " <> show status <> ": " <> BS8.unpack body

-- | Get all projects and return as CSV
getProjectsCsv :: RuddrM (Either String [String])
getProjectsCsv = getProjectsCsvWithSearch Nothing

-- | Get projects with optional search filter and return as CSV
getProjectsCsvWithSearch :: Maybe Text -> RuddrM (Either String [String])
getProjectsCsvWithSearch searchTerm = do
  let queryParams = case searchTerm of
        Nothing   -> []
        Just term -> [("nameContains", term)]  -- Use Ruddr's official parameter
  request <- createRequest "projects" queryParams
  result <- executeRequest request :: RuddrM (Either String (ApiResponse [Project]))
  case result of
    Left err -> return $ Left err
    Right apiResponse -> do
      let projects = responseData apiResponse
      return $ Right $ projectsToCsv projects

-- | Get a specific project by ID and return as CSV
getProjectCsv :: Text -> RuddrM (Either String [String])
getProjectCsv projectId = do
  request <- createRequest ("projects/" <> projectId) []
  result <- executeRequest request :: RuddrM (Either String Project)
  case result of
    Left err      -> return $ Left err
    Right project -> return $ Right $ projectsToCsv [project]

-- | Demo function to get all projects as CSV
demoProjectsCsv :: Text -> IO ()
demoProjectsCsv apiKey = do
  let client = newRuddrClient apiKey
  putStrLn "Fetching all projects as CSV..."
  result <- runRuddr client getProjectsCsv
  case result of
    Left err -> putStrLn $ "Error: " <> err
    Right csvLines -> do
      putStrLn $ "Found " <> show (length csvLines - 1) <> " projects:"
      mapM_ putStrLn csvLines

-- | Demo function to search projects and return as CSV
demoSearchProjectsCsv :: Text -> Text -> IO ()
demoSearchProjectsCsv apiKey searchTerm = do
  let client = newRuddrClient apiKey
  putStrLn $ "Searching for projects containing '" <> T.unpack searchTerm <> "' as CSV..."
  result <- runRuddr client $ getProjectsCsvWithSearch (Just searchTerm)
  case result of
    Left err -> putStrLn $ "Error: " <> err
    Right csvLines -> do
      putStrLn $ "Found " <> show (length csvLines - 1) <> " matching projects:"
      mapM_ putStrLn csvLines

-- | Demo function to get a specific project as CSV
demoProjectCsv :: Text -> Text -> IO ()
demoProjectCsv apiKey projectId = do
  let client = newRuddrClient apiKey
  putStrLn $ "Fetching project " <> T.unpack projectId <> " as CSV..."
  result <- runRuddr client $ getProjectCsv projectId
  case result of
    Left err -> putStrLn $ "Error: " <> err
    Right csvLines -> do
      putStrLn "Project details:"
      mapM_ putStrLn csvLines

-- | Save CSV to file
saveProjectsCsvToFile :: Text -> FilePath -> Maybe Text -> IO ()
saveProjectsCsvToFile apiKey filename searchTerm = do
  let client = newRuddrClient apiKey
  result <- runRuddr client $ getProjectsCsvWithSearch searchTerm
  case result of
    Left err -> putStrLn $ "Error: " <> err
    Right csvLines -> do
      writeFile filename (unlines csvLines)
      putStrLn $ "CSV saved to " <> filename <> " with " <> show (length csvLines - 1) <> " projects"

