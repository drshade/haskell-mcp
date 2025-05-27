{-# LANGUAGE OverloadedStrings #-}
module Examples.Sage.API
  ( SageEnv(..)
  , Invoice(..)
  , listInvoices          -- ^ high-level helper
  , listInvoicesRaw       -- ^ lower-level, JSON Value
  ) where

import           Control.Exception    (SomeException, displayException, try)
import           Data.Aeson
import           Data.Aeson.Types     (parseEither)
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as TE
import           GHC.Generics         (Generic)
import           Network.HTTP.Req
import           Network.OAuth.OAuth2 (AccessToken (..))

--------------------------------------------------------------------------------
--  Environment
--------------------------------------------------------------------------------

data SageEnv = SageEnv
  { sageToken :: AccessToken    -- ^ Bearer token from OAuth
  , sageSite  :: Text           -- ^ resource_owner_id ( X-Site header )
  , sageHost  :: Text           -- ^ api.accounting.sage.com  (region specific)
  }

--------------------------------------------------------------------------------
--  Handy invoice projection
--------------------------------------------------------------------------------

data Invoice = Invoice
  { invoiceId     :: Text
  , invoiceNumber :: Text
  , contactName   :: Text
  , totalGross    :: Double
  } deriving (Show, Generic)

instance FromJSON Invoice where
  parseJSON = withObject "Invoice" $ \o ->
    Invoice <$> o .: "id"
            <*> o .: "reference"
            <*> (o .: "contact" >>= (.: "displayed_as"))
            <*> (o .: "total_amount")

--------------------------------------------------------------------------------
--  Public helpers
--------------------------------------------------------------------------------

-- | map over IO, then over Either
listInvoices
  :: SageEnv
  -> Maybe Text               -- ^ optional @contact_id@ to filter by client
  -> Maybe (Text,Text)        -- ^ optional (fromDate,toDate)  YYYY-MM-DD
  -> IO (Either String [Invoice])
listInvoices env mCid mDates =
  (>>= project) <$> listInvoicesRaw env mCid mDates
  where
    project :: Value -> Either String [Invoice]
    project = parseEither (withObject "Resp" (.: "sales_invoices"))

-- | Lower-level helper returning the raw JSON response.
listInvoicesRaw
  :: SageEnv
  -> Maybe Text
  -> Maybe (Text,Text)
  -> IO (Either String Value)
listInvoicesRaw env mCid mDates = do
  let SageEnv {sageToken = AccessToken tok, sageSite = site, sageHost = host} = env
      url  = https host /: "v3.1" /: "sales_invoices"
      base = header "Authorization" ("Bearer " <> tokBS)
           <> header "X-Site"        siteBS
      tokBS  = TE.encodeUtf8 tok
      siteBS = TE.encodeUtf8 site

      opts1 = maybe id (\cid -> (<> ("contact_id" =: cid))) mCid base
      opts2 = maybe opts1 (\(f,t) -> opts1
                          <> ("from_date" =: f)
                          <> ("to_date"   =: t)) mDates

  runReqCatch $ req GET url NoReqBody jsonResponse opts2
  where
    -- Wrap Req in try / Either and unwrap the JsonResponse
    runReqCatch :: FromJSON a => Req (JsonResponse a) -> IO (Either String a)
    runReqCatch act = do
      e <- try (runReq defaultHttpConfig act)
      pure $ case e of
        Left  (ex :: SomeException) -> Left (displayException ex)
        Right r                     -> Right (responseBody r)

-- demo :: IO ()
-- demo = do
--   env <- SageEnv <$> getSageToken   -- your own loader
--                  <*> getSageSite
--                  <*> pure "api.accounting.sage.com"

--   e <- listInvoices env
--                     (Just "ffRteb5wuy34wtsvghgGF...")  -- customer id
--                     (Just ("2025-01-01","2025-05-31"))

--   case e of
--     Right invs -> mapM_ print invs
--     Left  err  -> putStrLn ("❌ " <> err)
