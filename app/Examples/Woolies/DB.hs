{-# LANGUAGE OverloadedStrings #-}

module Examples.Woolies.DB where

import           Control.Exception                  (SomeException, try)
import           Data.ByteString                    (ByteString)
import           Data.Scientific                    (Scientific)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time                          (LocalTime)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

type ProductRow = String

connectionString :: ByteString
connectionString = "postgresql://tom@localhost/groceries"

queryCategories :: IO (Either String [String])
queryCategories = do
  conn <- connectPostgreSQL connectionString
  result <- try $ query conn sql () :: IO (Either SomeException [Only Text])
  case result of
    Left ex    -> return $ Left $ "Database error: " <> show ex
    Right rows -> return $ Right $ map (T.unpack . fromOnly) rows
  where
    sql = "SELECT DISTINCT category FROM products WHERE dept = 'FOODS'"

queryByName :: Text -> Maybe Text -> IO (Either String [String])
queryByName namePattern category = do
  conn <- connectPostgreSQL connectionString
  result <- case category of
    Nothing -> try $ query conn sqlWithoutCategory (Only $ "%" <> namePattern <> "%")
    Just cat -> try $ query conn sqlWithCategory ("%" <> namePattern <> "%", cat)
  case (result :: Either SomeException [ProductRecord]) of
    Left ex    -> return $ Left $ "Database error: " <> show ex
    Right rows -> return $ Right $ map rowToCsv rows
  where
    baseSql = "SELECT id, store, name, dept, category, price, sku, url, updated_at, detail, ingredients, review_count, average_rating, image_url FROM products WHERE name ILIKE ?"
    sqlWithoutCategory = baseSql <> " LIMIT 8"
    sqlWithCategory = baseSql <> " AND category = ? LIMIT 8"

-- | Query products by detail pattern match
queryByDetail :: Text -> Maybe Text -> IO (Either String [String])
queryByDetail detailPattern category = do
  conn <- connectPostgreSQL connectionString
  result <- case category of
    Nothing -> try $ query conn sqlWithoutCategory (Only $ "%" <> detailPattern <> "%")
    Just cat -> try $ query conn sqlWithCategory ("%" <> detailPattern <> "%", cat)
  case (result :: Either SomeException [ProductRecord]) of
    Left ex    -> return $ Left $ "Database error: " <> show ex
    Right rows -> return $ Right $ map rowToCsv rows
  where
    baseSql = "SELECT id, store, name, dept, category, price, sku, url, updated_at, detail, ingredients, review_count, average_rating, image_url FROM products WHERE detail ILIKE ?"
    sqlWithoutCategory = baseSql <> " LIMIT 8"
    sqlWithCategory = baseSql <> " AND category = ? LIMIT 8"

-- | Query products by ingredients pattern match
queryByIngredient :: Text -> Maybe Text -> IO (Either String [String])
queryByIngredient ingredientPattern category = do
  conn <- connectPostgreSQL connectionString
  result <- case category of
    Nothing -> try $ query conn sqlWithoutCategory (Only $ "%" <> ingredientPattern <> "%")
    Just cat -> try $ query conn sqlWithCategory ("%" <> ingredientPattern <> "%", cat)
  case (result :: Either SomeException [ProductRecord]) of
    Left ex    -> return $ Left $ "Database error: " <> show ex
    Right rows -> return $ Right $ map rowToCsv rows
  where
    baseSql = "SELECT id, store, name, dept, category, price, sku, url, updated_at, detail, ingredients, review_count, average_rating, image_url FROM products WHERE ingredients ILIKE ?"
    sqlWithoutCategory = baseSql <> " LIMIT 8"
    sqlWithCategory = baseSql <> " AND category = ? LIMIT 8"

-- Internal types and helpers
data ProductRecord = ProductRecord
  { prodId            :: Int
  , prodStore         :: Text
  , prodName          :: Text
  , prodDept          :: Text
  , prodCategory      :: Text
  , prodPrice         :: Int
  , prodSku           :: Text
  , prodUrl           :: Text
  , prodUpdatedAt     :: LocalTime
  , prodDetail        :: Maybe Text
  , prodIngredients   :: Maybe Text
  , prodReviewCount   :: Maybe Int
  , prodAverageRating :: Maybe Scientific
  , prodImageUrl      :: Maybe Text
  } deriving (Show)

instance FromRow ProductRecord where
  fromRow = ProductRecord <$> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field

-- Convert a product record to CSV format
rowToCsv :: ProductRecord -> String
rowToCsv p = T.unpack $ T.intercalate ","
  [ quote $ T.pack $ show $ prodId p
  , quote $ prodStore p
  , quote $ prodName p
  , quote $ prodDept p
  , quote $ prodCategory p
  , quote $ T.pack $ show $ prodPrice p
  , quote $ prodSku p
  , quote $ prodUrl p
  , quote $ T.pack $ show $ prodUpdatedAt p
  , quote $ maybe "" id $ prodDetail p
  , quote $ maybe "" id $ prodIngredients p
  , quote $ maybe "" (T.pack . show) $ prodReviewCount p
  , quote $ maybe "" (T.pack . show) $ prodAverageRating p
  , quote $ maybe "" id $ prodImageUrl p
  ]
  where
    quote txt = "\"" <> T.replace "\"" "\"\"" txt <> "\""
