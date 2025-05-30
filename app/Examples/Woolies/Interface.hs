module Examples.Woolies.Interface where

import           Data.Map  (fromList)
import           MCP.Types (DocTable)

data Prompt = BuildRecipeAndShoppingList { description :: String }

data Tool
    = GetCategories
    | SearchProductsByName { name :: String, category :: Maybe String }
    | SearchProductsByDetail { detail :: String, category :: Maybe String  }
    | SearchProductsByIngredient { ingredient :: String, category :: Maybe String  }

promptDoc :: DocTable
promptDoc = fromList
  -- constructor docs
  [ ( "BuildRecipeAndShoppingList" , "Build a recipe and shopping list for a dish!" )

  -- field-level docs
  , ( "description", "Description of the dish" )
  ]

toolDoc :: DocTable
toolDoc = fromList
  -- constructor docs
  [ ( "GetCategories",                "Get all the categories of food available from Woolworths South Africa catalog (to help refine searches)" )
  , ( "SearchProductsByName",         "Search Woolworths South Africa for products by matching name (results are limited so try different queries to find alternatives)" )
  , ( "SearchProductsByDetail",       "Search Woolworths South Africa for products by matching contents in the products description (results are limited so try different queries to find alternatives)" )
  , ( "SearchProductsByIngredient",   "Search Woolworths South Africa for products by matching ingredients in the product (results are limited so try different queries to find alternatives)" )

  -- field-level docs
  , ( "name",         "Matching this search criteria, using 'contains' semantics" )
  , ( "detail",       "Matching this search criteria, using 'contains' semantics" )
  , ( "ingredient",   "Matching this search criteria, using 'contains' semantics" )
  , ( "category",     "Optional category to filter results, if not provided all categories are searched" )
  ]
