module Examples.Woolies.Interface where

import           Data.Map  (fromList)
import           MCP.Types (DocTable)

data Prompt
  = BuildRecipeAndShoppingList { description :: String }
  | PlanAndOrderWeeklyShopping { description :: String }

data Tool
    = GetCategories
    | SearchProductsByName { name :: String, category :: Maybe String }
    | SearchProductsByDetail { detail :: String, category :: Maybe String  }
    | SearchProductsByIngredient { ingredient :: String, category :: Maybe String  }
    | CheckoutAndPay { skus :: String }

promptDoc :: DocTable
promptDoc = fromList
  -- constructor docs
  [ ( "BuildRecipeAndShoppingList" , "Build a recipe and shopping list for a dish!" )
  , ( "PlanAndOrderWeeklyShopping" , "Plan and order a weekly shopping list for my family" )

  -- field-level docs
  , ( "description", "Additional description, preferences, ideas, etc" )
  ]

toolDoc :: DocTable
toolDoc = fromList
  -- constructor docs
  [ ( "GetCategories",                "Get all the categories of food available from Woolworths South Africa catalog (to help refine searches)" )
  , ( "SearchProductsByName",         "Search Woolworths South Africa for products by matching name (results are limited so try different queries to find alternatives)" )
  , ( "SearchProductsByDetail",       "Search Woolworths South Africa for products by matching contents in the products description (results are limited so try different queries to find alternatives)" )
  , ( "SearchProductsByIngredient",   "Search Woolworths South Africa for products by matching ingredients in the product (results are limited so try different queries to find alternatives)" )
  , ( "CheckoutAndPay",               "Checkout and pay for the products provided (comma seperated list of skus) - ALWAYS ASK FOR CONFIRMATION BEFORE RUNNING THIS" )

  -- field-level docs
  , ( "name",         "Matching this search criteria, using 'contains' semantics" )
  , ( "detail",       "Matching this search criteria, using 'contains' semantics" )
  , ( "ingredient",   "Matching this search criteria, using 'contains' semantics" )
  , ( "category",     "Optional category to filter results, if not provided all categories are searched" )
  , ( "skus",         "Comma separated list of skus to checkout and pay for" )
  ]
