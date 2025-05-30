{-# LANGUAGE OverloadedStrings #-}

module Examples.Woolies.Handlers where

import qualified Data.Text                  as T
import           Examples.Woolies.DB        as DB
import           Examples.Woolies.Interface
import           MCP.Derive
import           MCP.Types                  (PromptDefinition, ToolDefinition)

$(deriveInvokePrompt ''Prompt)
$(deriveInvokeTool ''Tool)

prompts :: [PromptDefinition]
prompts = $(derivePrompts ''Prompt 'promptDoc)

tools :: [ToolDefinition]
tools = $(deriveTools ''Tool 'toolDoc)

executePrompt :: Prompt -> IO String
executePrompt (BuildRecipeAndShoppingList description) =
    pure $
        "Please build me a recipe and shopping list based on the following description: " ++ description
        ++ "\nAll ingredients should be available from Woolworths South Africa, and you should use the available tools to query and search for relevant products from their catalog."
        ++ "\nDo not leave out any ingredients, other than those things that would be generally available in the kitchen such as salt, pepper, sugar, olive oil, cooking oil, etc."
        ++ "\nGenerally it helps if you query the list of categories first, and then search for products in those categories so that you can be more specific in your queries."
        ++ "\nAlways return the results in a format that is easy to read, such as a list of ingredients with their quantities and any other relevant details, and then provide the recipe itself."
        ++ "\nInclude the full shopping list with all the ingredients and their quantities in a table format, including prices and links to the products on the website."

executeTool :: Tool -> IO String
executeTool GetCategories = do
    result <- DB.queryCategories
    pure $ either id unlines result
executeTool (SearchProductsByName name category) = do
    result <- DB.queryByName (T.pack name) (T.pack <$> category)
    pure $ either id unlines result
executeTool (SearchProductsByDetail detail category) = do
    result <- DB.queryByDetail (T.pack detail) (T.pack <$> category)
    pure $ either id unlines result
executeTool (SearchProductsByIngredient ingredient category) = do
    result <- DB.queryByIngredient (T.pack ingredient) (T.pack <$> category)
    pure $ either id unlines result
