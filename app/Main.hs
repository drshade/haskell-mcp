module Main where

import           Examples.Planets.Handlers    as Planets
import           Examples.Ruddr.Handlers      as Ruddr
import           Examples.Salesforce.Handlers as Salesforce
import           Examples.Woolies.Handlers    as Woolies
import           MCP.Server                   (McpServerInfo (..),
                                               runMcpServerStdIn)
import           System.Environment           (getArgs)

mainPlanets :: IO ()
mainPlanets =
    runMcpServerStdIn
        McpServerInfo
            { serverName = "Planets"
            , serverVersion = "0.1.0"
            , serverInstructions = "Planets MCP Server"
            }
        Planets.prompts Planets.invokePrompt Planets.executePrompt
        Planets.tools Planets.invokeTool Planets.executeTool

mainSalesforce :: IO ()
mainSalesforce =
    runMcpServerStdIn
        McpServerInfo
            { serverName = "Salesforce"
            , serverVersion = "0.1.0"
            , serverInstructions = "Use these tools to interact with Salesforce to get opportunities and other data."
            }
        Salesforce.prompts Salesforce.invokePrompt Salesforce.executePrompt
        Salesforce.tools Salesforce.invokeTool Salesforce.executeTool

mainRuddr :: IO ()
mainRuddr =
    runMcpServerStdIn
        McpServerInfo
            { serverName = "Projects in Delivery"
            , serverVersion = "0.1.0"
            , serverInstructions = "Use these tools to fetch information about projects in delivery"
            }
        Ruddr.prompts Ruddr.invokePrompt Ruddr.executePrompt
        Ruddr.tools Ruddr.invokeTool Ruddr.executeTool

mainWoolies :: IO ()
mainWoolies =
    runMcpServerStdIn
        McpServerInfo
            { serverName = "Products available at Woolworths South Africa Grocery Store"
            , serverVersion = "0.1.0"
            , serverInstructions = "Use these tools to fetch information about products available at Woolworths South Africa, including prices, ingredients, descriptions etc."
            }
        Woolies.prompts Woolies.invokePrompt Woolies.executePrompt
        Woolies.tools Woolies.invokeTool Woolies.executeTool

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["planets"]    -> mainPlanets
        ["salesforce"] -> mainSalesforce
        ["ruddr"]      -> mainRuddr
        ["woolies"]    -> mainWoolies
        _              -> mainSalesforce
