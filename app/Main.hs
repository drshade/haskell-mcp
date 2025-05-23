module Main where

import           Examples.Planets.Handlers    as Planets
import           Examples.Salesforce.Handlers as Salesforce
import           MCP.Server                   (McpServerInfo (..),
                                               runMcpServerStdIn)

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

main :: IO ()
main = mainSalesforce
