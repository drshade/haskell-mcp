module Main where

import           Examples.Planets.Handlers as Planets
import           MCP.Server                (runMcpServerStdIn)

main :: IO ()
main = runMcpServerStdIn
            Planets.prompts Planets.invokePrompt Planets.executePrompt
            Planets.tools Planets.invokeTool Planets.executeTool
