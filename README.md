
# An MCP Server for Haskell

Wanna write cool MCP Prompts and Tools like this?
```haskell
data Prompt = RobotStyle { feel     :: String  }

data Tool
    = GetAllGames
    | GetGameStatus  { gameid :: String  }
    | MessagePlayers { message :: String }
```

And then invoke them like this?
```haskell
executePrompt :: Prompt -> IO String
executePrompt (RobotStyle feel) = pure $ "Speak like a ROBOT with the following feel: " ++ feel

executeTool :: Tool -> IO String
executeTool GetAllGames                 = pure "List of all games: [Game1, Game2, Game3]"
executeTool (GetGameStatus gameid)      = pure $ "The status of game " ++ gameid ++ " is: RUNNING"
executeTool (MessagePlayers message)    = pure $ "Message sent to players: " ++ message
```

Easy! Use this library. Take a look at the example in Examples/Planets.

Hack it to pieces and launch the MCP server like this:
```haskell
main :: IO ()
main = runMcpServerStdIn
            Planets.prompts Planets.invokePrompt Planets.executePrompt
            Planets.tools Planets.invokeTool Planets.executeTool
```

Then setup your MCP client to talk to it via stdin/stdout.

My preference has been to use docker to make this easier, and there is a Dockerfile provided in this project to help you do exactly that. Once you have the project built, you can launch it via Claude Desktop like this:

```json
{
    "mcpServers": {
        "haskell-mcp": {
            "command": "docker",
            "args": [
                "run",
                "--rm",
                "-i",
                "haskell-mcp:latest"
            ]
        }
    }
}
```

And thats it! Beautiful MCP!