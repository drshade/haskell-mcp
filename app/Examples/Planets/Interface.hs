module Examples.Planets.Interface where
import           Data.Map  (fromList)
import           MCP.Types (DocTable)

data Prompt = RobotStyle { feel     :: String  }

data Tool
    = GetAllGames
    | GetGameStatus  { gameid :: String  }
    | MessagePlayers { message :: String }

data Resource
    = Players
    | ShipTypes
    | PlayerInfo { playerid :: String }

promptDoc :: DocTable
promptDoc = fromList
  -- constructor docs
  [ ( "RobotStyle" , "Suggestions on how to speak like a ROBOT" )

  -- field-level docs
  , ( "feel"       , "Desired mood / feel (optional)" )
  ]

toolDoc :: DocTable
toolDoc = fromList
  -- constructor docs
  [ ( "GetAllGames",        "Get a list of all games available" )
  , ( "GetGameStatus" ,     "Get the status of a single game"    )
  , ( "MessagePlayers" ,    "Send a message to all players"    )

  -- field-level docs
  , ( "gameid",             "ID of the game to query" )
  , ( "message",            "Message to send to players" )
  ]

resourceDoc :: DocTable
resourceDoc = fromList
  -- constructor docs
  [ ( "Players" , "List of all players in the game" )
  , ( "ShipTypes", "List of all ship types available" )

  -- field-level docs
  ]
