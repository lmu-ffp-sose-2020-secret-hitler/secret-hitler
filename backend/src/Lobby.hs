module Lobby where

import Control.Lens
import Data.Generics.Labels ()
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import GHC.Generics (Generic)

data Lobby = Lobby {
  players :: IntMap Player
} deriving stock (Generic)

data Player = Player {
  name :: Text
} deriving stock (Generic)

newLobby :: Lobby
newLobby = Lobby {
  players = IntMap.empty
}

removePlayer :: Int -> Lobby -> Lobby
removePlayer id lobbyOld =
  lobbyOld & #players . at id .~ Nothing
