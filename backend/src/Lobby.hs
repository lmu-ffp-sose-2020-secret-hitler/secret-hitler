module Lobby where

import Common.GameMessages (PlayerId)
import Control.Lens
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

data Lobby = Lobby {
  players :: Map PlayerId Player
} deriving stock (Generic)

data Player = Player {
  name :: Text
} deriving stock (Generic)

newLobby :: Lobby
newLobby = Lobby {
  players = Map.empty
}

removePlayer :: PlayerId -> Lobby -> Lobby
removePlayer id lobbyOld =
  lobbyOld & #players . at id .~ Nothing
