module Lobby where

import qualified Network.WebSockets as WS
import Control.Lens
import Control.Concurrent
import qualified Data.Aeson as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Exception
import Control.Monad
import Common.MessageTypes
import GHC.Generics (Generic)
import Data.Generics.Labels ()

newtype PlayerId = PlayerId Int
  deriving newtype (Eq, Ord)

data Player = Player {
  name :: Text,
  connection :: WS.Connection
} deriving stock (Generic)

data Lobby = Lobby {
  players :: Map PlayerId Player
} deriving stock (Generic)

newLobby :: Lobby
newLobby = Lobby {
  players = M.empty
}

lobbyMessage :: Lobby -> ServerToClient
lobbyMessage (Lobby {players}) = LobbyMessage $ fmap (view #name) $ M.elems $ players

application :: MVar Lobby -> WS.ServerApp
application lobbyMVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  (WS.sendTextData conn . A.encode . lobbyMessage) =<< readMVar lobbyMVar
  playerId <- modifyMVar lobbyMVar $ \lobby ->
    let
      playerId = fromMaybe (PlayerId 0) (fst <$> (M.lookupMax $ lobby ^. #players))
      player = (Player "" conn)
    in pure (lobby & #players . at playerId .~ Just player, playerId)

  talk playerId conn `finally` removePlayer playerId lobbyMVar

removePlayer :: PlayerId -> MVar Lobby -> IO ()
removePlayer playerId lobbyMVar =
  modifyMVar_
    lobbyMVar
    (pure . set (#players . at playerId) Nothing)

talk :: PlayerId -> WS.Connection -> IO ()
talk _playerId conn = forever $ do
  WS.receiveData conn :: IO Text
