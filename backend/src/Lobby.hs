module Lobby where

import Common.MessageTypes
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.Aeson as A
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS

newtype PlayerId = PlayerId Int
  deriving newtype (Eq, Ord, Enum)

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

application :: MVar Lobby -> WS.ServerApp
application lobbyMVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  sendLobby conn =<< readMVar lobbyMVar
  playerId <- modifyMVar lobbyMVar $ \lobbyOld -> do
    let playerId = nextPlayerId (lobbyOld ^. #players)
        lobbyNew = lobbyOld & #players . at playerId .~ Just (Player "new player" conn)
    broadcast lobbyNew -- to-do. Are we fine with doing network IO while holding the mutex?
    pure (lobbyNew, playerId)

  talk playerId conn lobbyMVar `finally` removePlayer playerId lobbyMVar

nextPlayerId :: Map PlayerId b -> PlayerId
nextPlayerId players =
  fromMaybe (PlayerId 0) (succ <$> fst <$> M.lookupMax players)

removePlayer :: PlayerId -> MVar Lobby -> IO ()
removePlayer playerId lobbyMVar =
  modifyMVar_ lobbyMVar $ \lobbyOld -> do
    let lobbyNew = set (#players . at playerId) Nothing lobbyOld
    broadcast lobbyNew -- to-do. Are we fine with doing network IO while holding the mutex?
    pure lobbyNew

talk :: PlayerId -> WS.Connection -> MVar Lobby -> IO ()
talk playerId conn lobbyMVar = forever $ do
  message <- A.decodeStrict' <$> WS.receiveData conn
  case message of
    Nothing -> pure ()
    Just (Join newName) ->
      modifyMVar_ lobbyMVar $ \lobbyOld -> do
        let lobbyNew = set (#players . ix playerId . #name) newName lobbyOld
        broadcast lobbyNew -- to-do. Are we fine with doing network IO while holding the mutex?
        pure lobbyNew

broadcast :: Lobby -> IO ()
broadcast lobby@(Lobby {players}) =
  for_ players $ \Player {connection} ->
    sendLobby connection lobby

sendLobby :: WS.Connection -> Lobby -> IO ()
sendLobby connection = WS.sendTextData connection . A.encode . lobbyMessage

lobbyMessage :: Lobby -> ServerToClient
lobbyMessage (Lobby {players}) = LobbyMessage $ fmap (view #name) $ M.elems $ players
