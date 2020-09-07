module Lobby where

import qualified Network.WebSockets as WS
import Control.Lens
import Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B.L
import qualified Data.Aeson as Aeson
-- import qualified Data.Text.IO as T.IO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Exception
import Control.Monad
import Common.MessageTypes
import GHC.Generics (Generic)

newtype PlayerId = PlayerId Int
  deriving newtype (Eq, Ord)

data Player = Player {
  name :: Text,
  connection :: WS.Connection
} deriving stock (Generic)

data Lobby = Lobby {
  players :: Map PlayerId Player
} deriving stock ()

newLobby :: Lobby
newLobby = Lobby {
  players = Map.empty
}

application :: MVar Lobby -> WS.ServerApp
application lobbyMVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  Lobby {players} <- readMVar lobbyMVar
  WS.sendTextData conn $
    B.L.toStrict $
    Aeson.encode $
    PlayerList $
    fmap (view #name) $
    Map.elems $
    players
  playerId <- modifyMVar lobbyMVar $ \Lobby {players} -> do
    print (length players + 1)
    let playerId = fromMaybe (PlayerId 0) (fst <$> (Map.lookupMax players))
    let player = (Player "" conn)
    return (Lobby {players = Map.insert playerId player players}, playerId)

  talk playerId conn `finally` removePlayer playerId lobbyMVar

removePlayer :: PlayerId -> MVar Lobby -> IO ()
removePlayer playerId lobbyMVar =
  modifyMVar_ lobbyMVar $ \Lobby {players} ->
    return $ Lobby {players = Map.delete playerId players}

talk :: PlayerId -> WS.Connection -> IO ()
talk _playerId conn = forever $ do
  WS.receiveData conn :: IO Text
