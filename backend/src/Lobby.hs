module Lobby where

import Data.Foldable (for_)
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

lobbyMessage :: Lobby -> ServerToClient
lobbyMessage (Lobby {players}) = LobbyMessage $ fmap (view #name) $ M.elems $ players

application :: MVar Lobby -> WS.ServerApp
application lobbyMVar pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  (WS.sendTextData conn . A.encode . lobbyMessage) =<< readMVar lobbyMVar
  playerId <- modifyMVar lobbyMVar $ \lobbyOld ->
    let
      playerId =
        fromMaybe
          (PlayerId 0)
          ((succ . fst) <$> (M.lookupMax $ lobbyOld ^. #players))
      lobbyNew = lobbyOld & #players . at playerId .~ Just (Player "new player" conn)
    in
      broadcast lobbyNew -- to-do. Are we fine with doing network IO while holding the mutex?
      *>
      pure (lobbyNew, playerId)

  talk playerId conn lobbyMVar `finally` removePlayer playerId lobbyMVar

removePlayer :: PlayerId -> MVar Lobby -> IO ()
removePlayer playerId lobbyMVar =
  modifyMVar_
    lobbyMVar
    (
      \lobbyOld ->
      let lobbyNew = set (#players . at playerId) Nothing lobbyOld
      in
        broadcast lobbyNew -- to-do. Are we fine with doing network IO while holding the mutex?
        *>
        pure lobbyNew
    )

talk :: PlayerId -> WS.Connection -> MVar Lobby -> IO ()
talk playerId conn lobbyMVar = forever $
  A.decodeStrict' <$> WS.receiveData conn
  >>=
  \case
    Nothing -> pure ()
    Just (Join newName) ->
      modifyMVar_
        lobbyMVar
        (
          \lobbyOld ->
          let lobbyNew = set (#players . ix playerId . #name) newName lobbyOld
          in
            broadcast lobbyNew -- to-do. Are we fine with doing network IO while holding the mutex?
            *>
            pure lobbyNew
        )

broadcast :: Lobby -> IO ()
broadcast lobby@(Lobby {players}) =
  for_
    players
    (
      \(Player {connection}) ->
      WS.sendTextData connection $ A.encode $ lobbyMessage $ lobby
    )
