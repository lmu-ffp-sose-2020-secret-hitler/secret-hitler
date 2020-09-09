module Backend where

import Common.MessageTypes
import Common.Route
import Control.Concurrent
import Control.Exception (finally)
import Control.Lens
import Control.Monad
import Data.Foldable (for_)
import qualified Data.Aeson as Aeson
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity (Identity (Identity))
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as IntMap
import Game
import GHC.Generics (Generic)
import Lobby
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap (runWebSocketsSnap)
import Obelisk.Backend (Backend (Backend, _backend_run, _backend_routeEncoder))

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  {
    _backend_run =
      \serve -> do
        stateMVar <- newMVar newServerState
        serve $ \case
          BackendRoute_Missing :=> Identity () -> return ()
          BackendRoute_Main :=> Identity () -> do
            runWebSocketsSnap (application stateMVar)
    ,
    _backend_routeEncoder = fullRouteEncoder
  }

data ServerState = ServerState {
  connections :: IntMap WS.Connection,
  gameState :: GameState
} deriving stock (Generic)

data GameState =
  LobbyState Lobby |
  GameState Game

newServerState :: ServerState
newServerState = ServerState {
  connections = IntMap.empty,
  gameState = LobbyState Lobby.newLobby
}

application :: MVar ServerState -> WS.ServerApp
application stateMVar pending = do
  result <- modifyMVar stateMVar $ \stateOld@ServerState {connections, gameState} -> do
    case gameState of
      LobbyState lobbyOld -> do
        connection <- WS.acceptRequest pending
        WS.forkPingThread connection 30
        let id = nextId connections
            player = Lobby.Player {name = "new player"}
            lobbyNew = lobbyOld & (#players . at id) .~ Just player
            stateNew = stateOld
              & #connections . at id .~ Just connection
              & #gameState .~ LobbyState lobbyNew
        broadcast stateNew -- to-do. Are we fine with doing network IO while holding the mutex?
        return (stateNew, Just (id, connection))
      _ -> do
        WS.rejectRequest pending "Game is already in progress"
        return (stateOld, Nothing)
  case result of
    Nothing -> return ()
    Just (id, connection) -> talk id connection stateMVar `finally` removeClient id stateMVar

removeClient :: Int -> MVar ServerState -> IO ()
removeClient id stateMVar = do
  putStrLn $ "Client " ++ show id ++ " disconnected"
  modifyMVar_ stateMVar $ \stateOld -> do
    return $ stateOld
      & #connections . at id .~ Nothing
      & #gameState %~ removeClientFromLobby id

removeClientFromLobby :: Int -> GameState -> GameState
removeClientFromLobby id (LobbyState lobby) =
  LobbyState $ Lobby.removePlayer id lobby
removeClientFromLobby _id gameState = gameState

nextId :: IntMap b -> Int
nextId map =
  fromMaybe 0 (succ <$> fst <$> IntMap.lookupMax map)

broadcast :: ServerState -> IO ()
broadcast (ServerState {connections, gameState = LobbyState lobby}) =
  for_ connections $ \connection ->
    sendLobby lobby connection

sendState :: GameState -> WS.Connection -> IO ()
sendState state =
  case state of
    LobbyState lobby -> sendLobby lobby

sendLobby :: Lobby -> WS.Connection -> IO ()
sendLobby lobby connection = WS.sendTextData connection $ Aeson.encode $ lobbyMessage $ lobby

lobbyMessage :: Lobby -> ServerToClient
lobbyMessage (Lobby {players}) = LobbyMessage $ fmap (view #name) $ IntMap.elems $ players

talk :: Int -> WS.Connection -> MVar ServerState -> IO ()
talk id connection stateMVar = do
  forever $ do
    messageMaybe <- Aeson.decodeStrict' <$> WS.receiveData connection
    case messageMaybe of
      Nothing -> putStrLn $ "Could not decode message from client " ++ show id
      Just message -> do
        putStrLn $ "Received message from client " ++ show id ++ ": " ++ show message
        case message of
          LobbyToServer payload -> modifyMVar_ stateMVar (answerLobbyToServer id payload)
          GameToServer payload -> modifyMVar_ stateMVar (answerGameToServer id payload)

answerLobbyToServer :: Int -> LobbyToServer -> ServerState -> IO (ServerState)
answerLobbyToServer id payload stateOld@ServerState {gameState=LobbyState lobbyOld} = do
  let lobbyNew = updateLobby id payload lobbyOld
      stateNew = stateOld & #gameState .~ LobbyState lobbyNew
  broadcast stateNew -- to-do. Are we fine with doing network IO while holding the mutex?
  return stateNew
answerLobbyToServer _id _payload stateOld = return stateOld

updateLobby :: Int -> LobbyToServer -> Lobby -> Lobby
updateLobby id (Join nameNew) = #players . ix id . #name .~ nameNew

answerGameToServer :: Int -> GameToServer -> ServerState -> IO (ServerState)
answerGameToServer id payload stateOld@ServerState {gameState=GameState gameOld} = do
  let gameNew = updateGame id payload gameOld
      stateNew = stateOld & #gameState .~ GameState gameNew
  broadcast stateNew -- to-do. Are we fine with doing network IO while holding the mutex?
  return stateNew
answerGameToServer _id _payload stateOld = return stateOld

updateGame :: Int -> GameToServer -> Game -> Game
updateGame _id IncreaseLiberalPolicyCount = #goodPolicies %~ (+1)
