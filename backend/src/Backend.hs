module Backend where

import Common.GameMessages (
    GameAction (..),
    GameEvent (..),
    GamePhase (..),
    GameView (..),
    PlayerView (..),
    Role (..),
    isVotePhase,
  )
import Common.MessageTypes
import Common.Route
import Control.Concurrent
import Control.Exception (SomeException, catch, finally)
import Control.Lens
import Control.Monad
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Dependent.Sum (DSum (..))
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (Identity))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import Game (Game (..))
import qualified Game
import Lobby (Lobby (..))
import qualified Lobby
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap (runWebSocketsSnap)
import Obelisk.Backend (Backend (Backend, _backend_run, _backend_routeEncoder))
import Random (runRandomIO)

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
        putStrLn $ "Establishing connection to client " ++ show id
        -- to-do. Are we fine with doing network IO while holding the mutex?
        sendLobbyToAll (stateNew ^. #connections) lobbyNew
        return (stateNew, Just (id, connection))
      _ -> do
        WS.rejectRequest pending "Game is already in progress"
        return (stateOld, Nothing)
  case result of
    Nothing -> return ()
    Just (id, connection) ->
      talk id connection stateMVar
      `catch` (putStrLn . show :: SomeException -> IO ())
      `finally` (removeClient id stateMVar)

nextId :: IntMap b -> Int
nextId map = fromMaybe 0 (succ <$> fst <$> IntMap.lookupMax map)

removeClient :: Int -> MVar ServerState -> IO ()
removeClient id stateMVar = do
  putStrLn $ "Client " ++ show id ++ " disconnected"
  modifyMVar_ stateMVar $ \stateOld -> do
    let stateNew = stateOld & #connections . at id .~ Nothing
    -- to-do. Are we fine with doing network IO while holding the mutex?
    removeClientFromLobby id stateNew

removeClientFromLobby :: Int -> ServerState -> IO ServerState
removeClientFromLobby id state@(ServerState {
  connections,
  gameState = LobbyState lobbyOld
}) = do
  let lobbyNew = Lobby.removePlayer id lobbyOld
  sendLobbyToAll connections lobbyNew
  return $ state & #gameState .~ LobbyState lobbyNew
removeClientFromLobby _id state = return state

talk :: Int -> WS.Connection -> MVar ServerState -> IO ()
talk id connection stateMVar = do
  putStrLn $ "Client " ++ show id ++ " connected"
  forever $ do
    messageMaybe <- Aeson.decodeStrict' <$> WS.receiveData connection
    case messageMaybe of
      Nothing -> putStrLn $ "Could not decode message from client " ++ show id
      Just message -> do
        Text.putStrLn $ "Received message from client " <> Text.pack (show id) <> ": "
          <> (encodeAsText message)
        case message of
          LobbyAction payload -> modifyMVar_ stateMVar (answerLobbyToServer id payload)
          GameAction payload -> modifyMVar_ stateMVar (answerGameToServer id payload)

sendToAll :: (Foldable f, Aeson.ToJSON msg) => f WS.Connection -> msg -> IO ()
sendToAll connections message = do
  Text.putStrLn $ "Sending message to all clients: " <> (encodeAsText message)
  traverse_ (sendMessage message) connections

sendToAllWithKey :: Aeson.ToJSON msg => IntMap WS.Connection -> (Int -> msg) -> IO ()
sendToAllWithKey connections createMessage = do
  case fst <$> IntMap.lookupMin connections of
    Nothing -> return ()
    Just minId -> Text.putStrLn $
      "Sending different messages to all clients, message for first client: "
      <> (encodeAsText $ createMessage minId)
  void $ IntMap.traverseWithKey (sendMessage . createMessage) connections

sendMessage :: Aeson.ToJSON msg => msg -> WS.Connection -> IO ()
sendMessage message connection = WS.sendTextData connection $ Aeson.encode message

encodeAsText :: Aeson.ToJSON msg => msg -> Text.Text
encodeAsText message = decodeUtf8 $ toStrict $ Aeson.encode message

----------------------------------------------------------------------------------------------------
--    _            _      _
--   | |     ___  | |__  | |__   _   _
--   | |    / _ \ | '_ \ | '_ \ | | | |
--   | |___| (_) || |_) || |_) || |_| |
--   |_____|\___/ |_.__/ |_.__/  \__, |
--                               |___/
----------------------------------------------------------------------------------------------------

answerLobbyToServer :: Int -> LobbyAction -> ServerState -> IO (ServerState)
answerLobbyToServer id payload stateOld@ServerState {
  connections,
  gameState = LobbyState lobbyOld
} = do
  stateNew <- case payload of
    StartGame -> do
      let playerNames = lobbyOld ^. #players <&> view #name
      game <- runRandomIO $ Game.generateRandomGame playerNames
      sendToAllWithKey connections (gameMessage game Nothing)
      return $ stateOld & #gameState .~ GameState game
    Join nameNew -> do
      let lobbyNew = lobbyOld & #players . ix id . #name .~ nameNew
      -- to-do. Are we fine with doing network IO while holding the mutex?
      sendLobbyToAll connections lobbyNew
      return $ stateOld & #gameState .~ LobbyState lobbyNew
  return stateNew
answerLobbyToServer _id _payload stateOld = do
  putStrLn "There is currently no active Lobby"
  return stateOld

sendLobbyToAll :: IntMap WS.Connection -> Lobby -> IO ()
sendLobbyToAll connections lobby =
  sendToAll connections (lobbyMessage lobby)

lobbyMessage :: Lobby -> StateFromServer
lobbyMessage lobby = LobbyFromServer $ lobbyView lobby

lobbyView :: Lobby -> LobbyView
lobbyView (Lobby {players}) = LobbyView {
  playerNames = fmap (view #name) $ IntMap.elems $ players
}

----------------------------------------------------------------------------------------------------
--     ____
--    / ___|  __ _  _ __ ___    ___
--   | |  _  / _` || '_ ` _ \  / _ \
--   | |_| || (_| || | | | | ||  __/
--    \____| \__,_||_| |_| |_| \___|
--
----------------------------------------------------------------------------------------------------

answerGameToServer :: Int -> GameAction -> ServerState -> IO (ServerState)
answerGameToServer id payload stateOld@ServerState {
  connections,
  gameState = GameState gameOld
} = do
  (gameNew, gameEvent) <- runRandomIO $ Game.updateChecked id payload gameOld
  let stateNew = stateOld & #gameState .~ GameState gameNew
  -- to-do. Are we fine with doing network IO while holding the mutex?
  sendToAllWithKey connections (gameMessage gameNew (Just gameEvent))
  return stateNew
answerGameToServer _id _payload stateOld = do
  putStrLn "There is currently no Game in progress"
  return stateOld

gameMessage :: Game -> Maybe GameEvent -> Int -> StateFromServer
gameMessage game event playerId =
  GameFromServer $
  GameUpdate (createGameView game playerId) (event)

createGameView :: Game -> Int -> GameView
createGameView game@(Game {
  phase,
  goodPolicyCount,
  evilPolicyCount,
  presidentId,
  electionTracker
}) playerId =
  let
    players = game ^. #players
    player = fromJust $ game ^. #players . at playerId
    playerRole = player ^. #role
    playerCount = IntMap.size $ players
    gamePhase = game ^. #phase
    currentHand = Game.currentHand game
    drawPileSize = length (Game.drawPile game)
  in
  GameView {
    playerId,
    playerRole,
    players = IntMap.map (playerView playerRole playerCount gamePhase) players,
    phase,
    currentHand = filter (const $ Game.isPlayerAllowedToAct playerId game) currentHand,
    drawPileSize,
    discardPileSize = 6+11 - drawPileSize - (length currentHand) - goodPolicyCount - evilPolicyCount,
    goodPolicyCount,
    evilPolicyCount,
    presidentId,
    electionTracker
  }

playerView :: Role -> Int -> GamePhase -> Game.Player -> PlayerView
playerView playerRole playerCount gamePhase (Game.Player {
  name,
  turnOrder,
  role,
  vote,
  alive
}) =
  PlayerView {
    name,
    turnOrder,
    role = mfilter (const $ canSeeOtherRoles playerRole playerCount) (Just role),
    vote = mfilter (const $ isVotePhase gamePhase) vote,
    alive
  }
  where
    canSeeOtherRoles :: Role -> Int -> Bool
    canSeeOtherRoles GoodRole _playerCount = False
    canSeeOtherRoles EvilRole _playerCount = True
    canSeeOtherRoles EvilLeaderRole playerCount = playerCount <= 5
