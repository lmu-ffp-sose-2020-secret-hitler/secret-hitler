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
            lobbyNew = joinLobby lobbyOld id
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
        modifyMVar_ stateMVar $ (case message of
          LobbyAction payload -> answerLobbyToServer payload
          GameAction payload -> answerGameToServer payload
          ReturnToLobbyAction -> returnToLobby
          ) id

sendToAll :: (Foldable f, Aeson.ToJSON msg) => f WS.Connection -> msg -> IO ()
sendToAll connections message = do
  Text.putStrLn $ "Sending message to all clients: " <> (encodeAsText message)
  traverse_ (sendMessage message) connections

sendToAllWithKeyMaybe :: Aeson.ToJSON msg => IntMap WS.Connection -> (Int -> Maybe msg) -> IO ()
sendToAllWithKeyMaybe connections createMessageMaybe =
  void $ flip IntMap.traverseWithKey connections $ \id connection ->
    case createMessageMaybe id of
      Nothing -> return ()
      Just message -> do
        Text.putStrLn $ "Sending message to client " <> Text.pack (show id) <> ": "
          <> encodeAsText message
        sendMessage message connection

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

answerLobbyToServer :: LobbyAction -> Int -> ServerState -> IO (ServerState)
answerLobbyToServer payload id stateOld@ServerState {
  connections,
  gameState = LobbyState lobbyOld
} = do
  stateNew <- case payload of
    StartGame -> do
      let playerNames = lobbyOld ^. #players <&> view #name
      game <- runRandomIO $ Game.generateRandomGame playerNames
      -- to-do. Are we fine with doing network IO while holding the mutex?
      sendGameUpdateToAll connections game Nothing
      return $ stateOld & #gameState .~ GameState game
    Join nameNew -> do
      let lobbyNew = lobbyOld & #players . ix id . #name .~ nameNew
      -- to-do. Are we fine with doing network IO while holding the mutex?
      sendLobbyToAll connections lobbyNew
      return $ stateOld & #gameState .~ LobbyState lobbyNew
  return stateNew
answerLobbyToServer _payload _id stateOld = do
  putStrLn "There is currently no active Lobby"
  return stateOld

returnToLobby :: Int -> ServerState -> IO ServerState
returnToLobby id stateOld@(ServerState {
  connections,
  gameState = LobbyState lobbyOld
}) = do
  let lobbyNew = joinLobby lobbyOld id
  sendLobbyToAll connections lobbyNew
  let stateNew = stateOld & #gameState .~ LobbyState lobbyNew
  return stateNew
returnToLobby _id stateOld = do
  putStrLn "There is currently no active Lobby"
  return stateOld

joinLobby :: Lobby -> Int -> Lobby
joinLobby lobby id =
  lobby & #players . at id .~ Just Lobby.Player {name = "new player"}

sendLobbyToAll :: IntMap WS.Connection -> Lobby -> IO ()
sendLobbyToAll connections lobby@(Lobby { players }) =
  sendToAll (IntMap.intersection connections players) (lobbyMessage lobby)

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

answerGameToServer :: GameAction -> Int -> ServerState -> IO (ServerState)
answerGameToServer payload id stateOld@ServerState {
  connections,
  gameState = GameState gameOld
} = do
  (gameNew, gameEvent) <- runRandomIO $ Game.updateChecked id payload gameOld
  -- to-do. Are we fine with doing network IO while holding the mutex?
  sendGameUpdateToAll connections gameNew (Just gameEvent)
  case gameNew ^. #phase of
    GameOverPhase {} -> return $ stateOld & #gameState .~ LobbyState Lobby.newLobby
    _ -> return $ stateOld & #gameState .~ GameState gameNew
answerGameToServer _payload _id stateOld = do
  putStrLn "There is currently no Game in progress"
  return stateOld

sendGameUpdateToAll :: IntMap WS.Connection -> Game -> Maybe GameEvent -> IO ()
sendGameUpdateToAll connections game@(Game {
  players
}) gameEvent =
  sendToAllWithKeyMaybe (IntMap.intersection connections players) (gameMessage game gameEvent)

gameMessage :: Game -> Maybe GameEvent -> Int -> Maybe StateFromServer
gameMessage game event receiverId
  | Just VotePlaced { playerId } <- event, receiverId /= playerId =
    Nothing
  | otherwise =
    Just $
    GameFromServer $
    GameUpdate (createGameView game receiverId) event

createGameView :: Game -> Int -> GameView
createGameView game@(Game {
  phase,
  goodPolicyCount,
  evilPolicyCount,
  presidentId,
  electionTracker
}) viewerId =
  let
    viewer = fromJust $ game ^. #players . at viewerId
    viewerRole = viewer ^. #role
    currentHand = Game.currentHand game
    drawPileSize = length (Game.drawPile game)
  in
  GameView {
    playerId = viewerId,
    playerRole = viewerRole,
    players = IntMap.mapWithKey (playerView viewerRole game) (game ^. #players),
    phase,
    currentHand = filter (const $ Game.isPlayerAllowedToAct viewerId game) currentHand,
    drawPileSize,
    discardPileSize = 6+11 - drawPileSize - (length currentHand) - goodPolicyCount - evilPolicyCount,
    goodPolicyCount,
    evilPolicyCount,
    presidentId,
    electionTracker,
    vetoUnlocked = evilPolicyCount >= 5
  }

playerView :: Role -> Game -> Int -> Game.Player -> PlayerView
playerView viewerRole game id (Game.Player {
  name,
  turnOrder,
  role,
  vote,
  alive
}) =
  let playerCount = IntMap.size $ game ^. #players in
  PlayerView {
    name,
    turnOrder,
    role = mfilter (const $ canSeeOtherRoles viewerRole playerCount) (Just role),
    vote = mfilter (const $ not $ isVotePhase (game ^. #phase)) vote,
    alive,
    eligible = Game.isEligible id game
  }
  where
    canSeeOtherRoles :: Role -> Int -> Bool
    canSeeOtherRoles GoodRole _playerCount = False
    canSeeOtherRoles EvilRole _playerCount = True
    canSeeOtherRoles EvilLeaderRole playerCount = playerCount <= 5
