module Lobby where

import qualified Network.WebSockets as WS
import Control.Concurrent
-- import qualified Data.ByteString as B
-- import qualified Data.Text.IO as T.IO
import Data.Map.Strict

newtype PlayerId = PlayerId Int

data Player = Player {
  name :: Text,
  connection :: WS.Connection
}

data Lobby = Lobby {
  players :: Map PlayerId Player
} deriving stock ()

newLobby :: Lobby
newLobby = Lobby {players = []}

application :: MVar Lobby -> WS.ServerApp
application lobbyMVar pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    modifyMVar_ lobbyMVar (\Lobby {players} -> do
      print (length players + 1)
      pure $ Lobby (conn : players))
    -- msgbs <- WS.receiveData conn :: IO B.ByteString
    -- let msgC = decode $ WS.toLazyByteString msgbs :: Maybe C2S
    --     -- msg = case msgC of
    --     --     Just (C2Sjoin txt) -> txt
    --     --     Just C2Sclose      -> "close msg"
    --     --     Just (C2Smsg txt)  -> txt
    --     --     Nothing            -> "hmm nothing"
    -- -- T.putStrLn $ "msg = " <> msg
    -- clients <- readMVar state
    -- case msgC of
    --     Nothing           ->
    --         T.putStrLn "Decoded msgC is nothing..."
    --     Just (C2Smsg txt) ->
    --         T.putStrLn $ "C2Smsg should not happen here, txt =" <> txt
    --     Just C2Sclose     ->
    --         T.putStrLn "C2Sclose should never happen here..."
    --     Just (C2Sjoin nm) ->
    --       case nm of
    --           _ | any ($ nm) [T.null, T.any isPunctuation, T.any isSpace] -> do
    --                     T.putStrLn $ ":" <> nm <> ":"
    --                     WS.sendTextData conn $ (toStrict . encode) S2Cnameproblem
    --             | clientExists client clients ->
    --                 WS.sendTextData conn $ (toStrict . encode) S2Cuserexists
    --             | otherwise -> flip finally disconnect $ do
    --                modifyMVar_ state $ \s -> do
    --                    let s' = addClient client s
    --                    WS.sendTextData conn $ (toStrict . encode . S2Cwelcome) $
    --                        T.intercalate ", " (map fst s)
    --                    broadcast (nm <> " joined") s'
    --                    return s'
    --                talk conn state client
    --       where
    --         client     = (nm,conn)
    --         disconnect = do
    --             -- Remove client and return new state
    --             s <- modifyMVar state $ \s ->
    --                 let s' = removeClient client s in return (s', s')
    --             broadcast (fst client <> " disconnected") s
