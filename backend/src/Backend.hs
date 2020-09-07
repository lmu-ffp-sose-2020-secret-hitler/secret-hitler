module Backend where

import Common.Route
import Control.Concurrent
import Obelisk.Backend (Backend (Backend, _backend_run, _backend_routeEncoder))
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity (Identity (Identity))
import Game ()
import Network.WebSockets.Snap (runWebSocketsSnap)
import qualified Lobby

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  {
    _backend_run =
      \serve -> do
        lobby <- newMVar (Lobby.newLobby)
        serve $ \case
          BackendRoute_Missing :=> Identity () -> return ()
          BackendRoute_Main :=> Identity () -> do
            runWebSocketsSnap (Lobby.application lobby)
    ,
    _backend_routeEncoder = fullRouteEncoder
  }
