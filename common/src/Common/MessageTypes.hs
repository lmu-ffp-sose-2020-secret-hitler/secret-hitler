{-# language GADTs #-}

module Common.MessageTypes where

import Common.GameMessages (GameAction, GameEvent, GameView)
import Data.Text (Text)
import Data.Aeson as A
import GHC.Generics (Generic)
import Data.Functor.Identity (Identity (Identity))
import Data.GADT.Compare (GEq (geq), (:~:) (Refl))
import Data.Dependent.Sum (DSum ((:=>)), (==>))

data StateFromServer =
  LobbyFromServer LobbyView |
  GameFromServer GameUpdate
  deriving stock (Generic)
instance FromJSON StateFromServer
instance ToJSON StateFromServer where

data LobbyView =
  LobbyView {
    playerNames :: [Text]
  }
  deriving stock (Generic)
instance FromJSON LobbyView
instance ToJSON LobbyView where

lobbyViewInitial :: LobbyView
lobbyViewInitial = LobbyView []

data GameUpdate = GameUpdate
  {
    gameView :: GameView,
    gameEvent :: (Maybe GameEvent)
  }
  deriving stock (Generic)
instance FromJSON GameUpdate
instance ToJSON GameUpdate where

data ActionFromClient =
  LobbyAction LobbyAction |
  GameAction GameAction
  deriving stock (Generic)
instance FromJSON ActionFromClient
instance ToJSON ActionFromClient where

data LobbyAction =
  Join Text |
  StartGame
  deriving stock (Show, Generic)
instance FromJSON LobbyAction
instance ToJSON LobbyAction where

data StateFromServerTag a where
  LobbyFromServerTag :: StateFromServerTag LobbyView
  GameFromServerTag :: StateFromServerTag GameUpdate

instance GEq StateFromServerTag where
  geq LobbyFromServerTag LobbyFromServerTag = Just Refl
  geq GameFromServerTag GameFromServerTag = Just Refl
  geq _ _ = Nothing

stateFromServerToDSum :: StateFromServer -> DSum StateFromServerTag Identity
stateFromServerToDSum =
  \case
    LobbyFromServer lobbyView -> LobbyFromServerTag ==> lobbyView
    GameFromServer gameUpdate -> GameFromServerTag ==> gameUpdate
    
dsumToStateFromServer :: DSum StateFromServerTag Identity -> StateFromServer
dsumToStateFromServer =
  \case
    LobbyFromServerTag :=> Identity lobbyView -> LobbyFromServer lobbyView
    GameFromServerTag :=> Identity gameUpdate -> GameFromServer gameUpdate
