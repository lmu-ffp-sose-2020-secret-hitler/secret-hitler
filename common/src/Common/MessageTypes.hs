{-# language GADTs #-}

module Common.MessageTypes where

import Common.GameMessages (GameView, GameAction)
import Data.Text (Text)
import Data.Aeson as A
import GHC.Generics (Generic)
import Data.Functor.Identity (Identity (Identity))
import Data.GADT.Compare (GEq (geq), (:~:) (Refl))
import Data.Dependent.Sum (DSum ((:=>)), (==>))

data StateFromServer =
  LobbyFromServer LobbyView |
  GameFromServer GameView
  deriving stock (Generic)
instance ToJSON StateFromServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON StateFromServer

data LobbyView =
  LobbyView {
    playerNames :: [Text]
  }
  deriving stock (Generic)
instance ToJSON LobbyView where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LobbyView

lobbyViewInitial :: LobbyView
lobbyViewInitial = LobbyView []

data ActionFromClient =
  LobbyAction LobbyAction |
  GameAction GameAction
  deriving stock (Generic)
instance ToJSON ActionFromClient where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ActionFromClient

data LobbyAction =
  Join Text |
  StartGame
  deriving stock (Show, Generic)
instance ToJSON LobbyAction where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LobbyAction

data StateFromServerTag a where
  LobbyFromServerTag :: StateFromServerTag LobbyView
  GameFromServerTag :: StateFromServerTag GameView

instance GEq StateFromServerTag where
  geq LobbyFromServerTag LobbyFromServerTag = Just Refl
  geq GameFromServerTag GameFromServerTag = Just Refl
  geq _ _ = Nothing

stateFromServerToDSum :: StateFromServer -> DSum StateFromServerTag Identity
stateFromServerToDSum =
  \case
    LobbyFromServer lobbyView -> LobbyFromServerTag ==> lobbyView
    GameFromServer gameView -> GameFromServerTag ==> gameView
    
dsumToStateFromServer :: DSum StateFromServerTag Identity -> StateFromServer
dsumToStateFromServer =
  \case
    LobbyFromServerTag :=> Identity lobbyView -> LobbyFromServer lobbyView
    GameFromServerTag :=> Identity gameView -> GameFromServer gameView
