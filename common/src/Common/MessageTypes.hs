module Common.MessageTypes where

import Data.Text (Text)
import Data.Aeson as A
import GHC.Generics (Generic)

data GameView = GameView {
  policyLiberalCount :: Int
} deriving stock (Generic)
instance ToJSON GameView where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GameView

data StateFromServer =
  LobbyFromServer LobbyFromServer |
  GameFromServer GameFromServer
  deriving stock (Generic)
instance ToJSON ServerFromServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ServerFromServer

data LobbyFromServer =
  LobbyMessage {
    playerNames :: [Text]
  }
  deriving stock (Generic)
instance ToJSON LobbyFromServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LobbyFromServer

lobbyFromServerInitial :: LobbyFromServer
lobbyFromServerInitial = LobbyMessage []

data GameFromServer =
  GameMessage GameView
  deriving stock (Generic)
instance ToJSON GameFromServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GameFromServer

data ClientToServer =
  LobbyToServer LobbyToServer |
  GameToServer GameToServer
  deriving stock (Show, Generic)
instance ToJSON ClientToServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ClientToServer

data LobbyToServer =
  Join Text
  deriving stock (Show, Generic)
instance ToJSON LobbyToServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LobbyToServer

data GameToServer =
  IncreaseLiberalPolicyCount
  deriving stock (Show, Generic)
instance ToJSON GameToServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GameToServer
