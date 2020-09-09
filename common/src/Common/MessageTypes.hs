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

data ServerToClient =
  LobbyToClient LobbyToClient |
  GameToClient GameToClient
  deriving stock (Generic)
instance ToJSON ServerToClient where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ServerToClient

data LobbyToClient =
  LobbyMessage {
    playerNames :: [Text]
  }
  deriving stock (Generic)
instance ToJSON LobbyToClient where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LobbyToClient

data GameToClient =
  GameMessage GameView
  deriving stock (Generic)
instance ToJSON GameToClient where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GameToClient

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
