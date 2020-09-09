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
instance ToJSON StateFromServer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON StateFromServer

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

data InputFromClient =
  LobbyInput LobbyInput |
  GameInput GameInput
  deriving stock (Show, Generic)
instance ToJSON InputFromClient where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON InputFromClient

data LobbyInput =
  Join Text
  deriving stock (Show, Generic)
instance ToJSON LobbyInput where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LobbyInput

data GameInput =
  IncreaseLiberalPolicyCount
  deriving stock (Show, Generic)
instance ToJSON GameInput where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GameInput
