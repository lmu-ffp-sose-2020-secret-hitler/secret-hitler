module Common.MessageTypes where

import Data.Text (Text)
import Data.Aeson as A
import GHC.Generics (Generic)

data ServerToClient =
  LobbyMessage {
    playerNames :: [Text]
  }
  deriving stock (Generic)
instance ToJSON ServerToClient where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ServerToClient

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

-- update :: ClientToServer -> IO ()
-- update data = case data of
--   LobbyToServer (Join nameNew) ->...
--   GameToServer (IncreaseLiberalPolicyCount) ->...
  
