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
  Join Text
