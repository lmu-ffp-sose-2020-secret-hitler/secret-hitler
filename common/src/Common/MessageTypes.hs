module Common.MessageTypes where

import Data.Text (Text)
import Data.Aeson as Aeson

data ServerToClient =
  PlayerList [Text]

data ClientToServer =
  Join Text
