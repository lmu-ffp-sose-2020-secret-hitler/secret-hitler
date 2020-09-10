{-# language GADTs #-}

module Common.MessageTypes where

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

data GameView = GameView {
  policyLiberalCount :: Int
} deriving stock (Generic)
instance ToJSON GameView where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GameView

lobbyViewInitial :: LobbyView
lobbyViewInitial = LobbyView []

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
