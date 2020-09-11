module Common.GameMessages where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.IntMap.Strict
import GHC.Generics (Generic)

data GameView = GameView {
  -- phase :: GamePhase,
  players :: IntMap PlayerView,
  -- The cardPile contains the drawPile and the currentHand
  currentHand :: [Policy],
  goodPolicyCount :: Int,
  evilPolicyCount :: Int,
  presidentId :: Int,
  electionTracker :: Int
} deriving stock (Generic)
instance FromJSON GameView
instance ToJSON GameView

data PlayerView = PlayerView {
  name :: Text,
  turnOrder :: Int,
  role :: Maybe Role,
  vote :: Maybe Bool,
  alive :: Bool
} deriving stock (Generic)
instance FromJSON PlayerView
instance ToJSON PlayerView

data Role =
  GoodRole |
  EvilRole |
  EvilLeaderRole
  deriving stock (Generic)
instance FromJSON Role
instance ToJSON Role

data Policy =
  GoodPolicy |
  EvilPolicy
  deriving stock (Generic)
instance FromJSON Policy
instance ToJSON Policy

data PlayerAction =
  GameAction Int GameAction
  deriving stock (Generic)
instance FromJSON PlayerAction
instance ToJSON PlayerAction

data GameAction =
  NominateChancellor Int |
  PlaceVote Bool |
  PresidentDiscardPolicy Int |
  ChancellorDiscardPolicy Int |
  StopPeekingPolicies |
  ExecutePlayer Int |
  ProposeVeto |
  AcceptVeto |
  RejectVeto
  deriving stock (Generic)
instance FromJSON GameAction
instance ToJSON GameAction

data GameEvent =
  ChancellorNominated |
  VotePlaced |
  VoteSucceeded |
  VoteFailed |
  PresidentDiscardedPolicy |
  ChancellorDiscardedPolicy |
  PlayerKilled |
  VetoProposed |
  VetoAccepted |
  VetoRejected |
  Error Text
  deriving stock (Generic)
instance FromJSON GameEvent
instance ToJSON GameEvent
