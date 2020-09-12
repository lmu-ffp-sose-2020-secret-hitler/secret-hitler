module Common.GameMessages where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.IntMap.Strict
import GHC.Generics (Generic)

data GameView = GameView {
  -- The ID of the player that views this game
  playerId :: Int,
  playerRole :: Role,
  players :: IntMap PlayerView,
  phase :: GamePhase,
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

data Alignment =
  Good |
  Evil
  deriving stock (Generic)
instance FromJSON Alignment
instance ToJSON Alignment

alignment :: Role -> Alignment
alignment GoodRole = Good
alignment _ = Evil

data GamePhase =
  NominateChancellorPhase {
    previousGovernment :: Maybe Government
  } |
  VotePhase {
    previousGovernment :: Maybe Government,
    chancellorCandidateId :: Int
  } |
  PresidentDiscardPolicyPhase {
    chancellorId :: Int
  } |
  ChancellorDiscardPolicyPhase {
    chancellorId :: Int
  } |
  PolicyPeekPhase {
    chancellorId :: Int
  } |
  ExecutionPhase {
    chancellorId :: Int
  } |
  PendingVetoPhase {
    chancellorId :: Int
  }
  deriving stock (Generic)
instance FromJSON GamePhase
instance ToJSON GamePhase

isVotePhase :: GamePhase -> Bool
isVotePhase VotePhase {} = True
isVotePhase _ = False

data Government = Government {
  presidentId :: Int,
  chancellorId :: Int
} deriving stock (Generic)
instance FromJSON Government
instance ToJSON Government

data Policy =
  GoodPolicy |
  EvilPolicy
  deriving stock (Generic)
instance FromJSON Policy
instance ToJSON Policy

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
