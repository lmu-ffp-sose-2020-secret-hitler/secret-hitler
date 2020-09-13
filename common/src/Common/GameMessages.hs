{-# OPTIONS_GHC -Wno-partial-fields #-}

module Common.GameMessages where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey, FromJSONKey)
import Data.Text (Text)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

newtype PlayerId =
  PlayerId Int
  deriving stock (Generic)
  deriving newtype (
    Enum,
    Eq,
    FromJSONKey,
    Ord,
    Show,
    ToJSONKey
  )
instance FromJSON PlayerId
instance ToJSON PlayerId

data GameView = GameView {
  -- The ID of the player that views this game
  playerId :: PlayerId,
  -- The Role of the player that views this game
  playerRole :: Role,
  players :: Map PlayerId PlayerView,
  phase :: GamePhase,
  -- The current hand is empty unless the player is president or chancellor in the correct phase
  currentHand :: [Policy],
  drawPileSize :: Int,
  discardPileSize :: Int,
  goodPolicyCount :: Int,
  evilPolicyCount :: Int,
  presidentId :: PlayerId,
  electionTracker :: Int,
  vetoUnlocked :: Bool
} deriving stock (Generic)
instance FromJSON GameView
instance ToJSON GameView

data PlayerView = PlayerView {
  name :: Text,
  turnOrder :: Int,
  role :: Maybe Role,
  vote :: Maybe Bool,
  alive :: Bool,
  eligible :: Bool
} deriving stock (Generic)
instance FromJSON PlayerView
instance ToJSON PlayerView

data Role =
  GoodRole |
  EvilRole |
  EvilLeaderRole
  deriving stock (Generic, Eq)
instance FromJSON Role
instance ToJSON Role

roleAlignment :: Role -> Alignment
roleAlignment GoodRole = Good
roleAlignment _ = Evil

data Alignment =
  Good |
  Evil
  deriving stock (Generic)
instance FromJSON Alignment
instance ToJSON Alignment

data GamePhase =
  NominateChancellorPhase {
    previousGovernment :: Maybe Government
  } |
  VotePhase {
    previousGovernment :: Maybe Government,
    chancellorCandidateId :: PlayerId
  } |
  PresidentDiscardPolicyPhase {
    chancellorId :: PlayerId
  } |
  ChancellorDiscardPolicyPhase {
    chancellorId :: PlayerId
  } |
  PolicyPeekPhase {
    chancellorId :: PlayerId
  } |
  ExecutionPhase {
    chancellorId :: PlayerId
  } |
  PendingVetoPhase {
    chancellorId :: PlayerId
  } |
  GameOverPhase {
    reason :: GameOverReason
  }
  deriving stock (Generic)
instance FromJSON GamePhase
instance ToJSON GamePhase

data GameOverReason =
  AllEvilPoliciesPlayed |
  AllGoodPoliciesPlayed |
  EvilLeaderElected |
  EvilLeaderExecuted
  deriving stock (Generic)
instance FromJSON GameOverReason
instance ToJSON GameOverReason

gameOverWinner :: GameOverReason -> Alignment
gameOverWinner AllEvilPoliciesPlayed = Evil
gameOverWinner AllGoodPoliciesPlayed = Good
gameOverWinner EvilLeaderElected = Evil
gameOverWinner EvilLeaderExecuted = Good

isVotePhase :: GamePhase -> Bool
isVotePhase VotePhase {} = True
isVotePhase _ = False

data Government = Government {
  presidentId :: PlayerId,
  chancellorId :: PlayerId
} deriving stock (Generic)
instance FromJSON Government
instance ToJSON Government

data Policy =
  GoodPolicy |
  EvilPolicy
  deriving stock (Generic)
instance FromJSON Policy
instance ToJSON Policy

policyAlignment :: Policy -> Alignment
policyAlignment GoodPolicy = Good
policyAlignment EvilPolicy = Evil

data GameAction =
  NominateChancellor PlayerId |
  PlaceVote Bool |
  PresidentDiscardPolicy Int |
  ChancellorDiscardPolicy Int |
  StopPeekingPolicies |
  ExecutePlayer PlayerId |
  ProposeVeto |
  AcceptVeto |
  RejectVeto
  deriving stock (Generic)
instance FromJSON GameAction
instance ToJSON GameAction

data GameEvent =
  ChancellorNominated {
    presidentialCandidateId :: PlayerId,
    chancellorCandidateId :: PlayerId
  } |
  VotePlaced {
    playerId :: PlayerId,
    vote :: Bool
  } |
  VoteSucceeded {
    presidentId :: PlayerId,
    chancellorId :: PlayerId
  } |
  VoteFailed {
    presidentialCandidateId :: PlayerId,
    chancellorCandidateId :: PlayerId,
    policyEnacted :: Maybe Policy
  } |
  PresidentDiscardedPolicy {
    presidentId :: PlayerId
  } |
  ChancellorEnactedPolicy {
    chancellorId :: PlayerId,
    policy :: Policy
  } |
  PresidentStoppedPeekingPolicies {
    presidentId :: PlayerId
  } |
  PlayerExecuted {
    presidentId :: PlayerId,
    playerId :: PlayerId
  } |
  VetoProposed {
    presidentId :: PlayerId,
    chancellorId :: PlayerId
  } |
  VetoAccepted {
    presidentId :: PlayerId,
    chancellorId :: PlayerId,
    policyEnacted :: Maybe Policy
  } |
  VetoRejected {
    presidentId :: PlayerId,
    chancellorId :: PlayerId
  } |
  InvalidGameAction {
    message :: Text
  }
  deriving stock (Generic)
instance FromJSON GameEvent
instance ToJSON GameEvent
