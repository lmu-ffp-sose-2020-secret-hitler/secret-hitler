module Common.GameMessages where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.IntMap.Strict
import GHC.Generics (Generic)

data GameView = GameView {
  -- The ID of the player that views this game
  playerId :: Int,
  -- The Role of the player that views this game
  playerRole :: Role,
  players :: IntMap PlayerView,
  phase :: GamePhase,
  -- The current hand is empty unless the player is president or chancellor in the correct phase
  currentHand :: [Policy],
  drawPileSize :: Int,
  discardPileSize :: Int,
  goodPolicyCount :: Int,
  evilPolicyCount :: Int,
  presidentId :: Int,
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

policyAlignment :: Policy -> Alignment
policyAlignment GoodPolicy = Good
policyAlignment EvilPolicy = Evil

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
  EvilLeaderElected |
  VoteFailed (Maybe PolicyEnacted) |
  PresidentDiscardedPolicy |
  ChancellorEnactedPolicy PolicyEnacted |
  PresidentStoppedPeekingPolicies |
  PlayerKilled |
  EvilLeaderKilled |
  VetoProposed |
  VetoAccepted (Maybe PolicyEnacted) |
  VetoRejected |
  Error Text
  deriving stock (Generic)
instance FromJSON GameEvent
instance ToJSON GameEvent

eventWinner :: GameEvent -> Maybe Alignment
eventWinner (EvilLeaderElected) = Just Evil
eventWinner (VoteFailed policyEnacted) = policyEnactedWinner =<< policyEnacted
eventWinner (ChancellorEnactedPolicy policyEnacted) = policyEnactedWinner policyEnacted
eventWinner EvilLeaderKilled = Just Good
eventWinner (VetoAccepted policyEnacted) = policyEnactedWinner =<< policyEnacted
eventWinner _ = Nothing

data PolicyEnacted =
  PolicyEnacted Policy |
  LastPolicyEnacted Policy
  deriving stock (Generic)
instance FromJSON PolicyEnacted
instance ToJSON PolicyEnacted

policyEnactedWinner :: PolicyEnacted -> Maybe Alignment
policyEnactedWinner (LastPolicyEnacted policy) = Just (policyAlignment policy)
policyEnactedWinner _ = Nothing

enactedPolicy :: PolicyEnacted -> Policy
enactedPolicy (PolicyEnacted policy) = policy
enactedPolicy (LastPolicyEnacted policy) = policy
