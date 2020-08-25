module Game where

import           Control.Lens
import           Data.Maybe
import GHC.Generics (Generic)
import Data.Generics.Labels ()

data Alignment = Good
               | Evil
  deriving stock (Show)

data Role = GoodRole
          | EvilRole
          | EvilLeaderRole
  deriving stock (Show)

data Player = Player{
  role  :: Role,
  vote  :: Maybe Bool,
  alive :: Bool
} deriving stock (Show, Generic)

data Policy = GoodPolicy
            | EvilPolicy
  deriving stock (Show)

data VotePhaseData = VotePhaseData{
  chancellorCandidate :: Int
} deriving stock (Show)

data GamePhase = NominateChancellorPhase
               | VotePhase VotePhaseData
               | PresidentDiscardPolicyPhase
               | ChancellorDiscardPolicyPhase
  deriving stock (Show)

data Game = Game{
  phase                 :: GamePhase,
  players               :: [Player],
  drawPile              :: [Policy],
  goodPolicies          :: Int,
  evilPolicies          :: Int,
  presidentialCandidate :: Int,
  president             :: Maybe Int,
  chancellor            :: Maybe Int,
  electionTracker       :: Int
} deriving stock (Show, Generic)
