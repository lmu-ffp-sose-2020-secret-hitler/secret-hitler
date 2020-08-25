module Game where

import           Control.Lens hiding (element)
import           Data.Maybe
import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)

data Alignment = Good
               | Evil
  deriving stock (Show)

data Role = GoodRole
          | EvilRole
          | EvilLeaderRole
  deriving stock (Show)

data Player = Player{
  role  :: Role,
  vote  :: Maybe Bool
} deriving stock (Show, Generic)

data Policy = GoodPolicy
            | EvilPolicy
  deriving stock (Show)

data Government =
  Government
    {
      president :: PlayerId,
      chancellor :: PlayerId
    }
  deriving stock (Show)

data NominateChancellorPhaseData =
  NominateChancellorPhaseData
    {
      governmentPrevious :: Maybe Government
    }
  deriving stock (Show)

data VotePhaseData = VotePhaseData{
  governmentPrevious :: Maybe Government,
  chancellorCandidate :: PlayerId
} deriving stock (Show)

data PresidentDiscardPolicyPhaseData =
  PresidentDiscardPolicyPhaseData
    {
      chancellor :: PlayerId
    }
  deriving stock (Show)

data GamePhase = NominateChancellorPhase NominateChancellorPhaseData
               | VotePhase VotePhaseData
               | PresidentDiscardPolicyPhase PresidentDiscardPolicyPhaseData
               | ChancellorDiscardPolicyPhase
  deriving stock (Show)

data PresidentTracker =
  PresidentTracker {
    president :: PlayerId,
    regularPresidentLatest :: PlayerId
  }
  deriving stock (Show, Generic)

newtype PlayerId =
  PlayerId Int
  deriving newtype (Show, Eq)

data Game = Game{
  phase                 :: GamePhase,
  players               :: Map PlayerId Player,
  deadPlayers :: Map PlayerId Player,
  drawPile              :: [Policy],
  goodPolicies          :: Int,
  evilPolicies          :: Int,
  presidentTracker :: PresidentTracker,
  electionTracker       :: Int
} deriving stock (Show, Generic)

succeedingElement :: Eq element => element -> [element] -> Maybe element
succeedingElement element list =
  listToMaybe $
  drop 1 $
  dropWhile (/= element) $
  list ++ (take 1 list)

passPresidencyRegularly :: [PlayerId] -> PresidentTracker -> Maybe PresidentTracker
passPresidencyRegularly playerIds presidentTracker =
  passPresidencyTo
  <$>
  (succeedingElement (presidentTracker ^. #regularPresidentLatest) playerIds)
  where
    passPresidencyTo :: PlayerId -> PresidentTracker
    passPresidencyTo nextPresident =
      set #president nextPresident $
      set #regularPresidentLatest nextPresident $
      presidentTracker
