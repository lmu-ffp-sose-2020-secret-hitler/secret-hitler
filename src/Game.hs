module Game where

import           Control.Lens hiding (element)
import           Data.Maybe
import           Data.Monoid
import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty as NonEmpty
import Data.Bool (bool)
import Control.Applicative ((<|>))

data Alignment = Good
               | Evil
  deriving stock (Show)

data Role = GoodRole
          | EvilRole
          | EvilLeaderRole
  deriving stock (Show)

data Vote =
  No |
  Yes
  deriving stock (Show)

data Player = Player{
  role  :: Role,
  vote  :: Maybe Vote
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
} deriving stock (Show, Generic)

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
  deriving newtype (Show, Eq, Ord)

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

data ClientEvent =
  UserInput PlayerId UserInput

data GameEvent =
  SucceedVote |
  FailVote

data UserInput =
  Vote Vote |
  NominateChancellor PlayerId

update :: Game -> ClientEvent -> (Game, Maybe GameEvent)
update game@(Game {phase}) (UserInput actor userInput)
  | VotePhase votePhaseData <- phase, Vote vote <- userInput =
      registreVote game votePhaseData actor vote
  | otherwise = error "invalid input" -- to-do. exception handlin

registreVote ::
  Game -> VotePhaseData -> PlayerId -> Vote -> (Game, Maybe GameEvent)
registreVote game votePhaseData actor vote =
  case resultOverall of
    Nothing -> (gameNew, Nothing)
    Just Yes -> (succeedVote gameNew, Just SucceedVote)
    Just No -> (failVote gameNew, Just FailVote)
  where
    gameNew :: Game
    gameNew = set (#players . ix actor . #vote) (Just vote) game -- to-do. Are we fine with neither checking if the actor has voted already nor its existence here?
    resultsIndividual :: Maybe (Map PlayerId Vote)
    resultsIndividual = traverse (view #vote) (gameNew ^. #players)
    resultOverall :: Maybe (Vote)
    resultOverall =
      fmap (bool No Yes) $
      fmap (> Sum 0) $
      fmap (foldMap voteToSum) $
      resultsIndividual
    succeedVote :: Game -> Game
    succeedVote =
      set
        #phase
        (
          PresidentDiscardPolicyPhase $
          PresidentDiscardPolicyPhaseData $
          votePhaseData ^. #chancellorCandidate
        )
      .
      set #electionTracker 0
    failVote :: Game -> Game
    failVote =
      set
        #phase
        (
          NominateChancellorPhase $
          NominateChancellorPhaseData $
          votePhaseData ^. #governmentPrevious
        )
      .
      over #presidentTracker updatePresidentTracker
      .
      over #electionTracker (+1)
    updatePresidentTracker :: PresidentTracker -> PresidentTracker
    updatePresidentTracker =
      fromMaybe (error "all players dying should not be possible")
      .
      passPresidencyRegularly (gameNew ^. #players)

voteToSum :: Vote -> Sum Integer
voteToSum No = Sum (-1)
voteToSum Yes = Sum 1

passPresidencyRegularly ::
  Map PlayerId value -> PresidentTracker -> Maybe PresidentTracker
passPresidencyRegularly playerIds presidentTracker =
  passPresidencyTo <$>
    (
      fst <$> (Map.lookupGT (presidentTracker ^. #regularPresidentLatest) playerIds)
      <|>
      (fmap NonEmpty.head $ NonEmpty.nonEmpty $ Map.keys $ playerIds)
    )
  where
    passPresidencyTo :: PlayerId -> PresidentTracker
    passPresidencyTo nextPresident =
      set #president nextPresident $
      set #regularPresidentLatest nextPresident $
      presidentTracker
