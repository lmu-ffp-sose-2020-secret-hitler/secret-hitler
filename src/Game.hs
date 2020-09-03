module Game where

import           Control.Lens hiding (element)
import           Data.Maybe
import           Data.Monoid
import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Bool (bool)
import Control.Applicative ((<|>))

data Alignment =
  Good |
  Evil
  deriving stock (Show)

data Role =
  GoodRole |
  EvilRole |
  EvilLeaderRole
  deriving stock (Show)

data Vote =
  No |
  Yes
  deriving stock (Show)

data Player = Player {
  role  :: Role,
  vote  :: Maybe Vote,
  alive :: Bool
}
  deriving stock (Show, Generic)

data Policy =
  GoodPolicy |
  EvilPolicy
  deriving stock (Show)

data Government = Government {
  president :: PlayerId,
  chancellor :: PlayerId
}
  deriving stock (Show)

data NominateChancellorPhasePayload = NominateChancellorPhasePayload {
  governmentPrevious :: Maybe Government
}
  deriving stock (Show)

data VotePhasePayload = VotePhasePayload {
  governmentPrevious :: Maybe Government,
  chancellorCandidate :: PlayerId
}
  deriving stock (Show, Generic)

data PresidentDiscardPolicyPhasePayload = PresidentDiscardPolicyPhasePayload {
  chancellor :: PlayerId
}
  deriving stock (Show)

data GamePhase =
  NominateChancellorPhase NominateChancellorPhasePayload |
  VotePhase VotePhasePayload |
  PresidentDiscardPolicyPhase PresidentDiscardPolicyPhasePayload |
  ChancellorDiscardPolicyPhase
  deriving stock (Show)

data PresidentTracker = PresidentTracker {
    president :: PlayerId,
    regularPresidentLatest :: PlayerId
}
  deriving stock (Show, Generic)

newtype PlayerId =
  PlayerId Int
  deriving newtype (Show, Eq, Ord)

data Game = Game {
  phase                 :: GamePhase,
  -- players includes dead players too.
  -- Use the getter alivePlayers instead of #players wherever possible.
  players               :: NEMap PlayerId Player,
  drawPile              :: [Policy],
  goodPolicies          :: Int,
  evilPolicies          :: Int,
  presidentTracker :: PresidentTracker,
  electionTracker       :: Int
}
  deriving stock (Show, Generic)

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
  | VotePhase votePhasePayload <- phase, Vote vote <- userInput =
      registreVote game votePhasePayload actor vote
  | otherwise = error "invalid input" -- to-do. exception handlin

registreVote ::
  Game -> VotePhasePayload -> PlayerId -> Vote -> (Game, Maybe GameEvent)
registreVote gameOld votePhasePayload actor vote =
  case resultOverall of
    Nothing -> (game, Nothing)
    Just Yes -> (succeedVote game, Just SucceedVote)
    Just No -> (failVote game, Just FailVote)
  where
    resultOverall :: Maybe (Vote)
    resultOverall =
      fmap (bool No Yes) $
      fmap (> Sum 0) $
      fmap (foldMap voteToSum) $
      resultsIndividual
    resultsIndividual :: Maybe (Map PlayerId Vote)
    resultsIndividual = traverse (view #vote) (game ^. alivePlayers)
    game :: Game
    game = set (#players . ix actor . #vote) (Just vote) gameOld
    voteToSum :: Vote -> Sum Integer
    voteToSum No = Sum (-1)
    voteToSum Yes = Sum 1
    succeedVote :: Game -> Game
    succeedVote =
      set
        #phase
        (
          PresidentDiscardPolicyPhase $
          PresidentDiscardPolicyPhasePayload {
            chancellor = votePhasePayload ^. #chancellorCandidate
          }
        )
      .
      set #electionTracker 0
    failVote :: Game -> Game
    failVote =
      set
        #phase
        (
          NominateChancellorPhase $
          NominateChancellorPhasePayload {
            governmentPrevious = votePhasePayload ^. #governmentPrevious
          }
        )
      .
      over #presidentTracker updatePresidentTracker
      .
      over #electionTracker (+1)
    updatePresidentTracker :: PresidentTracker -> PresidentTracker
    updatePresidentTracker =
      passPresidencyRegularly (game ^. alivePlayers)

alivePlayers :: Getter Game (Map PlayerId Player)
alivePlayers = #players . to (NEMap.filter (view #alive))

passPresidencyRegularly ::
  Map PlayerId value -> PresidentTracker -> PresidentTracker
passPresidencyRegularly playerIds presidentTracker =
  passPresidencyTo $
  fromMaybe (error "all players dying should not be possible") $
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

type instance Index (NEMap key _value) = key
type instance IxValue (NEMap _key value) = value
instance Ord key => Ixed (NEMap key value) where
  ix key f m =
    case NEMap.lookup key m of
      Just value  -> (\valueNew -> NEMap.insert key valueNew m) <$> f value
      Nothing -> pure m
