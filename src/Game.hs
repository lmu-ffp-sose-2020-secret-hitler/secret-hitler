module Game where

import Data.Bool (bool)
import Data.InfiniteList as InfiniteList
import Data.Composition
import Control.Lens
import Data.Text (Text)
import Data.Monoid
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.String (IsString)
import GHC.Generics (Generic)
import Data.Generics.Labels ()

newtype Username =
  Username Text
  deriving stock (Eq)
  deriving newtype (Show, IsString, Hashable)

data Vote =
  No |
  Yes
  deriving stock (Show)

data Role =
  GoodRole |
  EvilRole |
  EvilLeaderRole
  deriving stock (Show)

data Election =
  MakeElection
    {
      chancellorCandidate :: Username,
      preliminaryResult :: HashMap Username Vote
    }
  deriving stock (Show, Generic)

data PresidentDiscardPolicy =
  MakePresidentDiscardPolicy
    {
      electionResult :: HashMap Username Vote,
      chancellor :: Username
    }
    deriving stock (Show, Generic)

data ChancellorDiscardPolicy =
  MakeChancellorDiscardPolicy
    {
      chancellor :: Username
    }
    deriving stock (Show, Generic)

data GamePhase =
  ChancellorNomination |
  Election Election |
  PresidentDiscardPolicy PresidentDiscardPolicy |
  ChancellorDiscardPolicy ChancellorDiscardPolicy
  deriving stock (Show)

data Player =
  Player
    {
      role :: Role
    }
  deriving stock (Show, Generic)

data Game =
  Game
    {
      phase :: GamePhase,
      players :: HashMap Username Player,
      electionTracker :: Integer,
      presidentStack :: InfiniteList Username,
      -- there has not been any presidents previously in the very first round
      presidentPrevious :: Maybe Username,
       -- there has not been any chancellors previously in the very first round
      chancellorPrevious :: Maybe Username
    }
  deriving stock (Show, Generic)

data Alignment =
  Good |
  Evil
  deriving stock (Show)

data Event =
  UserInput Username UserInput

data UserInput =
  Vote Vote |
  NominateChancellor Username

gameInitial :: HashMap Username Player -> Game
gameInitial players =
  Game
    {
      phase = ChancellorNomination,
      players,
      electionTracker = 0,
      presidentStack = InfiniteList.cycle (HashMap.keys players),
      presidentPrevious = Nothing,
      chancellorPrevious = Nothing
    }

getAlignment :: Role -> Alignment
getAlignment GoodRole = Good
getAlignment _ = Evil

update :: Game -> Event -> Game
update game@(Game {phase}) (UserInput actor userInput)
  | Election election <- phase, Vote vote <- userInput =
    let
      preliminaryResultNew :: HashMap Username Vote
      preliminaryResultNew =
        HashMap.insert actor vote (election ^. #preliminaryResult)
      electionIsComplete :: Bool
      electionIsComplete =
        all
          (flip HashMap.member preliminaryResultNew)
          (HashMap.keys (game ^. #players))
      succeedElection :: Game -> Game
      succeedElection =
        (
          #phase
          .~
          (PresidentDiscardPolicy .: MakePresidentDiscardPolicy)
              preliminaryResultNew
              (election ^. #chancellorCandidate)
        )
        .
        (#electionTracker .~ 0)
      failElection :: Game -> Game
      failElection =
        (#phase .~ ChancellorNomination)
        .
        (#presidentStack %~ (view #tail))
        .
        (#electionTracker %~ (+1))
    in
      (
        if not electionIsComplete
        then
          #phase .~ Election (election & #preliminaryResult .~ preliminaryResultNew)
        else
          bool
            failElection
            succeedElection
            (getSum (foldMap voteToSum preliminaryResultNew) > 0)
      )
        game
  | otherwise = error "invalid input" -- to-do. exception handling

voteToSum :: Vote -> Sum Integer
voteToSum No = Sum (-1)
voteToSum Yes = Sum 1
