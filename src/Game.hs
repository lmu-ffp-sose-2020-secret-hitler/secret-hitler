module Game where

import Data.InfiniteList as InfiniteList
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
      preliminaryResult :: HashMap Username (Maybe Vote)
    }
  deriving stock (Show, Generic)

data PresidentDiscardPolicy =
  MakePresidentDiscardPolicy
   {
     electionResult :: HashMap Username Vote
   }
   deriving stock (Show, Generic)

data GamePhase =
  ChancellorNomination |
  Election Election |
  PresidentDiscardPolicy PresidentDiscardPolicy |
  ChancellorDiscardPolicy
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
      presidentialCandidateStack :: InfiniteList Username,
      president :: Maybe Username, -- there is no president in the very first round
      chancellor :: Maybe Username -- there is no chancellor in the very first round
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
      presidentialCandidateStack = InfiniteList.cycle (HashMap.keys players),
      president = Nothing,
      chancellor = Nothing
    }

getAlignment :: Role -> Alignment
getAlignment GoodRole = Good
getAlignment _ = Evil

update :: Game -> Event -> Game
update game@(Game {phase}) (UserInput actor userInput)
  | Election election <- phase, Vote vote <- userInput =
    let
      preliminaryResultNew :: HashMap Username (Maybe Vote)
      preliminaryResultNew = HashMap.insert actor (Just vote) (election ^. #preliminaryResult)
    in
      #presidentialCandidateStack %~ (view #tail)
      $
      -- try and convert the new preliminary result into a final result
      case sequenceA preliminaryResultNew of
        Nothing ->
          #phase .~ Election (election & #preliminaryResult .~ preliminaryResultNew)
        Just finalResult ->
          if getSum (foldMap voteToSum finalResult) > 0
          then
            (#phase .~ PresidentDiscardPolicy (MakePresidentDiscardPolicy finalResult))
            .
            (#chancellor .~ Just (election ^. #chancellorCandidate))
            .
            (#president .~ Just (game ^. #presidentialCandidateStack . #head))
            .
            (#electionTracker .~ 0)
          else
            (#phase .~ ChancellorNomination)
            .
            (#electionTracker %~ (+1))
      $
      game
  | otherwise = error "invalid input" -- to-do. exception handling

voteToSum :: Vote -> Sum Integer
voteToSum No = Sum (-1)
voteToSum Yes = Sum 1
