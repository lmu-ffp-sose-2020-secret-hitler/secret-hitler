module Game where

import Control.Lens
import Data.Text
import Data.HashMap.Strict as HashMap
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
      chancellor :: Username,
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
      president :: Username
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

getAlignment :: Role -> Alignment
getAlignment GoodRole = Good
getAlignment _ = Evil

gameInitial :: Username -> Player -> Game
gameInitial username player =
  Game
    {
      phase = ChancellorNomination,
      players = HashMap.singleton username player,
      president = username
    }

update :: Game -> Event -> Game
update game@(Game {phase}) (UserInput actor userInput)
  | Election election <- phase, Vote vote <- userInput =
    let
      preliminaryResultNew :: HashMap Username (Maybe Vote)
      preliminaryResultNew = insert actor (Just vote) (election ^. #preliminaryResult)
    in
      game & #phase .~
        (
          -- try and convert the new preliminary result into a final result
          case sequenceA preliminaryResultNew of
            Nothing -> Election (election & #preliminaryResult .~ resultsNew)
            Just finalResult ->
              PresidentDiscardPolicy (MakePresidentDiscardPolicy finalResult)
        )
  | otherwise = error "invalid input" -- to-do. exception handling

testGame :: Game
testGame =
  Game {
    phase = Election (MakeElection "lol" HashMap.empty),
    players = HashMap.empty,
    president = "lol"
  }
