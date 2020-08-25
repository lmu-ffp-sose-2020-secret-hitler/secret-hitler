module Game where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import GHC.Generics (Generic)
import Data.Generics.Labels ()

data Alignment = Good
               | Evil
  deriving stock (Show)

data Role = GoodRole
          | EvilRole
          | EvilLeaderRole
  deriving stock (Show)

getAlignment :: Role -> Alignment
getAlignment GoodRole = Good
getAlignment _        = Evil

data Player = Player{
  role  :: Role,
  vote  :: Maybe Bool,
  alive :: Bool
} deriving stock (Show, Generic)

newPlayer :: Role -> Player
newPlayer role = Player{
  role = role,
  vote = Nothing,
  alive = True
}

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

newGame :: Int -> Game
newGame playerCount = Game{
  phase = NominateChancellorPhase,
  players = shufflePlayers playerCount,
  drawPile = shuffleDrawPile 6 11,
  evilPolicies = 0,
  goodPolicies = 0,
  presidentialCandidate = 0,
  president = Nothing,
  chancellor = Nothing,
  electionTracker = 0
}

-- TODO Random order
shufflePlayers :: Int -> [Player]
shufflePlayers playerCount =
  let roles = case playerCount of
                5 -> EvilLeaderRole : EvilRole : replicate 3 GoodRole
                6 -> EvilLeaderRole : EvilRole : replicate 4 GoodRole
  in
  map newPlayer roles

-- TODO Random order
shuffleDrawPile :: Int -> Int -> [Policy]
shuffleDrawPile g e = replicate g GoodPolicy ++ replicate e EvilPolicy

nominateChancellor :: Int -> Game -> Game
nominateChancellor playerIndex game =
  set (#players . each . #vote) Nothing game{
    phase = VotePhase VotePhaseData { chancellorCandidate = playerIndex }
  }

setVote :: Int -> Maybe Bool -> Game -> Game
setVote playerIndex vote' game =
  case phase game of
    VotePhase VotePhaseData { chancellorCandidate = chancellorCandidate } ->
      let game' = set (#players . ix playerIndex . #vote) vote' game in
      if anyOf (#players . each . #vote) isNothing game'
      then game'
      else if getSum (foldMapOf (#players . each . #vote . _Just) boolToSum game') > 0
        then -- majority voted yes
          voteSucceeded game'
        else -- majority voted no (or tie)
          voteFailed game'
      where
        boolToSum b = if b then Sum 1 else Sum (-1)
        voteSucceeded game =
          game{
            phase = PresidentDiscardPolicyPhase,
            president = Just (presidentialCandidate game),
            chancellor = Just chancellorCandidate
          }
        voteFailed game =
          selectNextPresidentialCandidate $ advanceElectionTracker game
    _ -> error "wrong game phase"

-- TODO: dead players can't run for president
selectNextPresidentialCandidate :: Game -> Game
selectNextPresidentialCandidate game =
  let playerCount = length (players game) in
  over #presidentialCandidate (\it -> (it + 1) `mod` playerCount) game{
    phase = NominateChancellorPhase
  }

advanceElectionTracker :: Game -> Game
advanceElectionTracker game =
  if electionTracker game < 2
  then over #electionTracker (+1) game
  else enactTopPolicy game{electionTracker = 0}

class GetCurrentHandSize a where
  getCurrentHandSize :: Num n => a -> n

instance GetCurrentHandSize Game where
  getCurrentHandSize game = getCurrentHandSize (phase game)

instance GetCurrentHandSize GamePhase where
  getCurrentHandSize phase = case phase of
    PresidentDiscardPolicyPhase  -> 3
    ChancellorDiscardPolicyPhase -> 2

getCurrentHand :: Game -> [Policy]
getCurrentHand game = take (getCurrentHandSize game) (drawPile game)

removeElement :: Int -> [a] -> [a]
removeElement index list = take index list ++ drop (index + 1) list

discardPolicy :: Int -> Game -> Game
discardPolicy policyIndex game =
  let game' = removePolicy policyIndex game in
  case phase game' of
    PresidentDiscardPolicyPhase  -> game'{phase = ChancellorDiscardPolicyPhase}
    ChancellorDiscardPolicyPhase -> selectNextPresidentialCandidate $ enactTopPolicy game'
    where
      removePolicy :: Int -> Game -> Game
      removePolicy policyIndex game =
        if policyIndex < 0 || getCurrentHandSize game <= policyIndex
        then error "Cannot discard policy outside of current hand"
        else over #drawPile (removeElement policyIndex) game

enactTopPolicy :: Game -> Game
enactTopPolicy game =
  let (policy:drawPile') = drawPile game
      game' = game{drawPile = drawPile'}
  in
  case policy of
    GoodPolicy -> over #goodPolicies (+1) game'
    EvilPolicy -> over #evilPolicies (+1) game'
