{-# LANGUAGE TemplateHaskell #-}
module Data where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid

data Alignment = Good
               | Evil
  deriving (Show)

data Role = GoodRole
          | EvilRole
          | EvilLeaderRole
  deriving (Show)

getAlignment :: Role -> Alignment
getAlignment GoodRole = Good
getAlignment _        = Evil

data Player = Player{
  _role  :: Role,
  _vote  :: Maybe Bool,
  _alive :: Bool
} deriving (Show)
makeLenses ''Player

newPlayer :: Role -> Player
newPlayer role = Player{
  _role = role,
  _vote = Nothing,
  _alive = True
}

data Policy = GoodPolicy
            | EvilPolicy
  deriving (Show)

data VotePhaseData = VotePhaseData{
  _chancellorCandidate :: Int
} deriving (Show)

data GamePhase = NominateChancellorPhase
               | VotePhase VotePhaseData
               | PresidentDiscardPolicyPhase
               | ChancellorDiscardPolicyPhase
  deriving (Show)

data Game = Game{
  _phase                 :: GamePhase,
  _players               :: [Player],
  _drawPile              :: [Policy],
  _goodPolicies          :: Int,
  _evilPolicies          :: Int,
  _presidentialCandidate :: Int,
  _president             :: Maybe Int,
  _chancellor            :: Maybe Int,
  _electionTracker       :: Int
} deriving (Show)
makeLenses ''Game

newGame :: Int -> Game
newGame playerCount = Game{
  _phase = NominateChancellorPhase,
  _players = shufflePlayers playerCount,
  _drawPile = shuffleDrawPile 6 11,
  _evilPolicies = 0,
  _goodPolicies = 0,
  _presidentialCandidate = 0,
  _president = Nothing,
  _chancellor = Nothing,
  _electionTracker = 0
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
  set (players.each.vote) Nothing game{
    _phase = VotePhase VotePhaseData { _chancellorCandidate = playerIndex }
  }

setVote :: Int -> Maybe Bool -> Game -> Game
setVote playerIndex vote' game =
  case _phase game of
    VotePhase VotePhaseData { _chancellorCandidate = chancellorCandidate } ->
      let game' = set (players.ix playerIndex.vote) vote' game in
      if anyOf (players.each.vote) isNothing game'
      then game'
      else if getSum (foldMapOf (players.each.vote._Just) boolToSum game') > 0
        then -- majority voted yes
          voteSucceeded game'
        else -- majority voted no (or tie)
          voteFailed game'
      where
        boolToSum b = if b then Sum 1 else Sum (-1)
        voteSucceeded game =
          game{
            _phase = PresidentDiscardPolicyPhase,
            _president = Just (_presidentialCandidate game),
            _chancellor = Just chancellorCandidate
          }
        voteFailed game =
          selectNextPresidentialCandidate $ advanceElectionTracker game
    _ -> error "wrong game phase"

-- TODO: dead players can't run for president
selectNextPresidentialCandidate :: Game -> Game
selectNextPresidentialCandidate game =
  let playerCount = length (_players game) in
  over presidentialCandidate (\it -> (it + 1) `mod` playerCount) game{
    _phase = NominateChancellorPhase
  }

advanceElectionTracker :: Game -> Game
advanceElectionTracker game =
  if _electionTracker game < 2
  then over electionTracker (+1) game
  else enactTopPolicy game{_electionTracker = 0}

class GetCurrentHandSize a where
  getCurrentHandSize :: Num n => a -> n

instance GetCurrentHandSize Game where
  getCurrentHandSize game = getCurrentHandSize (_phase game)

instance GetCurrentHandSize GamePhase where
  getCurrentHandSize phase = case phase of
    PresidentDiscardPolicyPhase  -> 3
    ChancellorDiscardPolicyPhase -> 2

getCurrentHand :: Game -> [Policy]
getCurrentHand game = take (getCurrentHandSize game) (_drawPile game)

removeElement :: Int -> [a] -> [a]
removeElement index list = take index list ++ drop (index + 1) list

discardPolicy :: Int -> Game -> Game
discardPolicy policyIndex game =
  let game' = removePolicy policyIndex game in
  case _phase game' of
    PresidentDiscardPolicyPhase  -> game'{_phase = ChancellorDiscardPolicyPhase}
    ChancellorDiscardPolicyPhase -> selectNextPresidentialCandidate $ enactTopPolicy game'
    where
      removePolicy policyIndex game =
        if policyIndex < 0 || getCurrentHandSize game <= policyIndex
        then error "Cannot discard policy outside of current hand"
        else over drawPile (removeElement policyIndex) game

enactTopPolicy :: Game -> Game
enactTopPolicy game =
  let (policy:drawPile) = _drawPile game
      game' = game{_drawPile = drawPile}
  in
  case policy of
    GoodPolicy -> over goodPolicies (+1) game'
    EvilPolicy -> over evilPolicies (+1) game'
