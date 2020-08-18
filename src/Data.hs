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

getAlignment GoodRole = Good
getAlignment _        = Evil

data Player = Player{
  _role  :: Role,
  _vote  :: Maybe Bool,
  _alive :: Bool
} deriving (Show)
makeLenses ''Player

newPlayer role = Player{
  _role = role,
  _vote = Nothing,
  _alive = True
}

data Policy = GoodPolicy
            | EvilPolicy
  deriving (Show)

data GamePhase = NominateChancellor
               | Vote
               | PresidentDiscardPolicy
               | ChancellorDiscardPolicy
  deriving (Show)

data Game = Game{
  _phase                 :: GamePhase,
  _players               :: [Player],
  _drawPile              :: [Policy],
  _goodPolicies          :: Int,
  _evilPolicies          :: Int,
  _presidentialCandidate :: Int,
  _chancellorCandidate   :: Maybe Int,
  _president             :: Maybe Int,
  _chancellor            :: Maybe Int,
  _electionTracker       :: Int
} deriving (Show)
makeLenses ''Game

newGame = Game{
  _phase = NominateChancellor,
  _players = [],
  _drawPile = shuffleDrawPile 6 11,
  _evilPolicies = 0,
  _goodPolicies = 0,
  _presidentialCandidate = 0,
  _chancellorCandidate = Nothing,
  _president = Nothing,
  _chancellor = Nothing,
  _electionTracker = 0
}

-- TODO Random order
shuffleDrawPile g e = replicate g GoodPolicy ++ replicate e EvilPolicy

nominateChancellor playerIndex game = game{_chancellorCandidate = playerIndex, _phase = Vote}

setVote playerIndex vote' game =
  let game' = set (players.ix playerIndex.vote) vote' game in
  if anyOf (players.folded.vote) isNothing game'
  then game'
  else if getSum (foldMapOf (players.folded.vote._Just) boolToSum game') > 0
    then -- majority voted yes
      voteSucceeded game'
    else -- majority voted no (or tie)
      voteFailed game'
  where
    boolToSum b = if b then Sum 1 else Sum (-1)
    voteSucceeded game =
      game{
        _phase = PresidentDiscardPolicy,
        _president = Just (_presidentialCandidate game),
        _chancellor = _chancellorCandidate game
      }
    voteFailed game =
      selectNextPresidentialCandidate $ advanceElectionTracker game{
        _phase = NominateChancellor
      }

selectNextPresidentialCandidate game =
  let playerCount = length (_players game) in
  over presidentialCandidate (\it -> (it + 1) `mod` playerCount) game

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
    PresidentDiscardPolicy  -> 3
    ChancellorDiscardPolicy -> 2

getCurrentHand game = take (getCurrentHandSize game) (_drawPile game)

removeElement index list = take index list ++ drop (index + 1) list

discardPolicy policyIndex game =
  let game' = removePolicy policyIndex game in
  case _phase game' of
    PresidentDiscardPolicy  -> game'{_phase = ChancellorDiscardPolicy}
    ChancellorDiscardPolicy -> enactTopPolicy game'
    where
      removePolicy policyIndex game =
        if policyIndex < 0 || getCurrentHandSize game <= policyIndex
        then error "Cannot discard policy outside of current hand"
        else over drawPile (removeElement policyIndex) game

enactTopPolicy game =
  let (policy:drawPile) = _drawPile game
      game' = game{_drawPile = drawPile}
  in
  case policy of
    GoodPolicy -> over goodPolicies (+1) game'
    EvilPolicy -> over evilPolicies (+1) game'
