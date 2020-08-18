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

data Player = Player {
  _role  :: Role,
  _vote  :: Maybe Bool,
  _alive :: Bool
} deriving (Show)
makeLenses ''Player

data Policy = GoodPolicy
            | EvilPolicy
  deriving (Show)

data GamePhase = NominateChancellor
               | Vote
               | PresidentDiscardPolicy
               | ChancellorDiscardPolicy
  deriving (Show)

data Game = Game {
  _phase                 :: GamePhase,
  _players               :: [Player],
  _drawPile              :: [Policy],
  _evilPolicies          :: Int,
  _goodPolicies          :: Int,
  _presidentialCandidate :: Maybe Int,
  _chancellorCandidate   :: Maybe Int,
  _president             :: Maybe Int,
  _chancellor            :: Maybe Int
} deriving (Show)
makeLenses ''Game

newGame = Game {
  _phase = NominateChancellor,
  _players = [],
  _drawPile = shuffleDrawPile 6 11,
  _evilPolicies = 0,
  _goodPolicies = 0,
  _presidentialCandidate = Nothing,
  _chancellorCandidate = Nothing,
  _president = Nothing,
  _chancellor = Nothing
}

-- TODO Random order
shuffleDrawPile g e = replicate g GoodPolicy ++ replicate e EvilPolicy

nominateChancellor playerIndex game = game {_chancellorCandidate = playerIndex, _phase = Vote}

setVote playerIndex vote' game =
  let game' = set (players.ix playerIndex.vote) vote' game in
  if anyOf (players.folded.vote) isNothing game'
  then game'
  else if getSum (foldMapOf (players.folded.vote._Just) boolToSum game') > 0
    then game'{ -- majority voted yes
      _phase = PresidentDiscardPolicy,
      _president = _presidentialCandidate game',
      _chancellor = _chancellorCandidate game'
    }
    else game'{ -- majority voted no (or tie)
      _phase = NominateChancellor
      -- TODO select next _presidentialCandidate and advance election tracker
    }
  where boolToSum b = if b then Sum 1 else Sum (-1)

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
  if policyIndex < 0 || getCurrentHandSize game <= policyIndex
  then error "Cannot discard policy outside of current hand"
  else over drawPile (removeElement policyIndex) game
