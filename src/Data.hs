module Data where

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
  role  :: Role,
  vote  :: Maybe Bool,
  alive :: Bool
} deriving (Show)

data Policy = GoodPolicy
            | EvilPolicy
  deriving (Show)

data GamePhase = NominateChancellor
               | Vote
               | PresidentDiscardPolicy
               | ChancellorDiscardPolicy
  deriving (Show)

data Game = Game {
  phase                 :: GamePhase,
  players               :: [Player],
  drawPile              :: [Policy],
  evilPolicies          :: Int,
  goodPolicies          :: Int,
  presidentialCandidate :: Maybe Int,
  chancellorCandidate   :: Maybe Int,
  president             :: Maybe Int,
  chancellor            :: Maybe Int
} deriving (Show)

newGame = Game {
  phase = NominateChancellor,
  players = [],
  drawPile = shuffleDrawPile 6 11,
  evilPolicies = 0,
  goodPolicies = 0,
  presidentialCandidate = Nothing,
  chancellorCandidate = Nothing,
  president = Nothing,
  chancellor = Nothing
}

-- TODO Random order
shuffleDrawPile g e = replicate g GoodPolicy ++ replicate e EvilPolicy

nominateChancellor game playerIndex = game {chancellorCandidate = playerIndex, phase = Vote}

setVote game playerIndex vote = undefined

getCurrentHand game = case phase game of
  PresidentDiscardPolicy -> take 3 (drawPile game)
  ChancellorDiscardPolicy -> take 2 (drawPile game)
