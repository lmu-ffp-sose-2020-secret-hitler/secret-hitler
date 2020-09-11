{-# OPTIONS_GHC -Wno-partial-fields #-}

module Game where

import Control.Applicative ((<|>))
import Control.Lens hiding (element)
import Data.Bool (bool)
import Data.Generics.Labels ()
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, generate)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import System.Random (StdGen, newStdGen)
import VectorShuffling.Immutable (shuffle)

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
  deriving stock (Show, Read)

data Player = Player {
  name :: Text,
  turnOrder :: Int,
  role :: Role,
  vote :: Maybe Vote,
  alive :: Bool
} deriving stock (Show, Generic)

instance Eq Player where
  p1 == p2 = (p1 ^. #turnOrder) == (p2 ^. #turnOrder)

instance Ord Player where
  compare p1 p2 = compare (p1 ^. #turnOrder) (p2 ^. #turnOrder)

newPlayer :: Text -> Int -> Role -> Player
newPlayer name turnOrder role = Player {
  name,
  -- A number in [0;playerCount[ specifying the turn order
  turnOrder,
  role,
  vote = Nothing,
  alive = True
}

data Policy =
  GoodPolicy |
  EvilPolicy
  deriving stock (Show)

data Government = Government {
  president :: Int,
  chancellor :: Int
} deriving stock (Show)

data GamePhase =
  NominateChancellorPhase {
    governmentPrevious :: Maybe Government
  } |
  VotePhase {
    governmentPrevious :: Maybe Government,
    chancellorCandidateId :: Int
  } |
  PresidentDiscardPolicyPhase {
    chancellorId :: Int
  } |
  ChancellorDiscardPolicyPhase {
    chancellorId :: Int
  }
  deriving stock (Show)

data Game = Game {
  phase :: GamePhase,
  -- players includes dead players too.
  -- Use getAlivePlayers instead of #players wherever possible.
  players :: IntMap Player,
  -- The cardPile contains the drawPile and the currentHand
  cardPile :: [Policy],
  goodPolicyCount :: Int,
  evilPolicyCount :: Int,
  presidentId :: Int,
  regularPresidentId :: Int,
  electionTracker :: Int
} deriving stock (Show, Generic)

getPresident :: Game -> Player
getPresident game =
  let presidentIdOld = game ^. #regularPresidentId in
  fromMaybe
    (error "president is not a player")
    (game ^. #players . at presidentIdOld)

getAlivePlayers :: Game -> IntMap Player
getAlivePlayers game =
  IntMap.filter (view #alive) $
  view #players $
  game

drawPile :: Game -> [Policy]
drawPile game@(Game { cardPile }) = drop (currentHandSize game) (cardPile)

currentHand :: Game -> [Policy]
currentHand game@(Game { cardPile }) = take (currentHandSize game) (cardPile)

currentHandSize :: Num p => Game -> p
currentHandSize (Game { phase }) = case phase of
  PresidentDiscardPolicyPhase {} -> 3
  ChancellorDiscardPolicyPhase {} -> 2
  _ -> 0

policyCount :: Policy -> ASetter Game Game Int Int
policyCount GoodPolicy = #goodPolicyCount
policyCount EvilPolicy = #evilPolicyCount

newGame :: IntMap Player -> [Policy] -> Game
newGame players drawPile = Game {
  phase = NominateChancellorPhase Nothing,
  players,
  cardPile = drawPile,
  evilPolicyCount = 0,
  goodPolicyCount = 0,
  presidentId = 0,
  regularPresidentId = 0,
  electionTracker = 0
}

generateRandomGame :: IntMap Text -> IO Game
generateRandomGame playerNames = do
  rngTurnOrder <- newStdGen
  rngRoles <- newStdGen
  rngDrawPile <- newStdGen
  let players = generateRandomPlayers playerNames rngTurnOrder rngRoles
  let drawPile = generateRandomCardPile 6 11 rngDrawPile
  pure $ newGame players drawPile

generateRandomPlayers :: IntMap Text -> StdGen -> StdGen -> IntMap Player
generateRandomPlayers playerNames rngTurnOrder rngRoles =
  let playerCount = IntMap.size playerNames
      turnOrders = generateRandomTurnOrders playerCount rngTurnOrder
      roles = generateRandomRoles playerCount rngRoles in
  IntMap.fromAscList $
  zipWith3 (\(id, name) turnOrder role -> (id, newPlayer name turnOrder role))
    (IntMap.toAscList playerNames) (Vector.toList turnOrders) (Vector.toList roles)

generateRandomTurnOrders :: Int -> StdGen -> Vector Int
generateRandomTurnOrders playerCount rng =
  fst $ shuffle (generate playerCount id) rng

generateRandomRoles :: Int -> StdGen -> Vector Role
generateRandomRoles playerCount rng =
  fst $ shuffle (generate playerCount currentRole) rng
  where
    currentRole :: Int -> Role
    currentRole playerCount = case playerCount of
      0 -> EvilLeaderRole
      1 -> GoodRole
      2 -> GoodRole
      3 -> GoodRole
      4 -> EvilRole
      5 -> GoodRole
      6 -> EvilRole
      7 -> GoodRole
      8 -> EvilRole
      9 -> GoodRole
      _ -> error $ "Unsupported player count " ++ show playerCount

generateRandomCardPile :: Int -> Int -> StdGen -> [Policy]
generateRandomCardPile goodPolicyCount evilPolicyCount rng =
  let
    goodPolicies = Vector.replicate goodPolicyCount GoodPolicy
    evilPolicies = Vector.replicate evilPolicyCount EvilPolicy
  in
  Vector.toList $
  fst $
  shuffle (goodPolicies Vector.++ evilPolicies) rng

data PlayerAction =
  GameAction Int GameAction
  deriving stock (Show, Read)

data GameAction =
  NominateChancellor Int |
  PlaceVote Vote |
  PresidentDiscardPolicy Int |
  ChancellorDiscardPolicy Int
  deriving stock (Show, Read)

data GameEvent =
  ChancellorNominated |
  VotePlaced |
  SucceedVote |
  FailVote |
  PresidentDiscardedPolicy |
  ChancellorDiscardedPolicy |
  Error Text
  deriving stock (Show)

updateChecked :: PlayerAction -> Game -> (Game, GameEvent)
updateChecked event@(GameAction actorId _) game
  | isPlayerAllowedToAct actorId game = update event game
  | otherwise = (game, Error $ "Player " <> Text.pack (show actorId) <> " is currently not allowed to act")
  where
  isPlayerAllowedToAct :: Int -> Game -> Bool
  isPlayerAllowedToAct actorId game@(Game { phase }) =
    case phase of
      NominateChancellorPhase {} -> actorId == game ^. #presidentId
      VotePhase {} -> True
      PresidentDiscardPolicyPhase {} -> actorId == game ^. #presidentId
      ChancellorDiscardPolicyPhase { chancellorId } -> actorId == chancellorId

update :: PlayerAction -> Game -> (Game, GameEvent)
update (GameAction actorId userInput) =
  case userInput of
    NominateChancellor playerId -> nominateChancellor playerId
    PlaceVote vote -> placeVote actorId vote
    PresidentDiscardPolicy policyIndex -> discardPolicy policyIndex
    ChancellorDiscardPolicy policyIndex -> discardPolicy policyIndex

withGameEvent :: GameEvent -> Game -> (Game, GameEvent)
withGameEvent = flip (,)

nominateChancellor :: Int -> Game -> (Game, GameEvent)
nominateChancellor playerId gameOld@(Game {
  phase = NominateChancellorPhase { governmentPrevious }
}) =
  withGameEvent ChancellorNominated $
  set (#players . traversed . #vote) Nothing $
  set #phase (VotePhase {
    chancellorCandidateId = playerId,
    governmentPrevious = governmentPrevious
  }) $
  gameOld
nominateChancellor _playerId gameOld =
  (gameOld, Error $ "Cannot nominate a chancellor outside of NominateChancellorPhase")

placeVote :: Int -> Vote -> Game -> (Game, GameEvent)
placeVote actorId vote gameOld@(Game {
  phase = VotePhase {
    governmentPrevious,
    chancellorCandidateId
  }
}) =
  let gameNew = set (#players . ix actorId . #vote) (Just vote) gameOld in
  case voteResult gameNew of
    Nothing -> (gameNew, VotePlaced)
    Just Yes -> (succeedVote gameNew, SucceedVote)
    Just No -> (failVote gameNew, FailVote)
  where
    voteResult :: Game -> Maybe (Vote)
    voteResult game =
      fmap (bool No Yes) $
      fmap (> Sum 0) $
      fmap (foldMap voteToSum) $
      playerVoteResults game
    playerVoteResults :: Game -> Maybe (IntMap Vote)
    playerVoteResults game = traverse (view #vote) (getAlivePlayers game)
    voteToSum :: Vote -> Sum Integer
    voteToSum No = Sum (-1)
    voteToSum Yes = Sum 1
    succeedVote :: Game -> Game
    succeedVote =
      set #phase (PresidentDiscardPolicyPhase {
          chancellorId = chancellorCandidateId
      })
    failVote :: Game -> Game
    failVote =
      set #phase (NominateChancellorPhase {
        governmentPrevious = governmentPrevious
      })
      .
      nominateNextRegularPresident
      .
      advanceElectionTracker
placeVote _actorId _vote gameOld =
  (gameOld, Error $ "Cannot vote outside of VotePhase")

advanceElectionTracker :: Game -> Game
advanceElectionTracker game@(Game { electionTracker }) =
  if electionTracker < 2
  then over #electionTracker (+1) game
  else enactTopPolicy game

nominateNextRegularPresident :: Game -> Game
nominateNextRegularPresident gameOld =
  let presidentIdNew = nextRegularPresidentId gameOld in
  gameOld
    & #regularPresidentId .~ presidentIdNew
    & #presidentId .~ presidentIdNew
  where
    nextRegularPresidentId :: Game -> Int
    nextRegularPresidentId game =
      let president = getPresident game
          alivePlayers = IntMap.toList $ getAlivePlayers game in
      fst $
      fromMaybe (error "all players dying should not be possible") $
        (minimumMaybe $ filter ((president <) . snd) alivePlayers)
        <|> (minimumMaybe $ alivePlayers)
    minimumMaybe :: Ord a => [a] -> Maybe a
    minimumMaybe [] = Nothing
    minimumMaybe list = Just $ minimum list

discardPolicy :: Int -> Game -> (Game, GameEvent)
discardPolicy policyIndex gameOld =
  case removePolicy policyIndex gameOld of
    Left error -> (gameOld, Error $ error)
    Right gameNew@(Game { phase }) ->
      case phase of
        PresidentDiscardPolicyPhase { chancellorId } ->
          withGameEvent PresidentDiscardedPolicy $
          set #phase (ChancellorDiscardPolicyPhase {
            chancellorId
          }) $
          gameNew
        ChancellorDiscardPolicyPhase {} ->
          withGameEvent ChancellorDiscardedPolicy $
          nominateNextRegularPresident $
          enactTopPolicy $
          gameNew
        _ -> (gameOld, Error $ "Cannot discard policy outside of PresidentDiscardPolicyPhase or ChancellorDiscardPolicyPhase")
  where
    removePolicy :: Int -> Game -> Either Text Game
    removePolicy policyIndex game =
      if 0 <= policyIndex && policyIndex < currentHandSize game
      then Right $ over #cardPile (removeElement policyIndex) game
      else Left "Cannot discard policy outside of current hand"
    removeElement :: Int -> [a] -> [a]
    removeElement i list = take i list ++ drop (i + 1) list

enactTopPolicy :: Game -> Game
enactTopPolicy gameOld@(Game { cardPile = policy : cardPileTail }) =
  over (policyCount policy) (+1) $
  set #cardPile cardPileTail $
  set #electionTracker 0 $
  gameOld
enactTopPolicy _gameOld =
  error "Cannot enact top policy from empty card pile"
