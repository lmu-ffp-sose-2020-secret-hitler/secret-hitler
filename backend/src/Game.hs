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
import VectorShuffling.Immutable (shuffle)
import GHC.Generics (Generic)
import System.Random (RandomGen, StdGen, randomR, newStdGen)

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
  role  :: Role,
  vote  :: Maybe Vote,
  alive :: Bool
} deriving stock (Show, Generic)

newPlayer :: Role -> Player
newPlayer role = Player {
  role = role,
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

data NominateChancellorPhasePayload = NominateChancellorPhasePayload {
  governmentPrevious :: Maybe Government
} deriving stock (Show, Generic)

data VotePhasePayload = VotePhasePayload {
  governmentPrevious :: Maybe Government,
  chancellorCandidate :: Int
} deriving stock (Show, Generic)

data PresidentDiscardPolicyPhasePayload = PresidentDiscardPolicyPhasePayload {
  chancellor :: Int
} deriving stock (Show, Generic)

data ChancellorDiscardPolicyPhasePayload = ChancellorDiscardPolicyPhasePayload {
  chancellor :: Int
} deriving stock (Show, Generic)

data GamePhase =
  NominateChancellorPhase NominateChancellorPhasePayload |
  VotePhase VotePhasePayload |
  PresidentDiscardPolicyPhase PresidentDiscardPolicyPhasePayload |
  ChancellorDiscardPolicyPhase ChancellorDiscardPolicyPhasePayload
  deriving stock (Show)

data PresidentTracker = PresidentTracker {
    president :: Int,
    regularPresidentLatest :: Int
} deriving stock (Show, Generic)

newPresidentTracker :: PresidentTracker
newPresidentTracker = PresidentTracker {
    president = 0,
    regularPresidentLatest = 0
  }

data Game = Game {
  phase :: GamePhase,
  -- players includes dead players too.
  -- Use getAlivePlayers instead of #players wherever possible.
  players :: Vector Player,
  -- The cardPile contains the drawPile and the currentHand
  cardPile :: [Policy],
  goodPolicies :: Int,
  evilPolicies :: Int,
  presidentTracker :: PresidentTracker,
  electionTracker :: Int
} deriving stock (Show, Generic)

getPresident :: Game -> Int
getPresident = view (#presidentTracker . #president)

getAlivePlayers :: Game -> IntMap Player
getAlivePlayers game =
  IntMap.fromDistinctAscList $
  Vector.toList $
  Vector.filter (view #alive . snd) $
  Vector.indexed $
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

newGame :: Vector Player -> [Policy] -> Game
newGame players drawPile = Game {
  phase = NominateChancellorPhase $ NominateChancellorPhasePayload Nothing,
  players,
  cardPile = drawPile,
  evilPolicies = 0,
  goodPolicies = 0,
  presidentTracker = newPresidentTracker,
  electionTracker = 0
}

generateRandomGame :: Int -> IO Game
generateRandomGame playerCount = do
  rngPlayers <- newStdGen
  rngDrawPile <- newStdGen
  let players = generateRandomPlayers playerCount rngPlayers
  let drawPile = generateRandomCardPile 6 11 rngDrawPile
  pure $ newGame players drawPile

generateRandomPlayers :: Int -> StdGen -> Vector Player
generateRandomPlayers playerCount rng =
  fst $ shuffle (generate playerCount currentPlayer) rng
  where
    currentPlayer :: Int -> Player
    currentPlayer = newPlayer . currentRole
    currentRole playerCount = case playerCount of
      1 -> EvilLeaderRole
      2 -> GoodRole
      3 -> GoodRole
      4 -> GoodRole
      5 -> EvilRole
      6 -> GoodRole
      7 -> EvilRole
      8 -> GoodRole
      9 -> EvilRole
      10 -> GoodRole
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

data ClientEvent =
  UserInput Int UserInput
  deriving stock (Show, Read)

data GameEvent =
  Error Text |
  SucceedVote |
  FailVote
  deriving stock (Show)

data UserInput =
  NominateChancellor Int |
  Vote Vote |
  DiscardPresidentPolicy Int |
  DiscardChancellorPolicy Int
  deriving stock (Show, Read)

updateChecked :: ClientEvent -> Game -> (Game, Maybe GameEvent)
updateChecked event@(UserInput actorId _) game
  | isPlayerAllowedToAct actorId game = update event game
  | otherwise = (game, Just $ Error $ "Player " <> Text.pack (show actorId) <> " is currently not allowed to act")
  where
  isPlayerAllowedToAct :: Int -> Game -> Bool
  isPlayerAllowedToAct actorId game@(Game { phase }) =
    case phase of
      NominateChancellorPhase {} -> actorId == getPresident game
      VotePhase {} -> True
      PresidentDiscardPolicyPhase {} -> actorId == getPresident game
      ChancellorDiscardPolicyPhase (ChancellorDiscardPolicyPhasePayload { chancellor }) -> actorId == chancellor

update :: ClientEvent -> Game -> (Game, Maybe GameEvent)
update (UserInput actorId userInput) =
  case userInput of
    NominateChancellor playerId -> nominateChancellor playerId
    Vote vote -> castVote actorId vote
    DiscardPresidentPolicy policyIndex -> discardPolicy policyIndex
    DiscardChancellorPolicy policyIndex -> discardPolicy policyIndex

withGameEvent :: Maybe GameEvent -> Game -> (Game, Maybe GameEvent)
withGameEvent = flip (,)

nominateChancellor :: Int -> Game -> (Game, Maybe GameEvent)
nominateChancellor playerId gameOld@(Game {
  phase = NominateChancellorPhase NominateChancellorPhasePayload { governmentPrevious }
}) =
  withGameEvent Nothing $
  set (#players . traversed . #vote) Nothing $
  set #phase (VotePhase $ VotePhasePayload {
    chancellorCandidate = playerId,
    governmentPrevious = governmentPrevious
  }) $
  gameOld
nominateChancellor _playerId gameOld =
  (gameOld, Just $ Error $ "Cannot nominate a chancellor outside of NominateChancellorPhase")

castVote :: Int -> Vote -> Game -> (Game, Maybe GameEvent)
castVote actorId vote gameOld@(Game {
  phase = VotePhase VotePhasePayload {
    governmentPrevious,
    chancellorCandidate
  }
}) =
  let gameNew = set (#players . ix actorId . #vote) (Just vote) gameOld in
  case voteResult gameNew of
    Nothing -> (gameNew, Nothing)
    Just Yes -> (succeedVote gameNew, Just SucceedVote)
    Just No -> (failVote gameNew, Just FailVote)
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
      set #phase (PresidentDiscardPolicyPhase $ PresidentDiscardPolicyPhasePayload {
          chancellor = chancellorCandidate
      })
    failVote :: Game -> Game
    failVote =
      set #phase (NominateChancellorPhase $ NominateChancellorPhasePayload {
        governmentPrevious = governmentPrevious
      })
      .
      nominateNextRegularPresident
      .
      advanceElectionTracker
castVote _actorId _vote gameOld =
  (gameOld, Just $ Error $ "Cannot vote outside of VotePhase")

advanceElectionTracker :: Game -> Game
advanceElectionTracker game@(Game { electionTracker }) =
  if electionTracker < 2
  then over #electionTracker (+1) game
  else enactTopPolicy game

nominateNextRegularPresident :: Game -> Game
nominateNextRegularPresident game =
  over #presidentTracker (nextRegularPresident (getAlivePlayers game)) game

nextRegularPresident :: IntMap value -> PresidentTracker -> PresidentTracker
nextRegularPresident players presidentTracker =
  let presidentialCandidate = presidentTracker ^. #regularPresidentLatest in
  passPresidencyTo $
  fromMaybe (error "all players dying should not be possible") $
  fst <$> (IntMap.lookupGT (presidentialCandidate) players <|> IntMap.lookupMin players)
  where
    passPresidencyTo :: Int -> PresidentTracker
    passPresidencyTo nextPresident =
      set #president nextPresident $
      set #regularPresidentLatest nextPresident $
      presidentTracker

discardPolicy :: Int -> Game -> (Game, Maybe GameEvent)
discardPolicy policyIndex gameOld =
  case removePolicy policyIndex gameOld of
    Left error -> (gameOld, Just $ Error $ error)
    Right gameNew@(Game { phase }) ->
      case phase of
        PresidentDiscardPolicyPhase (PresidentDiscardPolicyPhasePayload { chancellor }) ->
          withGameEvent Nothing $
          set #phase (ChancellorDiscardPolicyPhase $ ChancellorDiscardPolicyPhasePayload {
            chancellor = chancellor
          }) $
          gameNew
        ChancellorDiscardPolicyPhase {} ->
          withGameEvent Nothing $
          nominateNextRegularPresident $
          enactTopPolicy $
          gameNew
        _ -> (gameOld, Just $ Error $ "Cannot discard policy outside of PresidentDiscardPolicyPhase or ChancellorDiscardPolicyPhase")
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
  over (policyCounter policy) (+1) $
  set #cardPile cardPileTail $
  set #electionTracker 0 $
  gameOld
  where
    policyCounter :: Policy -> ASetter Game Game Int Int
    policyCounter policy =
      case policy of
        GoodPolicy -> #goodPolicies
        EvilPolicy -> #evilPolicies
enactTopPolicy _gameOld =
  error "Cannot enact top policy from empty card pile"
