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

data Game = Game {
  phase :: GamePhase,
  -- players includes dead players too.
  -- Use getAlivePlayers instead of #players wherever possible.
  players :: IntMap Player,
  -- The cardPile contains the drawPile and the currentHand
  cardPile :: [Policy],
  goodPolicyCount :: Int,
  evilPolicyCount :: Int,
  president :: Int,
  regularPresident :: Int,
  electionTracker :: Int
} deriving stock (Show, Generic)

getPresident :: Game -> Player
getPresident game =
  let presidentIdOld = game ^. #regularPresident in
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
  phase = NominateChancellorPhase $ NominateChancellorPhasePayload Nothing,
  players,
  cardPile = drawPile,
  evilPolicyCount = 0,
  goodPolicyCount = 0,
  president = 0,
  regularPresident = 0,
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
      NominateChancellorPhase {} -> actorId == game ^. #president
      VotePhase {} -> True
      PresidentDiscardPolicyPhase {} -> actorId == game ^. #president
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
nominateNextRegularPresident gameOld =
  let presidentIdNew = nextRegularPresidentId gameOld in
  gameOld
    & #regularPresident .~ presidentIdNew
    & #president .~ presidentIdNew
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
  over (policyCount policy) (+1) $
  set #cardPile cardPileTail $
  set #electionTracker 0 $
  gameOld
enactTopPolicy _gameOld =
  error "Cannot enact top policy from empty card pile"
