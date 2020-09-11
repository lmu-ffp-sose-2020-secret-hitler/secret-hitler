{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Control.Applicative ((<|>))
import Control.Lens hiding (element)
import Data.Bool (bool)
import Data.Function (on)
import Data.Generics.Labels ()
import Data.List (minimumBy)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe (fromMaybe, isNothing)
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
  presidentId :: Int,
  chancellorId :: Int
} deriving stock (Show)

data GamePhase =
  NominateChancellorPhase {
    previousGovernment :: Maybe Government
  } |
  VotePhase {
    previousGovernment :: Maybe Government,
    chancellorCandidateId :: Int
  } |
  PresidentDiscardPolicyPhase {
    chancellorId :: Int
  } |
  ChancellorDiscardPolicyPhase {
    chancellorId :: Int
  } |
  PolicyPeekPhase {
    chancellorId :: Int
  } |
  ExecutionPhase {
    chancellorId :: Int
  } |
  PendingVetoPhase {
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
  let presidentIdOld = game ^. #presidentId in
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
newGame players drawPile =
  let presidentId = fst $ (minimumBy (compare `on` turnOrder . snd)) $ IntMap.toList players in
  Game {
    phase = NominateChancellorPhase Nothing,
    players,
    cardPile = drawPile,
    evilPolicyCount = 0,
    goodPolicyCount = 0,
    presidentId,
    regularPresidentId = presidentId,
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
  ChancellorDiscardPolicy Int |
  StopPeekingPolicies |
  ExecutePlayer Int |
  ProposeVeto |
  AcceptVeto |
  RejectVeto
  deriving stock (Show, Read)

data GameEvent =
  ChancellorNominated |
  VotePlaced |
  VoteSucceeded |
  VoteFailed |
  PresidentDiscardedPolicy |
  ChancellorDiscardedPolicy |
  PlayerKilled |
  VetoProposed |
  VetoAccepted |
  VetoRejected |
  Error Text
  deriving stock (Show)

updateChecked :: PlayerAction -> Game -> (Game, GameEvent)
updateChecked event@(GameAction actorId _) game
  | isPlayerAllowedToAct actorId game = update event game
  | otherwise = (game, Error $ "Player " <> Text.pack (show actorId) <> " is currently not allowed to act")
  where
  isPlayerAllowedToAct :: Int -> Game -> Bool
  isPlayerAllowedToAct actorId game@(Game { phase }) =
    let actorIsPresident = actorId == game ^. #presidentId in
    case phase of
      NominateChancellorPhase {} -> actorIsPresident
      VotePhase {} -> True
      PresidentDiscardPolicyPhase {} -> actorIsPresident
      ChancellorDiscardPolicyPhase { chancellorId } -> actorId == chancellorId
      PolicyPeekPhase {} -> actorIsPresident
      ExecutionPhase {} -> actorIsPresident
      PendingVetoPhase {} -> actorIsPresident

update :: PlayerAction -> Game -> (Game, GameEvent)
update (GameAction actorId userInput) =
  case userInput of
    NominateChancellor playerId -> nominateChancellor playerId
    PlaceVote vote -> placeVote actorId vote
    PresidentDiscardPolicy policyIndex -> discardPolicy policyIndex
    ChancellorDiscardPolicy policyIndex -> discardPolicy policyIndex
    StopPeekingPolicies -> stopPeekingPolicies
    ExecutePlayer playerId -> executePlayer playerId
    ProposeVeto -> proposeVeto
    AcceptVeto -> acceptVeto
    RejectVeto -> rejectVeto

withGameEvent :: GameEvent -> Game -> (Game, GameEvent)
withGameEvent = flip (,)

nominateChancellor :: Int -> Game -> (Game, GameEvent)
nominateChancellor chancellorCandidateId gameOld@(Game {
  phase = NominateChancellorPhase { previousGovernment }
}) =
  if isEligible chancellorCandidateId previousGovernment $ getAlivePlayers gameOld
  then
    withGameEvent ChancellorNominated $
    gameOld
      & #players . traversed . #vote .~ Nothing
      & #phase .~ VotePhase { chancellorCandidateId, previousGovernment }
  else
    (gameOld, Error $ "Player " <> Text.pack (show chancellorCandidateId) <> " is not eligible")
nominateChancellor _playerId gameOld =
  (gameOld, Error "Cannot nominate a chancellor outside of NominateChancellorPhase")

isEligible :: Int -> Maybe Government -> IntMap Player -> Bool
isEligible chancellorCandidateId previousGovernment alivePlayers =
  if isNothing $ IntMap.lookup chancellorCandidateId alivePlayers
  then False
  else
    case previousGovernment of
      Nothing -> True
      Just Government { presidentId, chancellorId } ->
        if IntMap.size alivePlayers <= 5
        then chancellorCandidateId /= chancellorId
        else chancellorCandidateId /= chancellorId && chancellorCandidateId /= presidentId

placeVote :: Int -> Vote -> Game -> (Game, GameEvent)
placeVote actorId vote gameOld@(Game {
  phase = VotePhase { previousGovernment, chancellorCandidateId }
}) =
  let gameNew = gameOld & (#players . ix actorId . #vote) .~ Just vote in
  case voteResult gameNew of
    Nothing -> (gameNew, VotePlaced)
    Just Yes -> (succeedVote gameNew, VoteSucceeded)
    Just No -> (failVote gameNew, VoteFailed)
  where
    voteResult :: Game -> Maybe Vote
    voteResult game =
      bool No Yes <$>
      (> Sum 0) <$>
      foldMap voteToSum <$>
      playerVoteResults game
    playerVoteResults :: Game -> Maybe (IntMap Vote)
    playerVoteResults game = traverse (view #vote) (getAlivePlayers game)
    voteToSum :: Vote -> Sum Integer
    voteToSum No = Sum (-1)
    voteToSum Yes = Sum 1
    succeedVote :: Game -> Game
    succeedVote game =
      game & #phase .~ PresidentDiscardPolicyPhase { chancellorId = chancellorCandidateId }
    failVote :: Game -> Game
    failVote game =
      nominateNextRegularPresident previousGovernment $
      advanceElectionTracker $
      game
placeVote _actorId _vote gameOld =
  (gameOld, Error "Cannot vote outside of VotePhase")

discardPolicy :: Int -> Game -> (Game, GameEvent)
discardPolicy policyIndex gameOld =
  case removePolicy policyIndex gameOld of
    Left error -> (gameOld, Error $ error)
    Right gameNew -> updateGameAfterDiscard gameNew
  where
    removePolicy :: Int -> Game -> Either Text Game
    removePolicy policyIndex game =
      if 0 <= policyIndex && policyIndex < currentHandSize game
      then Right $ game & #cardPile %~ removeElement policyIndex
      else Left "Cannot discard policy outside of current hand"
    removeElement :: Int -> [a] -> [a]
    removeElement i list = take i list ++ drop (i + 1) list

    updateGameAfterDiscard :: Game -> (Game, GameEvent)
    updateGameAfterDiscard game@(Game {
      phase = PresidentDiscardPolicyPhase { chancellorId }
    }) =
      withGameEvent PresidentDiscardedPolicy $
      game & #phase .~ ChancellorDiscardPolicyPhase { chancellorId }
    updateGameAfterDiscard gameOld@(Game {
      phase = ChancellorDiscardPolicyPhase { chancellorId }
    }) =
      let (gameNew, policy) = enactTopPolicy gameOld in
      withGameEvent ChancellorDiscardedPolicy $
      case policy of
        GoodPolicy ->
          endGovernment chancellorId gameNew
        EvilPolicy ->
          case presidentialPowerPhase chancellorId gameNew of
            Just gamePhase -> gameNew & #phase .~ gamePhase
            Nothing -> endGovernment chancellorId gameNew
    updateGameAfterDiscard _game =
      (gameOld, Error "Cannot discard policy outside of PresidentDiscardPolicyPhase or ChancellorDiscardPolicyPhase")

presidentialPowerPhase :: Int -> Game -> Maybe GamePhase
presidentialPowerPhase chancellorId game =
  let
    playerCount = IntMap.size $ game ^. #players
    evilPolicyCount = game ^. #evilPolicyCount
  in
  if | playerCount <= 6 ->
        case evilPolicyCount of
          3 -> Just $ PolicyPeekPhase chancellorId
          4 -> Just $ ExecutionPhase chancellorId
          5 -> Just $ ExecutionPhase chancellorId
          _ -> Nothing
     | playerCount <= 8 ->
        case evilPolicyCount of
          _ -> Nothing
     | playerCount <= 8 ->
        case evilPolicyCount of
          _ -> Nothing
     | otherwise -> Nothing

enactTopPolicy :: Game -> (Game, Policy)
enactTopPolicy game@(Game { cardPile = policy : cardPileTail }) =
  (, policy) $
  game
    & (policyCount policy) %~ (+1)
    & #cardPile .~ cardPileTail
    & #electionTracker .~ 0
enactTopPolicy _gameOld = error "Cannot enact top policy from empty card pile"

stopPeekingPolicies :: Game -> (Game, GameEvent)
stopPeekingPolicies game@(Game {
  phase = PolicyPeekPhase { chancellorId }
}) =
  withGameEvent ChancellorDiscardedPolicy $
  endGovernment chancellorId $
  game
stopPeekingPolicies game =
  (game, Error "Cannot stop peeking policies outside of PolicyPeekPhase")

executePlayer :: Int -> Game -> (Game, GameEvent)
executePlayer playerId game@(Game {
  phase = ExecutionPhase { chancellorId }
}) =
  withGameEvent PlayerKilled $
  endGovernment chancellorId $
  game & #players . ix playerId . #alive .~ False
executePlayer _playerId game =
  (game, Error "Cannot execute a player outside of ExecutionPhase")

proposeVeto :: Game -> (Game, GameEvent)
proposeVeto game@(Game {
  phase = ChancellorDiscardPolicyPhase { chancellorId }
}) =
  if game ^. #evilPolicyCount == 5
  then
    withGameEvent VetoProposed $
    game & #phase .~ PendingVetoPhase { chancellorId }
  else
    (game, Error "Cannot propose veto when less than 5 evil policies are played")
proposeVeto game =
  (game, Error "Cannot propose veto outside of ChancellorDiscardPolicyPhase")

acceptVeto :: Game -> (Game, GameEvent)
acceptVeto game@(Game {
  cardPile = _policy1 : _policy2 : cardPileTail,
  phase = PendingVetoPhase { chancellorId }
}) =
  withGameEvent VetoAccepted $
  advanceElectionTracker $
  endGovernment chancellorId $
  game & #cardPile .~ cardPileTail
acceptVeto game =
  (game, Error "Cannot accept veto outside of PendingVetoPhase")

rejectVeto :: Game -> (Game, GameEvent)
rejectVeto game@(Game {
  phase = PendingVetoPhase { chancellorId }
}) =
  withGameEvent VetoRejected $
  game & #phase .~ ChancellorDiscardPolicyPhase { chancellorId }
rejectVeto game =
  (game, Error "Cannot reject veto outside of PendingVetoPhase")

advanceElectionTracker :: Game -> Game
advanceElectionTracker game@(Game { electionTracker }) =
  if electionTracker < 2
  then game & #electionTracker %~ (+1)
  else throwIntoChaos game
  where
    throwIntoChaos game = fst $ enactTopPolicy game -- TODO reset eligibility

endGovernment :: Int -> Game -> Game
endGovernment chancellorId game =
  let government = Government {
    presidentId = game ^. #presidentId,
    chancellorId
  } in
  nominateNextRegularPresident (Just government) game

nominateNextRegularPresident :: Maybe Government -> Game -> Game
nominateNextRegularPresident previousGovernment gameOld =
  let presidentIdNew = nextRegularPresidentId gameOld in
  gameOld
    & #phase .~ NominateChancellorPhase { previousGovernment }
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
