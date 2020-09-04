module Game where

import Control.Applicative ((<|>))
import Control.Lens hiding (element)
import Data.Bool (bool)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Maybe
import Data.Monoid
import GHC.Generics (Generic)
import System.Random
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap

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
  deriving stock (Show)

newtype PlayerId =
  PlayerId Int
  deriving newtype (Show, Eq, Ord)

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
  president :: PlayerId,
  chancellor :: PlayerId
} deriving stock (Show)

data NominateChancellorPhasePayload = NominateChancellorPhasePayload {
  governmentPrevious :: Maybe Government
} deriving stock (Show, Generic)

data VotePhasePayload = VotePhasePayload {
  governmentPrevious :: Maybe Government,
  chancellorCandidate :: PlayerId
} deriving stock (Show, Generic)

data PresidentDiscardPolicyPhasePayload = PresidentDiscardPolicyPhasePayload {
  chancellor :: PlayerId
} deriving stock (Show, Generic)

data ChancellorDiscardPolicyPhasePayload = ChancellorDiscardPolicyPhasePayload {
  chancellor :: PlayerId
} deriving stock (Show, Generic)

data GamePhase =
  NominateChancellorPhase NominateChancellorPhasePayload |
  VotePhase VotePhasePayload |
  PresidentDiscardPolicyPhase PresidentDiscardPolicyPhasePayload |
  ChancellorDiscardPolicyPhase ChancellorDiscardPolicyPhasePayload
  deriving stock (Show)

data PresidentTracker = PresidentTracker {
    president :: PlayerId,
    regularPresidentLatest :: PlayerId
} deriving stock (Show, Generic)

newPresidentTracker :: PresidentTracker
newPresidentTracker = PresidentTracker {
    president = PlayerId 0,
    regularPresidentLatest = PlayerId 0
  }

data Game = Game {
  phase :: GamePhase,
  -- players includes dead players too.
  -- Use the getter alivePlayers instead of #players wherever possible.
  players :: NEMap PlayerId Player,
  -- The cardPile contains the drawPile and the currentHand
  cardPile :: [Policy],
  goodPolicies :: Int,
  evilPolicies :: Int,
  presidentTracker :: PresidentTracker,
  electionTracker :: Int
} deriving stock (Show, Generic)

getPresident :: Game -> PlayerId
getPresident game = (presidentTracker game) ^. #president

alivePlayers :: Getter Game (Map PlayerId Player)
alivePlayers = #players . to (NEMap.filter (view #alive))

drawPile :: Game -> [Policy]
drawPile game = drop (currentHandSize game) (cardPile game)

currentHand :: Game -> [Policy]
currentHand game = take (currentHandSize game) (cardPile game)

currentHandSize :: Num p => Game -> p
currentHandSize game = case phase game of
  PresidentDiscardPolicyPhase {} -> 3
  ChancellorDiscardPolicyPhase {} -> 2
  _ -> 0

newGame :: NEMap PlayerId Player -> [Policy] -> Game
newGame players drawPile = Game {
  phase = NominateChancellorPhase $ NominateChancellorPhasePayload Nothing,
  players = players,
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
  let drawPile = generateRandomDrawPile 6 11 rngDrawPile
  return $ newGame players drawPile

generateRandomPlayers :: RandomGen rng => Int -> rng -> NEMap PlayerId Player
generateRandomPlayers playerCount rngOld =
  NEMap.mapKeysMonotonic PlayerId $ go playerCount rngOld
  where
    go playerCount rngOld =
      let player = newPlayer $ currentRole playerCount in
      if playerCount <= 1
      then NEMap.singleton 0 player
      else
        let (id, rngNew) = randomR (0, playerCount-1) rngOld in
        NEMap.insert id player $
        NEMap.mapKeysMonotonic (\k -> if k < id then k else k+1) $
        go (playerCount-1) rngNew
      where
        currentRole playerCount =
          case playerCount of
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

generateRandomDrawPile :: RandomGen rng => Int -> Int -> rng -> [Policy]
generateRandomDrawPile goodPolicyCount evilPolicyCount rngOld =
  let policyCount = goodPolicyCount + evilPolicyCount in
  if policyCount <= 0
  then []
  else
    let (r, rngNew) = randomR (1, policyCount) rngOld in
    if r <= goodPolicyCount
    then GoodPolicy : generateRandomDrawPile (goodPolicyCount-1) evilPolicyCount rngNew
    else EvilPolicy : generateRandomDrawPile goodPolicyCount (evilPolicyCount-1) rngNew

data ClientEvent =
  UserInput PlayerId UserInput

data GameEvent =
  SucceedVote |
  FailVote

data UserInput =
  NominateChancellor PlayerId |
  Vote Vote |
  DiscardPresidentPolicy Int |
  DiscardChancellorPolicy Int

updateChecked :: ClientEvent -> Game -> (Game, Maybe GameEvent)
updateChecked event@(UserInput actorId _) game =
  case isPlayerAllowedToAct actorId game of
    True -> update event game
    False -> error $ "Player " ++ show actorId ++ " is currently not allowed to act"
  where
  isPlayerAllowedToAct :: PlayerId -> Game -> Bool
  isPlayerAllowedToAct playerId game =
    case phase game of
      NominateChancellorPhase _ -> playerId == getPresident game
      VotePhase _ -> True
      PresidentDiscardPolicyPhase _ -> playerId == getPresident game
      ChancellorDiscardPolicyPhase ChancellorDiscardPolicyPhasePayload { chancellor } -> playerId == chancellor

update :: ClientEvent -> Game -> (Game, Maybe GameEvent)
update (UserInput actorId userInput) =
  case userInput of
    NominateChancellor playerId -> nominateChancellor playerId
    Vote vote -> castVote actorId vote
    DiscardPresidentPolicy policyIndex -> discardPolicy policyIndex
    DiscardChancellorPolicy policyIndex -> discardPolicy policyIndex

withGameEvent :: Maybe GameEvent -> Game -> (Game, Maybe GameEvent)
withGameEvent = flip (,)

nominateChancellor :: PlayerId -> Game -> (Game, Maybe GameEvent)
nominateChancellor playerId gameOld =
  case phase gameOld of
    NominateChancellorPhase payload ->
      withGameEvent Nothing $
      set (#players . traversed . #vote) Nothing gameOld {
        phase = VotePhase VotePhasePayload {
          chancellorCandidate = playerId,
          governmentPrevious = payload ^. #governmentPrevious
        }
      }
    _ -> error "Cannot nominate a chancellor outside of NominateChancellorPhase"

castVote :: PlayerId -> Vote -> Game -> (Game, Maybe GameEvent)
castVote actorId vote gameOld =
  case phase gameOld of
    VotePhase payload ->
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
        playerVoteResults :: Game -> Maybe (Map PlayerId Vote)
        playerVoteResults game = traverse (view #vote) (game ^. alivePlayers)
        voteToSum :: Vote -> Sum Integer
        voteToSum No = Sum (-1)
        voteToSum Yes = Sum 1
        succeedVote :: Game -> Game
        succeedVote =
          set #phase
            (
              PresidentDiscardPolicyPhase $
              PresidentDiscardPolicyPhasePayload {
                chancellor = payload ^. #chancellorCandidate
              }
            )
          .
          set #electionTracker 0
        failVote :: Game -> Game
        failVote =
          set #phase
            (
              NominateChancellorPhase $
              NominateChancellorPhasePayload {
                governmentPrevious = payload ^. #governmentPrevious
              }
            )
          .
          nominateNextPresident
          .
          advanceElectionTracker
    _ -> error "Cannot vote outside of VotePhase"

advanceElectionTracker :: Game -> Game
advanceElectionTracker game =
  if electionTracker game < 2
  then over #electionTracker (+1) game
  else enactTopPolicy game { electionTracker = 0 }

nominateNextPresident :: Game -> Game
nominateNextPresident game =
  over #presidentTracker (passPresidencyRegularly (game ^. alivePlayers)) game

passPresidencyRegularly ::
  Map PlayerId value -> PresidentTracker -> PresidentTracker
passPresidencyRegularly playerIds presidentTracker =
  passPresidencyTo $
  fromMaybe (error "all players dying should not be possible") $
  (
    fst <$> (Map.lookupGT (presidentTracker ^. #regularPresidentLatest) playerIds)
    <|>
    (fmap NonEmpty.head $ NonEmpty.nonEmpty $ Map.keys $ playerIds)
  )
  where
    passPresidencyTo :: PlayerId -> PresidentTracker
    passPresidencyTo nextPresident =
      set #president nextPresident $
      set #regularPresidentLatest nextPresident $
      presidentTracker

discardPolicy :: Int -> Game -> (Game, Maybe GameEvent)
discardPolicy policyIndex gameOld =
  let gameNew = removePolicy policyIndex gameOld in
  withGameEvent Nothing $
  case phase gameNew of
    PresidentDiscardPolicyPhase PresidentDiscardPolicyPhasePayload { chancellor } ->
      gameNew {
        phase = ChancellorDiscardPolicyPhase $ ChancellorDiscardPolicyPhasePayload {
          chancellor = chancellor
        }
      }
    ChancellorDiscardPolicyPhase _ ->
      nominateNextPresident $
      enactTopPolicy $
      gameNew
    _ -> error "Cannot discard policy outside of PresidentDiscardPolicyPhase or ChancellorDiscardPolicyPhase"
  where
    removePolicy policyIndex game =
      if 0 <= policyIndex && policyIndex < currentHandSize game
      then over #cardPile (removeElement policyIndex) game
      else error "Cannot discard policy outside of current hand"
    removeElement :: Int -> [a] -> [a]
    removeElement i list = take i list ++ drop (i + 1) list

enactTopPolicy :: Game -> Game
enactTopPolicy gameOld =
  case cardPile gameOld of
    (policy:cardPileTail) ->
      let gameNew = gameOld { cardPile = cardPileTail } in
      case policy of
        GoodPolicy -> over #goodPolicies (+1) gameNew
        EvilPolicy -> over #evilPolicies (+1) gameNew
    _ -> error "Cannot enact top policy from empty card pile"

type instance Index (NEMap key _value) = key
type instance IxValue (NEMap _key value) = value
instance Ord key => Ixed (NEMap key value) where
  ix key f m =
    case NEMap.lookup key m of
      Just value  -> (\valueNew -> NEMap.insert key valueNew m) <$> f value
      Nothing -> pure m
