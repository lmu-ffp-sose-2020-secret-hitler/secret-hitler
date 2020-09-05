module Game where

import Control.Applicative ((<|>))
import Control.Lens hiding (element)
import Data.Bool (bool)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Random (RandomGen, randomR, newStdGen)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Text as Text

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

newtype PlayerId =
  PlayerId Int
  deriving newtype (Show, Eq, Ord, Read)

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
  -- Use getAlivePlayers instead of #players wherever possible.
  players :: NEMap PlayerId Player,
  -- The cardPile contains the drawPile and the currentHand
  cardPile :: [Policy],
  goodPolicies :: Int,
  evilPolicies :: Int,
  presidentTracker :: PresidentTracker,
  electionTracker :: Int
} deriving stock (Show, Generic)

getPresident :: Game -> PlayerId
getPresident = view (#presidentTracker . #president)

getAlivePlayers :: Game -> Map PlayerId Player
getAlivePlayers = NEMap.filter (view #alive) . view #players

drawPile :: Game -> [Policy]
drawPile game@(Game {cardPile}) = drop (currentHandSize game) (cardPile)

currentHand :: Game -> [Policy]
currentHand game@(Game {cardPile}) = take (currentHandSize game) (cardPile)

currentHandSize :: Num p => Game -> p
currentHandSize (Game {phase = PresidentDiscardPolicyPhase {}}) = 3
currentHandSize (Game {phase = ChancellorDiscardPolicyPhase {}}) = 2
currentHandSize _game = 0

newGame :: NEMap PlayerId Player -> [Policy] -> Game
newGame players cardPile = Game {
  phase = NominateChancellorPhase (NominateChancellorPhasePayload Nothing),
  players,
  cardPile,
  evilPolicies = 0,
  goodPolicies = 0,
  presidentTracker = newPresidentTracker,
  electionTracker = 0
}

generateRandomGame :: Int -> IO Game
generateRandomGame playerCount = do
  rngPlayers <- newStdGen
  rngCardPile <- newStdGen
  let players = generateRandomPlayers playerCount rngPlayers
  let cardPile = generateRandomCardPile 6 11 rngCardPile
  pure $ newGame players cardPile

generateRandomPlayers :: RandomGen rng => Int -> rng -> NEMap PlayerId Player
generateRandomPlayers playerCount rngOld =
  NEMap.mapKeysMonotonic PlayerId $ go playerCount rngOld
  where
    go playerCount rngOld =
      let player = newPlayer currentRole
      in
        if playerCount <= 1
        then NEMap.singleton 0 player
        else
          let (id, rngNew) = randomR (0, playerCount-1) rngOld
          in
            NEMap.insert id player $
            NEMap.mapKeysMonotonic (\k -> if k < id then k else k+1) $
            go (playerCount-1) rngNew
      where
        currentRole =
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

generateRandomCardPile :: RandomGen rng => Int -> Int -> rng -> [Policy]
generateRandomCardPile goodPolicyCount evilPolicyCount rngOld
  | policyCount <= 0 = []
  | r <= goodPolicyCount =
    GoodPolicy : generateRandomCardPile (goodPolicyCount-1) evilPolicyCount rngNew
  | otherwise =
    EvilPolicy : generateRandomCardPile goodPolicyCount (evilPolicyCount-1) rngNew
  where
    policyCount = goodPolicyCount + evilPolicyCount
    (r, rngNew) = randomR (1, policyCount) rngOld

data ClientEvent =
  UserInput PlayerId UserInput
  deriving stock (Show, Read)

data GameEvent =
  Error Text |
  SucceedVote |
  FailVote
  deriving stock (Show)

data UserInput =
  NominateChancellor PlayerId |
  Vote Vote |
  DiscardPresidentPolicy Int |
  DiscardChancellorPolicy Int
  deriving stock (Show, Read)

updateChecked :: ClientEvent -> Game -> (Game, Maybe GameEvent)
updateChecked event@(UserInput actorId _) game@(Game {phase})
  | isPlayerAllowedToAct = update event game
  | otherwise = (game, Just $ Error $ "Player " <> Text.pack (show actorId) <> " is currently not allowed to act")
  where
  isPlayerAllowedToAct :: Bool
  isPlayerAllowedToAct =
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

nominateChancellor :: PlayerId -> Game -> (Game, Maybe GameEvent)
nominateChancellor
  playerId
  gameOld@(
    Game
      {
        phase =
          NominateChancellorPhase
            (NominateChancellorPhasePayload {governmentPrevious})
      }
  )
  =
    (, Nothing) $
    set (#players . traversed . #vote) Nothing $
    set #phase (
      VotePhase $
      VotePhasePayload {
        chancellorCandidate = playerId,
        governmentPrevious
      }
    ) $
    gameOld
nominateChancellor _playerId gameOld =
  (gameOld, Just $ Error $ "Cannot nominate a chancellor outside of NominateChancellorPhase")

castVote :: PlayerId -> Vote -> Game -> (Game, Maybe GameEvent)
castVote actorId vote gameOld@(Game {phase = VotePhase payload})=
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
    playerVoteResults game = traverse (view #vote) (getAlivePlayers game)
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
      nominateNextRegularPresident
      .
      advanceElectionTracker
castVote _actorId _vote gameOld =
  (gameOld, Just $ Error $ "Cannot vote outside of VotePhase")

advanceElectionTracker :: Game -> Game
advanceElectionTracker game@(Game {electionTracker})
  | electionTracker < 2 = over #electionTracker (+1) game
  | otherwise =
    enactTopPolicy $
    set #electionTracker 0 $
    game

nominateNextRegularPresident :: Game -> Game
nominateNextRegularPresident game =
  over #presidentTracker (nextRegularPresident (getAlivePlayers game)) game

nextRegularPresident ::
  Map PlayerId value -> PresidentTracker -> PresidentTracker
nextRegularPresident playerIds presidentTracker =
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
discardPolicy policyIndex gameOld
  | Left error <- gameNewEither = (gameOld, Just $ Error $ error)
  | Right gameNew@(Game {phase}) <- gameNewEither =
    case phase of
      PresidentDiscardPolicyPhase (PresidentDiscardPolicyPhasePayload { chancellor }) ->
        (, Nothing) $
        set #phase (
          ChancellorDiscardPolicyPhase $
          ChancellorDiscardPolicyPhasePayload {
            chancellor = chancellor
          }
        ) $
        gameNew
      ChancellorDiscardPolicyPhase {} ->
        (, Nothing) $
        nominateNextRegularPresident $
        enactTopPolicy $
        gameNew
      _ -> (gameOld, Just $ Error $ "Cannot discard policy outside of PresidentDiscardPolicyPhase or ChancellorDiscardPolicyPhase")
  where
    gameNewEither :: Either Text Game
    gameNewEither
      | 0 <= policyIndex && policyIndex < currentHandSize gameOld =
        Right $ over #cardPile (removeElement policyIndex) gameOld
      | otherwise = Left "Cannot discard policy outside of current hand"
    removeElement :: Int -> [a] -> [a]
    removeElement i list = take i list ++ drop (i + 1) list

enactTopPolicy :: Game -> Game
enactTopPolicy gameOld@(Game {cardPile = policy : cardPileTail}) =
  over policyCountField (+1) $
  set #cardPile cardPileTail $
  gameOld
  where
    policyCountField
      | GoodPolicy <- policy = #goodPolicies
      | EvilPolicy <- policy = #evilPolicies
enactTopPolicy _gameOld = error "Cannot enact top policy from empty card pile"

type instance Index (NEMap key _value) = key
type instance IxValue (NEMap _key value) = value
instance Ord key => Ixed (NEMap key value) where
  ix key f m =
    case NEMap.lookup key m of
      Just value  -> (\valueNew -> NEMap.insert key valueNew m) <$> f value
      Nothing -> pure m
