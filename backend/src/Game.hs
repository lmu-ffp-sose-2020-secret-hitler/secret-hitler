{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Game (
  Game (..),
  Player (..),
  drawPile,
  currentHand,
  generateRandomGame,
  updateChecked,
  isPlayerAllowedToAct,
  isEligible,
  nominateChancellor,
  placeVote,
  discardPolicy,
  stopPeekingPolicies,
  executePlayer,
  proposeVeto,
  acceptVeto,
  rejectVeto,
) where

import Common.GameMessages
  (
    GameAction (..),
    GameEvent (..),
    GameOverReason (..),
    GamePhase (..),
    Government (..),
    PlayerId,
    Policy (..),
    Role (..),
  )
import Control.Applicative ((<|>))
import Control.Lens hiding (element)
import Control.Monad (mfilter)
import Data.Function (on)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (minimumBy)
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (Sum (Sum))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, generate)
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Random (Random, withStdGen)
import VectorShuffling.Immutable (shuffle)

----------------------------------------------------------------------------------------------------
--     ____
--    / ___|  __ _  _ __ ___    ___
--   | |  _  / _` || '_ ` _ \  / _ \
--   | |_| || (_| || | | | | ||  __/
--    \____| \__,_||_| |_| |_| \___|
--
----------------------------------------------------------------------------------------------------

data Game = Game {
  phase :: GamePhase,
  -- players includes dead players too.
  players :: Map PlayerId Player,
  -- The cardPile contains the drawPile and the currentHand
  cardPile :: [Policy],
  goodPolicyCount :: Int,
  evilPolicyCount :: Int,
  presidentId :: PlayerId,
  regularPresidentId :: PlayerId,
  electionTracker :: Int
} deriving stock (Generic)

drawPile :: Game -> [Policy]
drawPile game@(Game { cardPile }) = drop (currentHandSize game) (cardPile)

currentHand :: Game -> [Policy]
currentHand game@(Game { cardPile }) = take (currentHandSize game) (cardPile)

currentHandSize :: Num p => Game -> p
currentHandSize (Game { phase }) = case phase of
  PresidentDiscardPolicyPhase {} -> 3
  ChancellorDiscardPolicyPhase {} -> 2
  PolicyPeekPhase {} -> 3
  _ -> 0

newGame :: Map PlayerId Player -> [Policy] -> Game
newGame players drawPile =
  let presidentId = fst $ (minimumBy (compare `on` (view #turnOrder) . snd)) $ Map.toList players in
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

----------------------------------------------------------------------------------------------------
--    ____   _
--   |  _ \ | |  __ _  _   _   ___  _ __
--   | |_) || | / _` || | | | / _ \| '__|
--   |  __/ | || (_| || |_| ||  __/| |
--   |_|    |_| \__,_| \__, | \___||_|
--                     |___/
----------------------------------------------------------------------------------------------------

data Player = Player {
  name :: Text,
  turnOrder :: Int,
  role :: Role,
  vote :: Maybe Bool,
  alive :: Bool
} deriving stock (Generic)

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

----------------------------------------------------------------------------------------------------
--     ____                                 _
--    / ___|  ___  _ __    ___  _ __  __ _ | |_  ___
--   | |  _  / _ \| '_ \  / _ \| '__|/ _` || __|/ _ \
--   | |_| ||  __/| | | ||  __/| |  | (_| || |_|  __/
--    \____| \___||_| |_| \___||_|   \__,_| \__|\___|
--
----------------------------------------------------------------------------------------------------

generateRandomGame :: Map PlayerId Text -> Random Game
generateRandomGame playerNames = do
  players <- generateRandomPlayers playerNames
  drawPile <- generateRandomCardPile 6 11
  return $ newGame players drawPile
  where
    generateRandomPlayers :: Map PlayerId Text -> Random (Map PlayerId Player)
    generateRandomPlayers playerNames = do
      let playerCount = Map.size playerNames
      turnOrders <- generateRandomTurnOrders playerCount
      roles <- generateRandomRoles playerCount
      return $ Map.fromAscList $
        zipWith3 (\(id, name) turnOrder role -> (id, newPlayer name turnOrder role))
        (Map.toAscList playerNames) (Vector.toList turnOrders) (Vector.toList roles)
    generateRandomTurnOrders :: Int -> Random (Vector Int)
    generateRandomTurnOrders playerCount =
      withStdGen $ shuffle (generate playerCount id)
    generateRandomRoles :: Int -> Random (Vector Role)
    generateRandomRoles playerCount =
      withStdGen $ shuffle (generate playerCount currentRole)
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

generateRandomCardPile :: Int -> Int -> Random [Policy]
generateRandomCardPile goodPolicyCount evilPolicyCount =
  let
    goodPolicies = Vector.replicate goodPolicyCount GoodPolicy
    evilPolicies = Vector.replicate evilPolicyCount EvilPolicy
  in
  Vector.toList <$> (withStdGen $ shuffle (goodPolicies Vector.++ evilPolicies))

----------------------------------------------------------------------------------------------------
--    _   _             _         _
--   | | | | _ __    __| |  __ _ | |_  ___
--   | | | || '_ \  / _` | / _` || __|/ _ \
--   | |_| || |_) || (_| || (_| || |_|  __/
--    \___/ | .__/  \__,_| \__,_| \__|\___|
--          |_|
----------------------------------------------------------------------------------------------------

updateChecked :: PlayerId -> GameAction -> Game -> Random (Game, GameEvent)
updateChecked actorId action game
  | isPlayerAllowedToAct actorId game = update actorId action game
  | otherwise = return (game, InvalidGameAction $ "Player " <> Text.pack (show actorId) <> " is currently not allowed to act")

isPlayerAllowedToAct :: PlayerId -> Game -> Bool
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
    GameOverPhase {} -> False

update :: PlayerId -> GameAction -> Game -> Random (Game, GameEvent)
update actorId action = do
  case action of
    NominateChancellor playerId -> return . nominateChancellor playerId
    PlaceVote vote -> placeVote actorId vote
    PresidentDiscardPolicy policyIndex -> discardPolicy policyIndex
    ChancellorDiscardPolicy policyIndex -> discardPolicy policyIndex
    StopPeekingPolicies -> return . stopPeekingPolicies
    ExecutePlayer playerId -> return . executePlayer playerId
    ProposeVeto -> return . proposeVeto
    AcceptVeto -> acceptVeto
    RejectVeto -> return . rejectVeto

withGameEvent :: GameEvent -> Game -> (Game, GameEvent)
withGameEvent = flip (,)

----------------------------------------------------------------------------------------------------
--    _   _                    _           ____  _                               _  _
--   | \ | |  ___   _ __ ___  (_) _ __    / ___|| |__    __ _  _ __    ___  ___ | || |  ___   _ __
--   |  \| | / _ \ | '_ ` _ \ | || '_ \  | |    | '_ \  / _` || '_ \  / __|/ _ \| || | / _ \ | '__|
--   | |\  || (_) || | | | | || || | | | | |___ | | | || (_| || | | || (__|  __/| || || (_) || |
--   |_| \_| \___/ |_| |_| |_||_||_| |_|  \____||_| |_| \__,_||_| |_| \___|\___||_||_| \___/ |_|
--
----------------------------------------------------------------------------------------------------

nominateChancellor :: PlayerId -> Game -> (Game, GameEvent)
nominateChancellor chancellorCandidateId gameOld@(Game {
  presidentId,
  phase = NominateChancellorPhase { previousGovernment }
}) =
  if isEligible chancellorCandidateId gameOld
  then
    withGameEvent ChancellorNominated {
      presidentialCandidateId = presidentId,
      chancellorCandidateId
    } $
    gameOld
      & #players . traversed . #vote .~ Nothing
      & #phase .~ VotePhase { chancellorCandidateId, previousGovernment }
  else
    (gameOld, InvalidGameAction $ "Player " <> Text.pack (show chancellorCandidateId) <> " is not eligible")
  where
nominateChancellor _playerId gameOld =
  (gameOld, InvalidGameAction "Cannot nominate a chancellor outside of NominateChancellorPhase")

isEligible :: PlayerId -> Game -> Bool
isEligible chancellorCandidateId game@(Game {
  presidentId,
  phase = NominateChancellorPhase { previousGovernment }
})
  | chancellorCandidateId == presidentId = False
  | isNothing $ Map.lookup chancellorCandidateId (alivePlayers game) = False
  | Nothing <- previousGovernment = True
  | Just Government { presidentId, chancellorId } <- previousGovernment =
    if Map.size (alivePlayers game) <= 5
    then chancellorCandidateId /= chancellorId
    else chancellorCandidateId /= chancellorId && chancellorCandidateId /= presidentId
isEligible _chancellorCandidateId _game = False

----------------------------------------------------------------------------------------------------
--    ____   _                    __     __      _
--   |  _ \ | |  __ _   ___  ___  \ \   / /___  | |_  ___
--   | |_) || | / _` | / __|/ _ \  \ \ / // _ \ | __|/ _ \
--   |  __/ | || (_| || (__|  __/   \ V /| (_) || |_|  __/
--   |_|    |_| \__,_| \___|\___|    \_/  \___/  \__|\___|
--
----------------------------------------------------------------------------------------------------

placeVote :: PlayerId -> Bool -> Game -> Random (Game, GameEvent)
placeVote playerId vote gameOld@(Game {
  phase = VotePhase { previousGovernment, chancellorCandidateId }
}) =
  let gameNew = gameOld & (#players . ix playerId . #vote) .~ Just vote in
  case voteResult gameNew of
    Nothing -> return (gameNew, VotePlaced { playerId, vote })
    Just True -> return $ succeedVote gameNew
    Just False -> failVote gameNew
  where
    voteResult :: Game -> Maybe Bool
    voteResult game =
      (> Sum 0) <$>
      foldMap boolToSum <$>
      playerVoteResults game
    playerVoteResults :: Game -> Maybe (Map PlayerId Bool)
    playerVoteResults game = traverse (view #vote) (alivePlayers game)
    boolToSum :: Bool -> Sum Int
    boolToSum True = Sum 1
    boolToSum False = Sum (-1)
    succeedVote :: Game -> (Game, GameEvent)
    succeedVote game@(Game { presidentId }) =
      case game ^. #players . at chancellorCandidateId of
        Nothing -> (game, InvalidGameAction $ "Player " <> Text.pack (show chancellorCandidateId) <> " is not in this game")
        Just chancellor ->
          withGameEvent VoteSucceeded { presidentId, chancellorId = chancellorCandidateId } $
          game & #phase .~
            if game ^. #evilPolicyCount >= 3 && chancellor ^. #role == EvilLeaderRole
            then GameOverPhase { reason = EvilLeaderElected }
            else PresidentDiscardPolicyPhase { chancellorId = chancellorCandidateId }
    failVote :: Game -> Random (Game, GameEvent)
    failVote game@(Game { presidentId }) = do
      (game, gameOverReason, policyEnacted) <- advanceElectionTracker game
      let keepTermLimits = const $ isNothing policyEnacted
      return $
        withGameEvent (VoteFailed {
          presidentialCandidateId = presidentId,
          chancellorCandidateId,
          policyEnacted
        }) $
        case gameOverReason of
          Just reason -> game & #phase .~ GameOverPhase { reason }
          _ -> nominateNextRegularPresident (mfilter keepTermLimits previousGovernment) game
placeVote _playerId _vote gameOld =
  return (gameOld, InvalidGameAction "Cannot vote outside of VotePhase")

----------------------------------------------------------------------------------------------------
--    ____   _                            _
--   |  _ \ (_) ___   ___  __ _  _ __  __| |
--   | | | || |/ __| / __|/ _` || '__|/ _` |
--   | |_| || |\__ \| (__| (_| || |  | (_| |
--   |____/ |_||___/ \___|\__,_||_|   \__,_|
--
----------------------------------------------------------------------------------------------------

discardPolicy :: Int -> Game -> Random (Game, GameEvent)
discardPolicy policyIndex gameOld =
  case removePolicy policyIndex gameOld of
    Left error -> return (gameOld, InvalidGameAction $ error)
    Right gameNew -> updateGameAfterDiscard gameNew
  where
    removePolicy :: Int -> Game -> Either Text Game
    removePolicy policyIndex game =
      if 0 <= policyIndex && policyIndex < currentHandSize game
      then Right $ game & #cardPile %~ removeElement policyIndex
      else Left "Cannot discard policy outside of current hand"
    removeElement :: Int -> [a] -> [a]
    removeElement i list = take i list ++ drop (i + 1) list

    updateGameAfterDiscard :: Game -> Random (Game, GameEvent)
    updateGameAfterDiscard game@(Game {
      presidentId,
      phase = PresidentDiscardPolicyPhase { chancellorId }
    }) =
      return $ withGameEvent PresidentDiscardedPolicy { presidentId } $
      game & #phase .~ ChancellorDiscardPolicyPhase { chancellorId }
    updateGameAfterDiscard gameOld@(Game {
      phase = ChancellorDiscardPolicyPhase { chancellorId }
    }) = do
      (gameNew, gameOverReason, policyEnacted) <- enactTopPolicy gameOld
      return $
        withGameEvent (ChancellorEnactedPolicy { chancellorId, policy = policyEnacted }) $
        if| Just reason <- gameOverReason -> gameNew & #phase .~ GameOverPhase { reason }
          | EvilPolicy <- policyEnacted,
            Just gamePhase <- presidentialPowerPhase chancellorId gameNew ->
              gameNew & #phase .~ gamePhase
          | otherwise -> endElectedGovernmentWithTermLimits chancellorId gameNew
    updateGameAfterDiscard _game =
      return (gameOld, InvalidGameAction "Cannot discard policy outside of PresidentDiscardPolicyPhase or ChancellorDiscardPolicyPhase")

    presidentialPowerPhase :: PlayerId -> Game -> Maybe GamePhase
    presidentialPowerPhase chancellorId game =
      let
        playerCount = Map.size $ game ^. #players
        evilPolicyCount = game ^. #evilPolicyCount
      in
      if| playerCount <= 6 ->
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

----------------------------------------------------------------------------------------------------
--    ____                   _      _               _     ____
--   |  _ \  _ __  ___  ___ (_)  __| |  ___  _ __  | |_  |  _ \  ___ __      __ ___  _ __  ___
--   | |_) || '__|/ _ \/ __|| | / _` | / _ \| '_ \ | __| | |_) |/ _ \\ \ /\ / // _ \| '__|/ __|
--   |  __/ | |  |  __/\__ \| || (_| ||  __/| | | || |_  |  __/| (_) |\ V  V /|  __/| |   \__ \
--   |_|    |_|   \___||___/|_| \__,_| \___||_| |_| \__| |_|    \___/  \_/\_/  \___||_|   |___/
--
----------------------------------------------------------------------------------------------------

stopPeekingPolicies :: Game -> (Game, GameEvent)
stopPeekingPolicies game@(Game {
  presidentId,
  phase = PolicyPeekPhase { chancellorId }
}) =
  withGameEvent PresidentStoppedPeekingPolicies { presidentId } $
  endElectedGovernmentWithTermLimits chancellorId $
  game
stopPeekingPolicies game =
  (game, InvalidGameAction "Cannot stop peeking policies outside of PolicyPeekPhase")

executePlayer :: PlayerId -> Game -> (Game, GameEvent)
executePlayer playerId gameOld@(Game {
  presidentId,
  phase = ExecutionPhase { chancellorId }
}) =
  case gameOld ^. #players . at playerId of
    Nothing -> (gameOld, InvalidGameAction $ "Player " <> Text.pack (show playerId) <> " is not in this game")
    Just player ->
      let gameNew = gameOld & #players . ix playerId . #alive .~ False in
      withGameEvent PlayerKilled { presidentId, playerId } $
      case player ^. #role of
        EvilLeaderRole -> gameNew & #phase .~ GameOverPhase { reason = EvilLeaderKilled }
        _ -> endElectedGovernmentWithTermLimits chancellorId gameNew
executePlayer _playerId game =
  (game, InvalidGameAction "Cannot execute a player outside of ExecutionPhase")

----------------------------------------------------------------------------------------------------
--   __     __     _
--   \ \   / /___ | |_  ___
--    \ \ / // _ \| __|/ _ \
--     \ V /|  __/| |_| (_) |
--      \_/  \___| \__|\___/
--
----------------------------------------------------------------------------------------------------

proposeVeto :: Game -> (Game, GameEvent)
proposeVeto game@(Game {
  presidentId,
  phase = ChancellorDiscardPolicyPhase { chancellorId }
}) =
  if game ^. #evilPolicyCount == 5
  then
    withGameEvent VetoProposed { presidentId, chancellorId } $
    game & #phase .~ PendingVetoPhase { chancellorId }
  else
    (game, InvalidGameAction "Cannot propose veto when less than 5 evil policies are played")
proposeVeto game =
  (game, InvalidGameAction "Cannot propose veto outside of ChancellorDiscardPolicyPhase")

acceptVeto :: Game -> Random (Game, GameEvent)
acceptVeto game@(Game {
  presidentId,
  cardPile = _policy1 : _policy2 : cardPileTail,
  phase = PendingVetoPhase { chancellorId }
}) = do
  game <- pure $ game & #cardPile .~ cardPileTail
  (game, gameOverMaybe, policyEnacted) <- advanceElectionTracker game
  game <- shuffleDrawPileIfNeccessary game
  game <-
    if| Just reason <- gameOverMaybe -> pure $ game & #phase .~ GameOverPhase { reason }
      | isNothing policyEnacted -> pure $ endElectedGovernmentWithTermLimits chancellorId game
      | otherwise -> pure $ nominateNextRegularPresident Nothing game
  pure $ withGameEvent VetoAccepted { presidentId, chancellorId, policyEnacted } game
acceptVeto game =
  return (game, InvalidGameAction "Cannot accept veto outside of PendingVetoPhase")

rejectVeto :: Game -> (Game, GameEvent)
rejectVeto game@(Game {
  presidentId,
  phase = PendingVetoPhase { chancellorId }
}) =
  withGameEvent VetoRejected { presidentId, chancellorId } $
  game & #phase .~ ChancellorDiscardPolicyPhase { chancellorId }
rejectVeto game =
  (game, InvalidGameAction "Cannot reject veto outside of PendingVetoPhase")

----------------------------------------------------------------------------------------------------
--    _   _  _    _  _
--   | | | || |_ (_)| | ___
--   | | | || __|| || |/ __|
--   | |_| || |_ | || |\__ \
--    \___/  \__||_||_||___/
--
----------------------------------------------------------------------------------------------------

alivePlayers :: Game -> Map PlayerId Player
alivePlayers game =
  Map.filter (view #alive) $
  view #players $
  game

advanceElectionTracker :: Game -> Random (Game, Maybe GameOverReason, Maybe Policy)
advanceElectionTracker game@(Game { electionTracker }) =
  if electionTracker < 2
  then return (game & #electionTracker %~ (+1), Nothing, Nothing)
  else (fmapTripple Just) <$> enactTopPolicy game

fmapTripple f (a, b, c) = (a, b, f c)

enactTopPolicy :: Game -> Random (Game, Maybe GameOverReason, Policy)
enactTopPolicy game@(Game { cardPile = policy : cardPileTail }) =
  fmap (withPolicyEnacted policy) $
  shuffleDrawPileIfNeccessary $
  game
    & (policyCount policy) %~ (+1)
    & #cardPile .~ cardPileTail
    & #electionTracker .~ 0
  where
    policyCount :: Policy -> ASetter Game Game Int Int
    policyCount GoodPolicy = #goodPolicyCount
    policyCount EvilPolicy = #evilPolicyCount

    withPolicyEnacted :: Policy -> Game -> (Game, Maybe GameOverReason, Policy)
    withPolicyEnacted policy game =
      let
        gameOverReason =
          case policy of
            GoodPolicy | game ^. #goodPolicyCount >= 5 -> Just AllGoodPoliciesPlayed
            EvilPolicy | game ^. #evilPolicyCount >= 6 -> Just AllEvilPoliciesPlayed
            _ -> Nothing
      in
      (game, gameOverReason, policy)
enactTopPolicy _gameOld = error "Cannot enact top policy from empty card pile"

shuffleDrawPileIfNeccessary :: Game -> Random Game
shuffleDrawPileIfNeccessary game =
  if length (game ^. #cardPile) < 3
  then
    let
      goodPolicyCount = 6 - game ^. #goodPolicyCount
      evilPolicyCount = 11 - game ^. #evilPolicyCount
    in do
    drawPile <- generateRandomCardPile goodPolicyCount evilPolicyCount
    return $ game & #cardPile .~ drawPile
  else return game

endElectedGovernmentWithTermLimits :: PlayerId -> Game -> Game
endElectedGovernmentWithTermLimits chancellorId game =
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
    nextRegularPresidentId :: Game -> PlayerId
    nextRegularPresidentId game =
      let president = getPresident game
          alivePlayersList = Map.toList $ alivePlayers game in
      fst $
      fromMaybe (error "all players dying should not be possible") $
        (minimumMaybeOn snd $ filter ((president <) . snd) alivePlayersList)
        <|> (minimumMaybeOn snd alivePlayersList)
    getPresident :: Game -> Player
    getPresident game =
      let presidentIdOld = game ^. #presidentId in
      fromMaybe
        (error "president is not a player")
        (game ^. #players . at presidentIdOld)
    minimumMaybeOn :: Ord b => (a -> b) -> [a] -> Maybe a
    minimumMaybeOn _ [] = Nothing
    minimumMaybeOn f list = minimumMaybeBy (compare `on` f) list
    minimumMaybeBy :: (a -> a -> Ordering) -> [a] -> Maybe a
    minimumMaybeBy _cmp [] = Nothing
    minimumMaybeBy cmp list = Just $ minimumBy cmp list
