{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language RecursiveDo #-}
{-# language RankNTypes #-}

module Frontend where

import Common.GameMessages
import Data.Foldable (for_)
import Data.Traversable (for)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromMaybe)
import GHC.TypeLits (Symbol)
import Data.List.NonEmpty (nonEmpty)
import Data.List (sortOn)
import qualified Data.IntMap.Strict as IM
import Data.Bool (bool)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
-- import qualified Data.ByteString as B
import Text.URI
import Control.Lens
import qualified Data.Aeson as A
import Common.MessageTypes
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Compose (Compose (getCompose))
import Data.Generics.Labels ()

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Reflex.Dom.Core hiding (button)
import Common.Route

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    {
      _frontend_head =
        do
          el "title" $ text "Obelisk Minimal Example"
          elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank,
      _frontend_body =
        do
          -- prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

          -- el "div" $ do
          --   exampleConfig <- getConfig "common/example"
          --   case exampleConfig of
          --     Nothing -> text "No config file found in config/common/example"
          --     Just s -> text $ T.E.decodeUtf8 s
          baseUri <- (fmap . fmap) T.E.decodeUtf8 (getConfig "common/route")
          application baseUri
    }

lobbyWidget ::
  (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m) =>
  Dynamic t LobbyView -> m (Event t LobbyAction)
lobbyWidget lobbyView =
  do
    elId "div" "player_list_lobby" $ void $
      simpleList (view #playerNames <$> lobbyView) (el "div" . dynText)
    nameElement <- inputElement $ def
    startGame <- (StartGame <$) <$> button "Start Game"
    pure $
      leftmost [
        startGame,
        Join <$> (updated $ value nameElement)
      ]

gameWidget ::
  forall t m.
  (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m) =>
  Dynamic t GameUpdate ->  m (Event t ActionFromClient)
gameWidget gameUpdate =
  elId "div" "board" $ do
    playerSelect <- fmap (GameAction <$>) $ playerList gameView
    imgStyle @"draw_pile.png" "grid-area: draw_pile" blank
    imgStyle @"board_fascist_5_6.png" "grid-area: board_fascist" blank
    elId "div" "policies_fascist" $
      dyn_ ((policyTiles @"policy_fascist.png" . view #evilPolicyCount) <$> gameView)
    imgStyle @"board_liberal.png" "grid-area: board_liberal" blank
    elId "div" "policies_liberal" $
      dyn_ ((policyTiles @"policy_liberal.png" . view #goodPolicyCount) <$> gameView)
    elId "div" "election_tracker" $
      elDynAttr
        "img"
        (
          fmap
            (\i -> "src" =: static @"circle.svg" <> "style" =: gridArea (1 + 2 * i) 1) $
          fmap (view #electionTracker) $
          gameView
        )
        blank
    elDynAttr
      "img"
      (
        fmap
          (\role ->
            "id" =: "identity" <>
            "src" =: case role of
              GoodRole -> static @"role_liberal.png"
              EvilRole -> static @"role_fascist.png"
              EvilLeaderRole -> static @"role_hitler.png"
          ) $
        fmap (view #playerRole) $
        gameView
      )
      blank
    imgStyle @"discard_pile.png" "grid-area: discard_pile" blank
    elId "div" "event" $ do
      dyn_ $ gameUpdateEventText <$> gameUpdate
    phaseDependentAction <- elId "div" "phase" $
      -- fmap switchDyn $
      -- widgetHold
      --   nominateChancellorPhaseWidget
      --   (
      --     updated $
      --     fmap phaseDependentWidget $
      --     gameView
      --   )
      switchHold never =<<
      dyn
        (
          phaseDependentWidget
          <$>
          gameView
        )
    -- display =<< (holdDyn StopPeekingPolicies phaseDependentAction)
    pure $ leftmost [playerSelect, phaseDependentAction]
  where
    gameView :: Dynamic t GameView
    gameView = view #gameView <$> gameUpdate

gameUpdateEventText :: DomBuilder t m => GameUpdate -> m ()
gameUpdateEventText gameUpdate =
  let
    game = gameUpdate ^. #gameView
    myId = game ^. #playerId
    player :: Int -> Maybe PlayerView
    player playerId = game ^. #players . at playerId
    playerName :: Int -> Text
    playerName playerId = fromMaybe "" ((view #name) <$> player playerId)
  in
  case gameUpdate ^. #gameEvent of
    Nothing -> blank
    Just ChancellorNominated { presidentialCandidateId, chancellorCandidateId } -> do
      text $ "Presidential Candidate " <> playerName presidentialCandidateId
        <> " nominated " <> playerName chancellorCandidateId 
        <> " for the office of chancellor."
      el "br" blank
      text "Do you support the proposed government?"
    Just VotePlaced { playerId, vote } | playerId == myId ->
      text $ "You voted " <> (if vote then "for" else "against")
        <> " the proposed government, but you can still change your mind."
    Just VotePlaced {} -> blank
    Just VoteSucceeded { presidentId, chancellorId } ->
      text $ playerName presidentId <> " and " <> playerName chancellorId
        <> " were as President and Chancellor."
    Just VoteFailed { presidentialCandidateId, chancellorCandidateId, policyEnacted } -> do
      text $ playerName presidentialCandidateId <> " and " <> playerName chancellorCandidateId
        <> " were not elected as President and Chancellor."
      chaosText policyEnacted
    Just PresidentDiscardedPolicy { presidentId } ->
      text $ "President " <> playerName presidentId <> " discarded a policy."
    Just ChancellorEnactedPolicy { chancellorId, policy } ->
      text $ "Chancellor " <> playerName chancellorId <> " enacted a " <> policyText policy <> "."
    Just PresidentStoppedPeekingPolicies { presidentId } ->
      text $ "President " <> playerName presidentId <> " took a peek at the next policies."
    Just PlayerKilled { presidentId, playerId } ->
      text $ "President " <> playerName presidentId <> " executed " <> playerName playerId <> "."
    Just VetoProposed { chancellorId, presidentId } ->
      text $ "Chancellor " <> playerName chancellorId <> " proposed a veto to President "
        <> playerName presidentId <> "."
    Just VetoAccepted { presidentId, chancellorId, policyEnacted } -> do
      text $ "President " <> playerName presidentId <> " accepted the veto proposed by Chancellor "
        <> playerName chancellorId <> "."
      chaosText policyEnacted
    Just VetoRejected { presidentId, chancellorId } ->
      text $ "President " <> playerName presidentId <> " rejected the veto proposed by Chancellor "
        <> playerName chancellorId <> "."
    Just InvalidGameAction { message } -> text $ "Invalid action: " <> message
  where
    policyText :: Policy -> Text
    policyText GoodPolicy = "liberal policy"
    policyText EvilPolicy = "fascist policy"
    chaosText :: DomBuilder t m => Maybe Policy -> m ()
    chaosText policyEnacted = 
      case policyEnacted of
        Nothing -> pure ()
        Just policy -> do
          el "br" blank
          text $ "Because of this the country was thrown into chaos, resulting in a "
            <> policyText policy <> "."

phaseDependentWidget ::
  (PostBuild t m, DomBuilder t m, MonadHold t m) =>
  GameView -> m (Event t ActionFromClient)
phaseDependentWidget
  (GameView {phase, playerId, presidentId, currentHand, vetoUnlocked})
  = case phase of
    NominateChancellorPhase {}
      | playerId == presidentId -> nominateChancellorPhaseWidget
    VotePhase {} -> votePhaseWidget
    PresidentDiscardPolicyPhase {}
      | length currentHand >= 3 ->
        discardPolicyPhaseWidget currentHand PresidentDiscardPolicy (pure never)
    ChancellorDiscardPolicyPhase {}
      | length currentHand >= 2 ->
        discardPolicyPhaseWidget currentHand ChancellorDiscardPolicy $
          if vetoUnlocked
          then
            (GameAction ProposeVeto <$) <$> button "I wish to veto this agenda"
          else pure never
    PolicyPeekPhase {}
      | length currentHand >= 3 ->
        policyPeekPhaseWidget currentHand
    ExecutionPhase {}
      | playerId == presidentId -> executionPhaseWidget
    PendingVetoPhase {}
      | playerId == presidentId ->
        fmap leftmost $
        sequenceA $
        [
          (GameAction AcceptVeto <$) <$> button "I agree to the veto",
          (GameAction RejectVeto <$) <$> button "nope"
        ]
    GameOverPhase { reason } ->
      gameOverPhaseWidget reason
    _ -> pure never

nominateChancellorPhaseWidget :: DomBuilder t m => m (Event t ActionFromClient)
nominateChancellorPhaseWidget = do
  text "Please nominate a chancellor by clicking their name."
  pure never

votePhaseWidget :: DomBuilder t m => m (Event t ActionFromClient)
votePhaseWidget = do
  elId "div" "vote_phase" $ do
    (yesButton, _) <- elAttr' "img" ("src" =: static @"ja_ballot.png") blank
    (noButton, _) <- elAttr' "img" ("src" =: static @"nein_ballot.png") blank
    return $
      leftmost [
        GameAction (PlaceVote True) <$ domEvent Click yesButton,
        GameAction (PlaceVote False) <$ domEvent Click noButton
      ]

discardPolicyPhaseWidget ::
  DomBuilder t m => [Policy] -> (Int -> GameAction) -> m (Event t ActionFromClient) -> m (Event t ActionFromClient)
discardPolicyPhaseWidget currentHand makeGameAction vetoWidget =
  elId "div" "policy_phase" $
    fmap leftmost $
    sequenceA $
    [
      (fmap . fmap) (GameAction . makeGameAction) $
      fmap leftmost $
      for
        (zip [0..] currentHand)
        (\(i, policy) ->
          (fmap . fmap) (const i) $
          fmap (domEvent Click) $
          fmap fst $
          elAttr'
            "img"
            ("src" =: case policy of
              GoodPolicy -> static @"policy_liberal.png"
              EvilPolicy -> static @"policy_fascist.png"
            )
            blank
        )
      ,
      vetoWidget
    ]

policyPeekPhaseWidget :: DomBuilder t m => [Policy] -> m (Event t ActionFromClient)
policyPeekPhaseWidget currentHand =
  elId "div" "policy_phase" $ do
    for_ currentHand $ \policy ->
      elAttr' "img" ("src" =: case policy of
          GoodPolicy -> static @"policy_liberal.png"
          EvilPolicy -> static @"policy_fascist.png"
      ) blank
    stopEvent <- button "Return policy tiles"
    return $ (GameAction StopPeekingPolicies) <$ stopEvent

executionPhaseWidget :: DomBuilder t m => m (Event t ActionFromClient)
executionPhaseWidget = do
  text "Please select a player to be executed."
  return never

gameOverPhaseWidget :: DomBuilder t m => GameOverReason -> m (Event t ActionFromClient)
gameOverPhaseWidget reason = do
  fmap snd $ el' "div" $ do
    text $ case reason of
      AllEvilPoliciesPlayed -> "Fascists won by enacting enough policies!"
      AllGoodPoliciesPlayed -> "Liberals won by enacting enough policies!"
      EvilLeaderElected -> "Fascists won by electing their leader!"
      EvilLeaderKilled -> "Liberals won by killing the Fascist leader!"
    (ReturnToLobbyAction <$) <$> button "Return to Lobby"

data TimeOfGovernment = Present | Past

playerList ::
  forall t m.
  (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m) =>
  Dynamic t GameView ->  m (Event t GameAction)
playerList gameView =
  fmap (attachDynWithMaybe wrapInGameAction (view #phase <$> gameView)) $
  fmap (gate $ current inSelectPhase) $
  elDynAttr
    "div"
    ((\s -> "id" =: "player_list" <> "class" =: bool "" "select" s) <$> inSelectPhase)
    (
      fmap switchDyn $
      (fmap . fmap) leftmost $
      simpleList
        (
          fmap (sortOn $ view $ _2 . #turnOrder) $
          fmap IM.toList $
          fmap (view #players) $
          gameView
        )
        (\(idAndPlayer :: Dynamic t (Int, PlayerView)) ->
          let player = snd <$> idAndPlayer
          in
            fmap (tagDyn $ fst <$> idAndPlayer) $
            fmap (domEvent Click) $
            fmap fst $
            elDynClass'
              "div"
              (
                ((<>)
                  <$> bool "dead " T.empty . view #alive
                  <*> bool "ineligible " T.empty . view #eligible
                )
                <$>
                player
              )
              (
                dynText (view #name <$> player) *>
                dyn_ (fmap roleDom $ fmap (view #role) $ player) *>
                dyn_
                  (zipDynWith
                    markOfficial
                    (fst <$> idAndPlayer)
                    gameView
                  )
              )
        )
    )
  where
    markOfficial :: Int -> GameView -> m ()
    markOfficial playerId (GameView {presidentId, phase})
      | playerId == presidentId = presidentMark Present
      | fromMaybe False $ (playerId ==) <$> chancellorIdGet phase =
        chancellorMark Present
      | 
        fromMaybe False $
        fmap (playerId ==) $
        fmap (view #presidentId) $
        previousGovernmentGet phase
        = presidentMark Past
      |
        fromMaybe False $
        fmap (playerId ==) $
        fmap (view #chancellorId) $
        previousGovernmentGet phase
        = chancellorMark Past
      | otherwise = pure ()
    presidentMark :: TimeOfGovernment -> m ()
    presidentMark timeOfGovernment =
      elAttr
        "img"
        (
          "title" =: "President" <>
          "id" =: "president_mark" <>
          "src" =: static @"president_mark.svg" <>
          case timeOfGovernment of
            Present -> mempty
            Past -> "class" =: "previous"
        )
        blank
    chancellorMark :: TimeOfGovernment -> m ()
    chancellorMark timeOfGovernment =
      elAttr
        "img"
        (
          "title" =: "Chancellor" <>
          "id" =: "chancellor_mark" <>
          "src" =: static @"chancellor_mark.png" <>
          case timeOfGovernment of
            Present -> mempty
            Past -> "class" =: "previous"
        )
        blank
    chancellorIdGet :: GamePhase -> (Maybe Int)
    chancellorIdGet =
      \case
        NominateChancellorPhase {} -> Nothing
        VotePhase {chancellorCandidateId} -> Just chancellorCandidateId
        PresidentDiscardPolicyPhase {chancellorId} -> Just chancellorId
        ChancellorDiscardPolicyPhase {chancellorId} -> Just chancellorId
        PolicyPeekPhase {chancellorId} -> Just chancellorId
        ExecutionPhase {chancellorId} -> Just chancellorId
        PendingVetoPhase {chancellorId} -> Just chancellorId
        GameOverPhase {} -> Nothing
    previousGovernmentGet :: GamePhase -> (Maybe Government)
    previousGovernmentGet =
      \case
        NominateChancellorPhase {previousGovernment} -> previousGovernment
        _ -> Nothing
    roleDom :: Maybe Role -> m ()
    roleDom =
      \case
        Nothing -> pure ()
        Just role ->
          elAttr
            "div"
            (
              "class" =: "role" <>
              "title" =:
                "L stands for Liberal, F stands for Fascist, H stands for Hitler"
            )
            (case role of
              GoodRole -> text "L"
              EvilRole -> text "F"
              EvilLeaderRole -> text "H"
            )
    inSelectPhase :: Dynamic t Bool
    inSelectPhase =
      (\(GameView {phase, playerId, presidentId}) ->
        case phase of
          NominateChancellorPhase {}
            | playerId == presidentId -> True
          ExecutionPhase {}
            | playerId == presidentId -> True
          _ -> False
      )
      <$>
      gameView
    wrapInGameAction :: GamePhase -> Int -> Maybe GameAction
    wrapInGameAction phase playerId =
      ($ playerId)
      <$>
      (case phase of
        NominateChancellorPhase {} -> Just NominateChancellor
        ExecutionPhase {} -> Just ExecutePlayer
        _ -> Nothing
      )    

tagDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagDyn = tag . current

attachDynWithMaybe ::
  Reflex t => (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c 
attachDynWithMaybe f = attachWithMaybe f . current

gridArea :: Int -> Int -> Text
gridArea x y =
  toStrict $
  toLazyText $
  (
    fromText "grid-area:" <>
    decimal (y + 1) <>
    fromText "/" <>
    decimal (x + 1) <>
    fromText "/" <>
    decimal (y + 2) <>
    fromText "/" <>
    decimal (x + 2)
  )

policyTiles ::
  forall (source :: Symbol) t m.
  (DomBuilder t m, StaticFile source) => Int -> m ()
policyTiles tileCount =
  for_
    [0 .. tileCount-1]
    (\i -> imgStyle @source (gridArea (1 + 2 * i) 0) blank)

button :: DomBuilder t m => Text -> m (Event t ())
button label =
  fmap (domEvent Click) $
  fmap fst $
  elAttr' "button" ("type" =: "button") (text label)

elId :: DomBuilder t m => Text -> Text -> m a -> m a
elId elementTag i child = snd <$> elId' elementTag i child

elId' ::
  DomBuilder t m =>
  Text -> Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elId' elementTag i = elAttr' elementTag ("id" =: i)

imgStyle ::
  forall (source :: Symbol) t m a.
  (DomBuilder t m, StaticFile source) => Text -> m a -> m a
imgStyle style child = snd <$> imgStyle' @source style child

imgStyle' ::
  forall (source :: Symbol) t m a.
  (DomBuilder t m, StaticFile source) =>
  Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
imgStyle' style child =
  elAttr' "img" ("src" =: static @source <> "style" =: style) child

application ::
  forall t m js.
  (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) =>
  Maybe Text -> m ()
application baseUri =
  mdo
    stateFromServer :: Event t StateFromServer <-
      fmap (mapMaybe A.decodeStrict') $
      fmap switchDyn $
      prerender
        (pure never)
        (case webSocketUri baseUri of
          Left _ -> pure never
          Right uri ->
            fmap (view webSocket_recv) $
            webSocket (render uri)
              (def & webSocketConfig_send .~ (((: []) . A.encode) <$> actionFromClient))
        )
    let
      widgetInitial :: m (Event t ActionFromClient)
      widgetInitial =
        (fmap . fmap) LobbyAction $
        (lobbyWidget =<<) $
        holdDyn lobbyViewInitial $
        mapMaybe
          (\case
            LobbyFromServer lobbyFromServer -> Just lobbyFromServer
            _ -> Nothing
          ) $
        stateFromServer
    widgetDynamic :: Event t (m (Event t ActionFromClient)) <-
      fmap updated $
      (fmap . fmap)
        (\case
          LobbyFromServerTag :=> lobbyFromServer ->
            (fmap . fmap) LobbyAction (lobbyWidget lobbyFromServer)
          GameFromServerTag :=> gameFromServer -> gameWidget gameFromServer
        ) $
      (stateFromServerDyn =<<) $
      holdDyn (LobbyFromServer lobbyViewInitial) $
      stateFromServer
    actionFromClient :: Event t ActionFromClient <-
      switchDyn <$>
      widgetHold
        widgetInitial
        widgetDynamic
    pure ()

webSocketUri :: Maybe Text -> Either (Maybe Text) URI
webSocketUri r =
  case checkEncoder fullRouteEncoder of
    Left errorText -> Left (Just errorText)
    Right encoder ->
      maybe (Left Nothing) Right $
      do
        let
          wsPath = fst $ encode encoder $ (FullRoute_Backend BackendRoute_Main) :/ ()
        uri' <- mkURI =<< r
        pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
        wsScheme <- case uriScheme uri' of
          rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
          rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
          _ -> Nothing
        pure (uri' {uriPath = Just (False, pathPiece), uriScheme = Just wsScheme})

stateFromServerDyn ::
  (Reflex t, MonadHold t m) =>
  Dynamic t StateFromServer -> m (Dynamic t (DSum StateFromServerTag (Dynamic t)))
stateFromServerDyn =
  (fmap . fmap) collapseComposeIdentity . factorDyn . fmap stateFromServerToDSum

collapseComposeIdentity ::
  Reflex t => DSum tag (Compose (Dynamic t) Identity) -> DSum tag (Dynamic t)
collapseComposeIdentity = mapNaturalTransformer (coerceDynamic . getCompose)

mapNaturalTransformer :: (forall a. f a -> g a) -> DSum k f -> DSum k g
mapNaturalTransformer f (sumTag :=> v) = sumTag :=> f v
