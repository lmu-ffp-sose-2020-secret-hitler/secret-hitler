{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language RecursiveDo #-}
{-# language RankNTypes #-}

module Frontend where

import Common.GameMessages
import Data.Foldable (for_)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromMaybe)
import GHC.TypeLits (Symbol)
import Data.List.NonEmpty
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
  Dynamic t GameUpdate ->  m (Event t GameAction)
gameWidget gameUpdate =
  elId "div" "board" $ do
    playerSelect :: Event t GameAction <- playerList gameView
    imgStyle @"draw_pile.png" "grid-area: draw_pile" blank
    imgStyle @"board_fascist_7_8.png" "grid-area: board_fascist" blank
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
            (\i -> "src" =: static @"circle.svg" <> "style" =: gridArea (2 * i) 2) $
          fmap (view #electionTracker) $
          gameView
        )
        blank
    elAttr "img" ("src" =: static @"role_liberal.png" <> "id" =: "identity") blank
    imgStyle @"discard_pile.png" "grid-area: discard_pile" blank
    phaseDependentAction <- elId "div" "phase_dependent" $
      fmap switchDyn $
      widgetHold
        nominateChancellorPhaseWidget
        (
          updated $
          fmap phaseDependentWidget $
          gameView
        )        
      -- switchHold never =<<
      -- dyn
      --   (
      --     phaseDependentWidget
      --     <$>
      --     gameView
      --   )
    pure $ leftmost [playerSelect, phaseDependentAction]
  where
    gameView :: Dynamic t GameView
    gameView = view #gameView <$> gameUpdate

phaseDependentWidget ::
  (PostBuild t m, DomBuilder t m, MonadHold t m) =>
  GameView -> m (Event t GameAction)
phaseDependentWidget (GameView {phase, playerId, presidentId}) =
  case phase of
    VotePhase {} -> votePhaseWidget
    NominateChancellorPhase {}
      | playerId == presidentId -> nominateChancellorPhaseWidget
    _ -> blank *> pure never

votePhaseWidget :: DomBuilder t m => m (Event t GameAction)
votePhaseWidget = do
  event <- elId "div" "vote_phase" $ do
    (yesButton, _) <- elAttr' "img" ("src" =: static @"ja_ballot.png") blank
    (noButton, _) <- elAttr' "img" ("src" =: static @"nein_ballot.png") blank
    return $
      leftmost [
        PlaceVote True <$ domEvent Click yesButton,
        PlaceVote False <$ domEvent Click noButton
      ]
  return event

nominateChancellorPhaseWidget :: DomBuilder t m => m (Event t GameAction)
nominateChancellorPhaseWidget = do
  text "Please nominate a chancellor by clicking their name."
  pure never

playerList ::
  forall t m.
  (PostBuild t m, DomBuilder t m, MonadFix m, MonadHold t m) =>
  Dynamic t GameView ->  m (Event t GameAction)
playerList gameView =
  fmap (attachDynWithMaybe wrapInGameAction (view #phase <$> gameView)) $
  fmap (gate $ current selectable) $
  elDynAttr
    "div"
    ((\s -> "id" =: "player_list" <> "class" =: bool "" "select" s) <$> selectable)
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
        (
          \(idAndPlayer :: Dynamic t (Int, PlayerView)) ->
          do
            let player = snd <$> idAndPlayer
            (reference, _) <-
              elDynClass'
                "div"
                ((bool "dead" T.empty . view #alive) <$> player)
                (do
                  dynText (view #name <$> player)
                  (
                    dyn_ $
                    fmap (bool blank presidentMark) $
                    zipDynWith
                      (==)
                      (fst <$> idAndPlayer)
                      (view #presidentId <$> gameView))
                  (
                    dyn_ $
                    fmap (bool blank chancellorMark) $
                    zipDynWith
                      (\p c -> fromMaybe False $ (p ==) <$> c)
                      (fst <$> idAndPlayer)
                      chancellorIdCurrent)
                )
            pure $ tagDyn (fst <$> idAndPlayer) (domEvent Click reference)
        )
    )
  where
    selectable :: Dynamic t Bool
    selectable =
      (\(GameView {phase, playerId, presidentId}) ->
        case phase of
          NominateChancellorPhase {}
            | playerId == presidentId -> True
          _ -> False
      )
      <$>
      gameView
    presidentMark :: m ()
    presidentMark =
      elAttr
        "img"
        (
          "title" =: "President" <>
          "id" =: "president_mark" <>
          "src" =: static @"president_mark.svg"
        )
        blank
    chancellorMark :: m ()
    chancellorMark =
      elAttr
        "img"
        (
          "title" =: "Chancellor" <>
          "id" =: "chancellor_mark" <>
          "src" =: static @"chancellor_mark.png"
        )
        blank
    chancellorIdCurrent :: Dynamic t (Maybe Int)
    chancellorIdCurrent =
      fmap
        (\case
          NominateChancellorPhase {} -> Nothing
          VotePhase {chancellorCandidateId} -> Just chancellorCandidateId
          PresidentDiscardPolicyPhase {chancellorId} -> Just chancellorId
          ChancellorDiscardPolicyPhase {chancellorId} -> Just chancellorId
          PolicyPeekPhase {chancellorId} -> Just chancellorId
          ExecutionPhase {chancellorId} -> Just chancellorId
          PendingVetoPhase {chancellorId} -> Just chancellorId
        ) $
      fmap (view #phase) $
      gameView
    wrapInGameAction :: GamePhase -> Int -> Maybe GameAction
    wrapInGameAction phase playerId =
      ($ playerId)
      <$>
      (case phase of
        NominateChancellorPhase {} -> Just NominateChancellor
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
    decimal y <>
    fromText "/" <>
    decimal x <>
    fromText "/" <>
    decimal (y + 1) <>
    fromText "/" <>
    decimal (x + 1)
  )

policyTiles ::
  forall (source :: Symbol) t m.
  (DomBuilder t m, StaticFile source) => Int -> m ()
policyTiles tileCount =
  for_
    [1 .. tileCount]
    (\i -> imgStyle @source (gridArea (2 * i) 1) blank)

button :: DomBuilder t m => Text -> m (Event t ())
button label =
  fmap (domEvent Click) $
  fmap fst $
  el' "button" (text label)

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
          GameFromServerTag :=> gameFromServer ->
            (fmap . fmap) GameAction (gameWidget gameFromServer)
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
