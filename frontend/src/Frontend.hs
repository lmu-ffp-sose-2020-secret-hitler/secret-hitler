{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language RecursiveDo #-}
{-# language RankNTypes #-}

module Frontend where

import Data.Foldable (for_)
import Control.Monad.Fix (MonadFix)
import GHC.TypeLits (Symbol)
import Data.List.NonEmpty
-- import Control.Monad (void)
import Data.Text (Text)
-- import qualified Data.Text as T
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
  Dynamic t LobbyView -> m (Event t LobbyInput)
lobbyWidget lobbyView =
  do
    _ <- elId "div" "player_list_lobby" $
      simpleList (view #playerNames <$> lobbyView) (\m -> el "div" $ dynText m)
    nameElement <- inputElement $ def
    startGame <- (StartGame <$) <$> button "Start Game"
    pure $
      leftmost [
        startGame,
        Join <$> (updated $ value nameElement)
      ]

gameWidget ::
  (PostBuild t m, DomBuilder t m) => Dynamic t GameView ->  m (Event t GameInput)
gameWidget gameView =
  do
    elId "div" "phase_independent" $ do
      elId "div" "player_list" blank
      imgStyle @"draw_pile.png" "grid-area: draw_pile" blank
      imgStyle @"board_fascist_7_8.png" "grid-area: board_fascist" blank
      elId "div" "board_fascist" $
        dyn_ ((policyTiles @"policy_fascist.png" . view #goodPolicyCount) <$> gameView)
      imgStyle @"board_liberal.png" "grid-area: board_liberal" blank
      elId "div" "board_liberal" $
        dyn_ ((policyTiles @"policy_liberal.png" . view #goodPolicyCount) <$> gameView)
      elAttr "img" ("src" =: static @"role_liberal.png" <> "id" =: "identity") blank
      imgStyle @"discard_pile.png" "grid-area: discard_pile" blank
    elId "div" "phase_dependent" $ text "phase_dependent"
    incPolicy <- (fmap . fmap) (const IncreaseLiberalPolicyCount) (button "Inc")
    pure incPolicy

policyTiles ::
  forall (source :: Symbol) t m.
  (DomBuilder t m, StaticFile source) => Int -> m ()
policyTiles tileCount = for_ [1 .. tileCount] $
  \i ->
  imgStyle
    @source
    (
      toStrict $
      toLazyText $
      (
        fromText "grid-area: 1 /" <>
        decimal (2 * i) <>
        fromText "/ 2 /" <>
        decimal (2 * i + 1)
      )
    )
    blank

button :: DomBuilder t m => Text -> m (Event t ())
button label = do
  (reference, _) <- el' "button" (text label)
  pure (domEvent Click reference)

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
              (def & webSocketConfig_send .~ (((: []) . A.encode) <$> inputFromClient))
        )
    let
      widgetInitial :: m (Event t InputFromClient)
      widgetInitial =
        (fmap . fmap) LobbyInput $
        (lobbyWidget =<<) $
        holdDyn lobbyViewInitial $
        mapMaybe
          (\case
            LobbyFromServer lobbyFromServer -> Just lobbyFromServer
            _ -> Nothing
          ) $
        stateFromServer
    widgetDynamic :: Event t (m (Event t InputFromClient)) <-
      fmap updated $
      (fmap . fmap)
        (\case
          LobbyFromServerTag :=> lobbyFromServer ->
            (fmap . fmap) LobbyInput (lobbyWidget lobbyFromServer)
          GameFromServerTag :=> gameFromServer ->
            (fmap . fmap) GameInput (gameWidget gameFromServer)
        ) $
      (stateFromServerDyn =<<) $
      holdDyn (LobbyFromServer lobbyViewInitial) $
      stateFromServer
    inputFromClient :: Event t InputFromClient <-
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
