{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}

module Frontend where

import GHC.TypeLits (Symbol)
import Data.List.NonEmpty
-- import Control.Monad (void)
import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
-- import qualified Data.ByteString as B
import Text.URI
import Control.Lens
import qualified Data.Aeson as A
import Common.MessageTypes
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
          -- el "h1" $ text "Welcome to Obelisk!"
          -- el "p" $ text $ T.pack commonStuff

          -- -- `prerender` and `prerender_` let you choose a widget to run on the server
          -- -- during prerendering and a different widget to run on the client with
          -- -- JavaScript. The following will generate a `blank` widget on the server and
          -- -- print "Hello, World!" on the client.
          -- prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

          -- elAttr "img" ("src" =: static @"obelisk.jpg") blank
          -- el "div" $ do
          --   exampleConfig <- getConfig "common/example"
          --   case exampleConfig of
          --     Nothing -> text "No config file found in config/common/example"
          --     Just s -> text $ T.E.decodeUtf8 s
          lobbyMessage <- gameWidget
          r <- (fmap . fmap) T.E.decodeUtf8 (getConfig "common/route")
          playerNames <-
            (holdDyn [] =<<) $
            (fmap . fmap) (view #playerNames :: ServerToClient -> [Text]) $
            fmap (mapMaybe A.decodeStrict') $
            fmap switchDyn $
            prerender
              (pure never)
              (case webSocketUri r of
                Left _ -> pure never
                Right uri ->
                  fmap (view webSocket_recv) $
                  webSocket (render uri)
                    (def & webSocketConfig_send .~ (((: []) . A.encode) <$> lobbyMessage))
              )
          _ <- el "ul" $ simpleList playerNames (\m -> el "li" $ dynText m)
          pure ()
    }

gameWidget :: DomBuilder t m => m (Event t GameToServer)
gameWidget =
  do
    elId "div" "board" $ do
      imgStyle @"board_fascist_7_8.png" "grid-area: board_fascist" blank
      imgStyle @"board_liberal.png" "grid-area: board_liberal" blank
      elId "div" "board_liberal" $ do
        imgStyle @"policy_liberal.png" "grid-area: 1 / 3 / 2 / 4" blank
        imgStyle @"policy_liberal.png" "grid-area: 1 / 5 / 2 / 6" blank
        imgStyle @"policy_liberal.png" "grid-area: 1 / 7 / 2 / 8" blank
        imgStyle @"policy_liberal.png" "grid-area: 1 / 9 / 2 / 10" blank
        imgStyle @"policy_liberal.png" "grid-area: 1 / 11 / 2 / 12" blank
      imgStyle @"discard_pile.png" "grid-area: discard_pile" blank
    pure never

elId' ::
  DomBuilder t m => Text -> Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elId' elementTag i = elAttr' elementTag ("id" =: i)

elId :: DomBuilder t m => Text -> Text -> m a -> m a
elId elementTag i child = snd <$> elId' elementTag i child

imgStyle' ::
  forall (source :: Symbol) t m a.
  (DomBuilder t m, StaticFile source) =>
  Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
imgStyle' style child =
  elAttr' "img" ("src" =: static @source <> "style" =: style) child

imgStyle ::
  forall (source :: Symbol) t m a.
  (DomBuilder t m, StaticFile source) =>
  Text -> m a -> m a
imgStyle style child = snd <$> imgStyle' @source style child

lobbyWidget :: DomBuilder t m => m (Event t LobbyToServer)
lobbyWidget =
  do
    nameElement <- inputElement $ def
    pure $
      Join <$>
      tag (current $ value nameElement) (domEvent Keyup nameElement)

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
