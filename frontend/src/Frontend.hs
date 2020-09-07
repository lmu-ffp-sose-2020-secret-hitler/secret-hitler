{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Data.List.NonEmpty
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.E
import Language.Javascript.JSaddle (eval, liftJSM)

import qualified Data.ByteString as B
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core hiding (button)

import Common.Api
import Common.Route
import Text.URI

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
          r <- (fmap . fmap) T.E.decodeUtf8 (getConfig "common/route")
          _ <- wsRespEv r
          return ()
    }

wsRespEv ::
  (
    Reflex t,
    Monad m,
    Prerender js t m
  ) => Maybe T.Text -> m (Dynamic t (Event t B.ByteString))
wsRespEv r =
   prerender (return never) $ do
    case checkEncoder fullRouteEncoder of
      Left err -> do
        el "div" $ text err
        return never
      Right encoder -> do
        let wsPath = fst $ encode encoder $ (FullRoute_Backend BackendRoute_Main) :/ ()
        let mUri = do
              uri' <- mkURI =<< r
              pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
              wsScheme <- case uriScheme uri' of
                rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
                rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
                _ -> Nothing
              return $ uri'
                { uriPath = Just (False, pathPiece)
                , uriScheme = Just wsScheme
                }
        case mUri of
          Nothing -> return never
          Just uri -> do
            ws <- webSocket (render uri) $ def & webSocketConfig_send .~ (never :: Reflex t => Event t [B.ByteString])
            return (_webSocket_recv ws)
