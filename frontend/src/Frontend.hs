{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad (void)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend (Frontend (Frontend, _frontend_head, _frontend_body))
import Obelisk.Configs (getConfig)
import Obelisk.Route (R)
import Obelisk.Generated.Static (static)

import Reflex.Dom.Core hiding (button)

import Common.Api
import Common.Route (FrontendRoute)


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
          el "h1" $ text "Welcome to Obelisk!"
          el "p" $ text $ Text.pack commonStuff

          -- `prerender` and `prerender_` let you choose a widget to run on the server
          -- during prerendering and a different widget to run on the client with
          -- JavaScript. The following will generate a `blank` widget on the server and
          -- print "Hello, World!" on the client.
          prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: Text.Text)

          elAttr "img" ("src" =: static @"obelisk.jpg") blank
          el "div" $ do
            exampleConfig <- getConfig "common/example"
            case exampleConfig of
              Nothing -> text "No config file found in config/common/example"
              Just s -> text $ Text.E.decodeUtf8 s
          return ()
    }
