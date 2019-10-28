module Main where

import Prelude
import Conduit.Api.User as User
import Conduit.AppM (runAppM)
import Conduit.Capability.User (class ManageUser)
import Conduit.Resources.User as User
import Control.Monad.Except (ExceptT, throwError)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError, _errorDetails, error401, serve)
import SQLite3 (newDB)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:>), Lit, Sub)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

main :: Effect Unit
main = launchAff_ do
  db <- newDB "conduit.db"
  liftEffect do
    server <- createServer $ serve User.api User.resources (runAppM { db })
    listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
