module Main where

import Prelude
import Conduit.Api.User (Api) as User
import Conduit.AppM (runAppM)
import Conduit.Resource.User (resources) as User
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Node.HTTP (createServer, listen)
import Nodetrout (serve)
import SQLite3 (newDB)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:/))

type Api = "users" := "api" :/ "users" :/ User.Api

main :: Effect Unit
main = launchAff_ do
  db <- newDB "conduit.db"
  let jwtSecret = "temporary"
  liftEffect do
    server <- createServer $ serve api resources (runAppM { db, jwtSecret }) onError
    listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."

  where

    api = Proxy :: Proxy Api

    resources =
      { users: User.resources
      }

    onError e = log $ "An unhandled error occurred: " <> message e
