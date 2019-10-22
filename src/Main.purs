module Main where

import Prelude
import Control.Monad.Except (ExceptT, throwError)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError, _errorDetails, error401, serve)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:/), type (:>), ReqBody)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

type Unlifted a = a

type AuthFieldsRep box r = ( email :: String, password :: box String | r )
type LoginFields = { | AuthFieldsRep Unlifted () }

type ProfileRep row =
  ( username :: String
  , bio :: Maybe String
  , image :: Maybe String
  | row
  )

type AuthenticatedUser = Record (ProfileRep (email :: String, token :: String))

type Site =
  "login" := "api" :/ "users" :/ "login" :/ ReqBody LoginFields JSON :> Post AuthenticatedUser JSON

site :: Proxy Site
site = Proxy

resources
  :: forall m
   . Monad m
  => { login :: LoginFields -> { "POST" :: ExceptT HTTPError m AuthenticatedUser } }
resources =
  { login: \({ email, password }) ->
    { "POST":
        if (email == "nick@saunde.rs" && password == "password")
          then pure { email, token: "TOKEN", username: "Nick", bio: Just "Chilling", image: Nothing }
          else throwError (error401 # _errorDetails .~ Just "Invalid credentials.")
    }
  }

main :: Effect Unit
main = do
  server <- createServer $ serve site resources identity
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
