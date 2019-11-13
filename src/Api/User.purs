module Conduit.Api.User where

import Conduit.Data.Auth (AuthUser, Login, Registration)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:/), type (:<|>), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

type Api =
       "login" := "login" :/ ReqBody { user :: Login } JSON :> Resource (Post { user :: AuthUser } JSON)
  :<|> "registration" := ReqBody { user :: Registration } JSON :> Resource (Post { user :: AuthUser } JSON)

api :: Proxy Api
api = Proxy
