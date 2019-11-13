module Conduit.Api.User where

import Conduit.Data.Account (Account, Login, Registration)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:/), type (:<|>), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

type Api =
       "login" := "login" :/ ReqBody { user :: Login } JSON :> Resource (Post { user :: Account } JSON)
  :<|> "registration" := ReqBody { user :: Registration } JSON :> Resource (Post { user :: Account } JSON)

api :: Proxy Api
api = Proxy
