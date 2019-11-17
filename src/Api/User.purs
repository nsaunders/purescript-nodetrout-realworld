module Conduit.Api.User where

import Conduit.Data.Account (Account)
import Conduit.Data.Login (Login)
import Conduit.Data.Registration (Registration)
import Conduit.Data.Token (Token)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:/), type (:<|>), Header, ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get, Post)

type Wrap a = { user :: a }

type Api =
       "login" := "users" :/ "login" :/ ReqBody (Wrap Login) JSON :> Resource (Post (Wrap Account) JSON)
  :<|> "registration" := "users" :/ ReqBody (Wrap Registration) JSON :> Resource (Post (Wrap Account) JSON)
  :<|> "accountView" := "user" :/ Header "Authorization" Token :> Resource (Get (Wrap Account) JSON)

api :: Proxy Api
api = Proxy
