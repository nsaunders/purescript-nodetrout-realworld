module Conduit.Api.User where

import Conduit.Data.Auth (Registration)
import Conduit.Data.User (User)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:/), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

type Api = "registration" := "register" :/ ReqBody Registration JSON :> Resource (Post { user :: User } JSON)

api :: Proxy Api
api = Proxy
