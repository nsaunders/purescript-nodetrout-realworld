module Conduit.Api.User where

import Prelude
import Conduit.Data.Registration (Registration)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:>), type (:/), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

type Api = "registration" := "register" :/ ReqBody Registration JSON :> Resource (Post Unit JSON)

api :: Proxy Api
api = Proxy
