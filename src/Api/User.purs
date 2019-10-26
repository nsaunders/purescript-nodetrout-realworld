module Conduit.Api.User where

import Prelude
import Conduit.Database.User (Registration)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:=), type (:/), type (:>), ReqBody, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Post)

type Api base = "registration" := base :/ "login" :/ ReqBody Registration JSON :> Resource (Post Unit JSON)

api :: forall base. Proxy (Api base)
api = Proxy
