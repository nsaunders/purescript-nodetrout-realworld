module Conduit.Data.Email where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, un)

newtype Email = Email String

derive instance genericEmail :: Generic Email _
derive instance newtypeEmail :: Newtype Email _

instance decodeJsonEmail :: DecodeJson Email where
  decodeJson = decodeJson >>> map Email

instance showEmail :: Show Email where
  show = genericShow

toString :: Email -> String
toString = un Email
