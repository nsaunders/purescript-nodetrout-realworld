module Conduit.Data.Password where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, un)

newtype Password = Password String

derive instance genericPassword :: Generic Password _
derive instance newtypePassword :: Newtype Password _

instance decodeJsonPassword :: DecodeJson Password where
  decodeJson = decodeJson >>> map Password

instance genericShowPassword :: Show Password where
  show = genericShow

toString :: Password -> String
toString = un Password
