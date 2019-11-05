module Conduit.Data.Username where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, un)

newtype Username = Username String

derive instance genericUsername :: Generic Username _
derive instance newtypeUsername :: Newtype Username _

instance decodeJsonUsername :: DecodeJson Username where
  decodeJson = decodeJson >>> map Username

instance showUsername :: Show Username where
  show = genericShow

mkUsername :: String -> Either String Username
mkUsername username
  | username == "" = Left "Empty value"
  | otherwise = pure $ Username username

toString :: Username -> String
toString = un Username
