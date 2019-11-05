module Conduit.Data.Email where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.String.CodePoints (indexOf, length) as String
import Data.String.Pattern (Pattern(..))

newtype Email = Email String

derive instance genericEmail :: Generic Email _
derive instance newtypeEmail :: Newtype Email _

instance decodeJsonEmail :: DecodeJson Email where
  decodeJson = mkEmail <=< decodeJson

instance encodeJsonEmail :: EncodeJson Email where
  encodeJson = encodeJson <<< toString

instance showEmail :: Show Email where
  show = genericShow

mkEmail :: String -> Either String Email
mkEmail email
  | email == "" = Left "Empty value"
  | otherwise =
    let
      at = String.indexOf (Pattern "@") email
      end = String.length email - 1
    in
      case at of
        Nothing ->
          Left "Missing @ sign"
        Just 0 ->
          Left "Missing local part (username, alias, etc.)"
        Just index | index == end ->
          Left "Missing domain"
        Just _ ->
          pure $ Email email

toString :: Email -> String
toString = un Email
