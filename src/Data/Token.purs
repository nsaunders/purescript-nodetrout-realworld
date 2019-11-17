module Conduit.Data.Token where

import Prelude
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.String.CodePoints (drop, take) as String
import Data.String.Common (toLower) as String
import Node.Simple.Jwt (Jwt)
import Node.Simple.Jwt (fromString, toString) as Jwt
import Type.Trout.Header (class FromHeader)

newtype Token = Token Jwt

instance encodeJsonToken :: EncodeJson Token where
  encodeJson = encodeJson <<< Jwt.toString <<< toJwt

fromJwt :: Jwt -> Token
fromJwt = Token

toJwt :: Token -> Jwt
toJwt (Token jwt) = jwt

instance fromHeaderToken :: FromHeader Token where
  fromHeader h
    | String.toLower (String.take 6 h) == "token " = Right $ Token $ Jwt.fromString $ String.drop 6 h
    | otherwise = Left "Only Token authorization is supported."
