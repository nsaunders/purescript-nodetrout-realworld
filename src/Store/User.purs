module Conduit.Store.User where

import Prelude
import Conduit.Data.Email (Email)
import Conduit.Data.Email (toString) as Email
import Conduit.Data.Username (Username)
import Conduit.Data.Username (toString) as Username
import Data.Array (index, length)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Unfoldable (replicateA)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Node.Crypto.Hash (Algorithm(SHA512), base64)
import QueryDsl (Column, Table, insertInto, makeTable)
import QueryDsl.SQLite3 (runQuery)
import SQLite3 (DBConnection)
import Type.Data.Boolean (False, True)

user = makeTable "user" :: Table
  ( email :: Column String True
  , username :: Column String True
  , passwordHash :: Column String True
  , salt :: Column String True
  , bio :: Column String False
  , image :: Column String False
  )

type Registration =
  { email :: Email
  , username :: Username
  , password :: String
  }

newtype Salt = Salt String

derive instance newtypeSalt :: Newtype Salt _

genSalt :: forall m. MonadEffect m => m Salt
genSalt = liftEffect do
  let chars = toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  size <- randomInt 5 10
  (Salt <<< fromCharArray) <$> replicateA size ((fromMaybe 'A' <<< index chars) <$> randomInt 0 (length chars))

hashPassword :: forall m. MonadEffect m => Salt -> String -> m String
hashPassword (Salt salt) pass = liftEffect $ base64 SHA512 $ pass <> salt

register :: forall m. MonadAff m => Registration -> DBConnection -> m Unit
register { email, username, password } db = do
  salt <- genSalt
  passwordHash <- hashPassword salt password
  liftAff $ runQuery db $ insertInto user
    { email: Email.toString email
    , username: Username.toString username
    , passwordHash
    , salt: (un Salt salt)
    }
