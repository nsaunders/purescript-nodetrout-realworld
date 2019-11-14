module Conduit.Store.User where

import Prelude
import Conduit.Data.Account (Account)
import Conduit.Data.Login (Login, LoginError)
import Conduit.Data.Login (LoginError(..)) as Login
import Conduit.Data.Registration (Registration, RegistrationError(..))
import Conduit.Data.Email (mkEmail)
import Conduit.Data.Email (toString) as Email
import Conduit.Data.Password (Password)
import Conduit.Data.Password (toString) as Password
import Conduit.Data.Username (mkUsername)
import Conduit.Data.Username (toString) as Username
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (index, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, un)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Unfoldable (replicateA)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Node.Crypto.Hash (Algorithm(SHA512), base64)
import Node.Simple.Jwt (Secret, encode, toString) as Jwt
import Node.Simple.Jwt (Algorithm(HS256))
import QueryDsl (Column, Table, from, insertInto, makeTable, select, where_)
import QueryDsl.Expressions (eq) as Q
import QueryDsl.SQLite3 (runQuery, runSelectMaybeQuery)
import SQLite3 (DBConnection)
import Type.Data.Boolean (False, True)

user = makeTable "user" :: Table
  ( email :: Column String True
  , username :: Column String True
  , passwordHash :: Column String True
  , salt :: Column String True
  , bio :: Column (Maybe String) False
  , image :: Column (Maybe String) False
  )

type UserRecord =
  { email :: String
  , username :: String
  , passwordHash :: String
  , salt :: String
  , bio :: Maybe String
  , image :: Maybe String
  }

newtype Salt = Salt String

derive instance newtypeSalt :: Newtype Salt _

genSalt :: forall m. MonadEffect m => m Salt
genSalt = liftEffect do
  let chars = toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  size <- randomInt 5 10
  (Salt <<< fromCharArray) <$> replicateA size ((fromMaybe 'A' <<< index chars) <$> randomInt 0 (length chars))

hashPassword :: forall m. MonadEffect m => Salt -> Password -> m String
hashPassword (Salt salt) pass = liftEffect $ base64 SHA512 $ (Password.toString pass) <> salt

register
  :: forall env m
   . MonadAsk { db :: DBConnection, jwtSecret :: Jwt.Secret | env } m
  => MonadAff m
  => Registration
  -> ExceptT RegistrationError m Account
register { email, username, password } = do
  { db, jwtSecret } <- ask
  existing :: Maybe { username :: String } <- liftAff $ runSelectMaybeQuery db do
                                                u <- from user
                                                pure $ select { username: u.username }
                                                  `where_` (u.username `Q.eq` Username.toString username)
  when (isJust existing) (throwError UsernameConflict)
  salt <- genSalt
  passwordHash <- hashPassword salt password
  liftAff $ runQuery db $ insertInto user
    { email: Email.toString email
    , username: Username.toString username
    , passwordHash
    , salt: (un Salt salt)
    }
  token <- liftEffect $ Jwt.toString <$> Jwt.encode jwtSecret HS256 (Username.toString username)
  pure { email, token, username, bio: Nothing, image: Nothing }

logIn
  :: forall env m
   . MonadAsk { db :: DBConnection, jwtSecret :: Jwt.Secret | env } m
  => MonadAff m
  => Login
  -> ExceptT LoginError m Account
logIn login = do
  { db, jwtSecret } <- ask
  requestedUser :: Maybe UserRecord <- liftAff $ runSelectMaybeQuery db do
                                         u <- from user
                                         pure $ select
                                           { email: u.email
                                           , username: u.username
                                           , passwordHash: u.passwordHash
                                           , salt: u.salt
                                           , bio: u.bio
                                           , image: u.image
                                           }
                                           `where_` (u.username `Q.eq` Username.toString login.username)
  case requestedUser of
    Nothing ->
      throwError Login.InvalidUsername
    Just { email, username, passwordHash, salt, bio, image } -> do
      loginPasswordHash <- hashPassword (Salt salt) login.password
      when (loginPasswordHash /= passwordHash) $ throwError Login.InvalidPassword
      token <- liftEffect $ Jwt.toString <$> Jwt.encode jwtSecret HS256 username
      case { email: _, username: _, token, bio, image } <$> mkEmail email <*> mkUsername username of
        Left error ->
          throwError Login.InvalidUserData
        Right loggedInUser ->
          pure loggedInUser
