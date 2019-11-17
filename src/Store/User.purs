module Conduit.Store.User where

import Prelude
import Conduit.Data.Account (Account)
import Conduit.Data.AccountView (AccountViewError)
import Conduit.Data.AccountView (AccountViewError(..)) as AccountView
import Conduit.Data.Login (Login, LoginError)
import Conduit.Data.Login (LoginError(..)) as Login
import Conduit.Data.Email (mkEmail)
import Conduit.Data.Email (toString) as Email
import Conduit.Data.Password (Password)
import Conduit.Data.Password (toString) as Password
import Conduit.Data.Registration (Registration, RegistrationError(..))
import Conduit.Data.Token (Token)
import Conduit.Data.Token (fromJwt, toJwt) as Token
import Conduit.Data.Username (Username, mkUsername)
import Conduit.Data.Username (toString) as Username
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Except.Trans (except, withExceptT)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (index, length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, un)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Unfoldable (replicateA)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Node.Crypto.Hash (Algorithm(SHA512), base64)
import Node.Simple.Jwt (Secret, decode, encode) as Jwt
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
  token <- liftEffect $ Token.fromJwt <$> Jwt.encode jwtSecret HS256 (Username.toString username)
  pure { email, token, username, bio: Nothing, image: Nothing }

getRecordByUsername
  :: forall env m
   . MonadAsk { db :: DBConnection | env } m
  => MonadAff m
  => Username
  -> m (Maybe UserRecord)
getRecordByUsername username = do
  { db } <- ask
  liftAff $ runSelectMaybeQuery db do
    u <- from user
    pure $ select
      { email: u.email
      , username: u.username
      , passwordHash: u.passwordHash
      , salt: u.salt
      , bio: u.bio
      , image: u.image
      }
      `where_` (u.username `Q.eq` Username.toString username)

mkAccount
  :: forall env m
   . MonadAsk { jwtSecret :: Jwt.Secret | env } m
  => MonadEffect m
  => UserRecord
  -> ExceptT String m Account
mkAccount { email, username, passwordHash, salt, bio, image } = do
  { jwtSecret } <- ask
  token <- liftEffect $ Token.fromJwt <$> Jwt.encode jwtSecret HS256 username
  except $ { email: _, username: _, token, bio, image } <$> mkEmail email <*> mkUsername username

logIn
  :: forall env m
   . MonadAsk { db :: DBConnection, jwtSecret :: Jwt.Secret | env } m
  => MonadAff m
  => Login
  -> ExceptT LoginError m Account
logIn login = do
  requestedUser <- getRecordByUsername login.username
  case requestedUser of
    Nothing ->
      throwError Login.InvalidUsername
    Just userRecord@{ salt, passwordHash } -> do
      loginPasswordHash <- hashPassword (Salt salt) login.password
      when (loginPasswordHash /= passwordHash) $ throwError Login.InvalidPassword
      withExceptT (const Login.InvalidUserData) $ mkAccount userRecord

viewAccount
  :: forall env m
   . MonadAsk { db :: DBConnection, jwtSecret :: Jwt.Secret | env } m
  => MonadAff m
  => Token
  -> ExceptT AccountViewError m Account
viewAccount token = do
  { jwtSecret } <- ask
  payload <- liftEffect $ lmap (const "JWT error") <$> (Jwt.decode jwtSecret $ Token.toJwt token)
  case payload >>= mkUsername of
    Left _ ->
      throwError AccountView.InvalidToken
    Right username -> do
      requestedUser <- getRecordByUsername username
      case requestedUser of
        Nothing ->
          throwError AccountView.InvalidUsername
        Just userRecord ->
          withExceptT (const AccountView.InvalidUserData) $ mkAccount userRecord
