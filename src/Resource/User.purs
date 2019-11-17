module Conduit.Resource.User where

import Prelude
import Conduit.Capability.Account (class Account, login, register, viewAccount)
import Conduit.Data.Account (Account)
import Conduit.Data.AccountView (AccountViewError(..))
import Conduit.Data.Login (Login, LoginError)
import Conduit.Data.Login (LoginError(..)) as Login
import Conduit.Data.Registration (Registration, RegistrationError(..))
import Conduit.Data.Token (Token)
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Lens ((.~))
import Data.Maybe (Maybe(Just))
import Nodetrout (HTTPError, _errorDetails, error401, error409, error500)

resources
  :: forall m
   . Account m
  => { login :: { user :: Login } -> { "POST" :: ExceptT HTTPError m { user :: Account } }
     , registration :: { user :: Registration } -> { "POST" :: ExceptT HTTPError m { user :: Account } }
     , accountView :: Token -> { "GET" :: ExceptT HTTPError m { user :: Account } }
     }
resources =
  { login: \{ user: l } -> { "POST": withExceptT loginErrorHTTP $ { user: _ } <$> login l }
  , registration: \{ user: r } -> { "POST": withExceptT registrationErrorHTTP $ { user: _ } <$> register r }
  , accountView: \token -> { "GET": withExceptT accountViewErrorHTTP $ { user: _ } <$> viewAccount token }
  }

loginErrorHTTP :: LoginError -> HTTPError
loginErrorHTTP = case _ of
  Login.InvalidUserData -> error500 # _errorDetails .~ Just "Invalid user data"
  _ -> error401 # _errorDetails .~ Just "Invalid username or password"

registrationErrorHTTP :: RegistrationError -> HTTPError
registrationErrorHTTP = case _ of
  UsernameConflict -> error409 # _errorDetails .~ Just "The requested username is already taken."

accountViewErrorHTTP :: AccountViewError -> HTTPError
accountViewErrorHTTP = case _ of
  InvalidUserData -> error500 # _errorDetails .~ Just "Invalid user data"
  _ -> error401 # _errorDetails .~ Just "Invalid token"
