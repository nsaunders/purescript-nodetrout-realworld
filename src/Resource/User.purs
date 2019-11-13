module Conduit.Resource.User where

import Prelude
import Conduit.Capability.Auth (class Auth, loginUser, registerUser)
import Conduit.Data.Auth (AuthUser, Login, LoginError(..), Registration, RegistrationError(..))
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Lens ((.~))
import Data.Maybe (Maybe(Just))
import Nodetrout (HTTPError, _errorDetails, error401, error409, error500)

resources
  :: forall m
   . Auth m
  => { login :: { user :: Login } -> { "POST" :: ExceptT HTTPError m { user :: AuthUser } }
     , registration :: { user :: Registration } -> { "POST" :: ExceptT HTTPError m { user :: AuthUser } }
     }
resources =
  { login: \{ user: l } -> { "POST": withExceptT loginErrorHTTP $ { user: _ } <$> loginUser l }
  , registration: \{ user: r } -> { "POST": withExceptT registrationErrorHTTP $ { user: _ } <$> registerUser r }
  }

loginErrorHTTP :: LoginError -> HTTPError
loginErrorHTTP = case _ of
  InvalidUserData -> error500 # _errorDetails .~ Just "Invalid user data"
  _ -> error401 # _errorDetails .~ Just "Invalid username or password"

registrationErrorHTTP :: RegistrationError -> HTTPError
registrationErrorHTTP = case _ of
  UsernameConflict -> error409 # _errorDetails .~ Just "The requested username is already taken."
