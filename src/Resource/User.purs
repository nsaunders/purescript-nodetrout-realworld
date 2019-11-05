module Conduit.Resource.User where

import Prelude
import Conduit.Capability.User (class ManageUser, loginUser, registerUser)
import Conduit.Data.Auth (Login, LoginError(..), Registration, RegistrationError(..))
import Conduit.Data.User (User)
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Lens ((.~))
import Data.Maybe (Maybe(Just))
import Nodetrout (HTTPError, _errorDetails, error401, error409, error500)

resources
  :: forall m
   . ManageUser m
  => { login :: Login -> { "POST" :: ExceptT HTTPError m { user :: User } }
     , registration :: Registration -> { "POST" :: ExceptT HTTPError m { user :: User } }
     }
resources =
  { login: \l -> { "POST": withExceptT loginErrorHTTP $ { user: _ } <$> loginUser l }
  , registration: \r -> { "POST": withExceptT registrationErrorHTTP $ { user: _ } <$> registerUser r }
  }

loginErrorHTTP :: LoginError -> HTTPError
loginErrorHTTP = case _ of
  InvalidUserData -> error500 # _errorDetails .~ Just "Invalid user data"
  _ -> error401 # _errorDetails .~ Just "Invalid username or password"

registrationErrorHTTP :: RegistrationError -> HTTPError
registrationErrorHTTP = case _ of
  UsernameConflict -> error409 # _errorDetails .~ Just "The requested username is already taken."
