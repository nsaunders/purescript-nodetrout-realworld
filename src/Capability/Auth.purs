module Conduit.Capability.Auth where

import Prelude
import Conduit.Data.Auth (AuthUser, Login, LoginError, Registration, RegistrationError)
import Control.Monad.Except (ExceptT)

class Monad m <= Auth m where
  loginUser :: Login -> ExceptT LoginError m AuthUser
  registerUser :: Registration -> ExceptT RegistrationError m AuthUser
