module Conduit.Capability.User where

import Prelude
import Conduit.Data.Auth (Login, LoginError, Registration, RegistrationError)
import Conduit.Data.User (User)
import Control.Monad.Except (ExceptT)

class Monad m <= ManageUser m where
  loginUser :: Login -> ExceptT LoginError m User
  registerUser :: Registration -> ExceptT RegistrationError m User
