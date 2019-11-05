module Conduit.Capability.User where

import Prelude
import Conduit.Data.Auth (Registration, RegistrationError)
import Conduit.Data.User (User)
import Control.Monad.Except (ExceptT)

class Monad m <= ManageUser m where
  registerUser :: Registration -> ExceptT RegistrationError m User
