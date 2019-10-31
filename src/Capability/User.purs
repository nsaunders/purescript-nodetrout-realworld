module Conduit.Capability.User where

import Prelude
import Conduit.Data.Registration (Registration, RegistrationError)
import Control.Monad.Except (ExceptT)

class Monad m <= ManageUser m where
  registerUser :: Registration -> ExceptT RegistrationError m Unit
