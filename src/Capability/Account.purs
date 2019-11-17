module Conduit.Capability.Account where

import Prelude
import Conduit.Data.Account (Account)
import Conduit.Data.AccountView (AccountViewError)
import Conduit.Data.Login (Login, LoginError)
import Conduit.Data.Registration (Registration, RegistrationError)
import Conduit.Data.Token (Token)
import Control.Monad.Except (ExceptT)

class Monad m <= Account m where
  login :: Login -> ExceptT LoginError m Account
  register :: Registration -> ExceptT RegistrationError m Account
  viewAccount :: Token -> ExceptT AccountViewError m Account
