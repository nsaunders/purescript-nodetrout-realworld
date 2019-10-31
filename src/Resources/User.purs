module Conduit.Resources.User where

import Prelude
import Conduit.Capability.User (class ManageUser, registerUser)
import Conduit.Data.Registration (Registration, RegistrationError(..))
import Control.Monad.Except (ExceptT, withExceptT)
import Data.Lens ((.~))
import Data.Maybe (Maybe(Just))
import Nodetrout (HTTPError, _errorDetails, error409)

resources :: forall m. ManageUser m => { registration :: Registration -> { "POST" :: ExceptT HTTPError m Unit } }
resources =
  { registration: \r -> { "POST": withExceptT registrationErrorHTTP $ registerUser r }
  }

registrationErrorHTTP :: RegistrationError -> HTTPError
registrationErrorHTTP = case _ of
  UsernameConflict -> error409 # _errorDetails .~ Just "The requested username is already taken."
