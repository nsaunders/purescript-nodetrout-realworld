module Conduit.Data.Registration where

import Conduit.Data.Auth (AuthFields)
import Conduit.Data.Email (Email)

type Registration =
  { email :: Email
  | AuthFields
  }

data RegistrationError = UsernameConflict
