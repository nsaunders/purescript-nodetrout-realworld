module Conduit.Data.Auth where

import Conduit.Data.Email (Email)
import Conduit.Data.Profile (ProfileRep)
import Conduit.Data.Password (Password)
import Conduit.Data.Username (Username)

type CommonFields =
  ( username :: Username
  , password :: Password
  )

type Registration =
  { email :: Email
  | CommonFields
  }

data RegistrationError = UsernameConflict

type Login = { | CommonFields }

data LoginError = InvalidUsername | InvalidPassword | InvalidUserData

type AuthUser =
  { email :: Email
  , token :: String
  | ProfileRep
  }
