module Conduit.Data.Auth where

import Conduit.Data.Email (Email)
import Conduit.Data.Username (Username)
import Conduit.Data.Password (Password)

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
