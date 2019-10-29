module Conduit.Data.Registration where

import Conduit.Data.Email (Email)
import Conduit.Data.Username (Username)
import Conduit.Data.Password (Password)

type Registration =
  { email :: Email
  , username :: Username
  , password :: Password
  }
