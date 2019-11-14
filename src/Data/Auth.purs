module Conduit.Data.Auth where

import Conduit.Data.Password (Password)
import Conduit.Data.Username (Username)

type AuthFields =
  ( username :: Username
  , password :: Password
  )
