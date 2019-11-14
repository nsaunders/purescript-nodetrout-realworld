module Conduit.Data.Login where

import Conduit.Data.Auth (AuthFields)

type Login = { | AuthFields }

data LoginError = InvalidUsername | InvalidPassword | InvalidUserData
