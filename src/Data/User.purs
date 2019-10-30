module Conduit.Data.User where

import Conduit.Data.Email (Email)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)

type User =
  { email :: Email
  , username :: Username
  , bio :: Maybe String
  , image :: Maybe String
  }
