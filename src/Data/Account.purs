module Conduit.Data.Account where

import Conduit.Data.Email (Email)
import Conduit.Data.Profile (ProfileRep)

type Account =
  { email :: Email
  , token :: String
  | ProfileRep
  }
