module Conduit.Data.Account where

import Conduit.Data.Email (Email)
import Conduit.Data.Profile (ProfileRep)
import Conduit.Data.Token (Token)

type Account =
  { email :: Email
  , token :: Token
  | ProfileRep
  }
