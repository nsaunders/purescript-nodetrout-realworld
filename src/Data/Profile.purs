module Conduit.Data.Profile where

import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)

type ProfileRep =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe String
  )
