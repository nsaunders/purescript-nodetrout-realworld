module Conduit.Data.Username where

import Prelude
import Data.Newtype (class Newtype, un)

newtype Username = Username String

derive instance newtypeUsername :: Newtype Username _

toString :: Username -> String
toString = un Username
