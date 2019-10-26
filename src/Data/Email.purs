module Conduit.Data.Email where

import Prelude
import Data.Newtype (class Newtype, un)

newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _

toString :: Email -> String
toString = un Email
