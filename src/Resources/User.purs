module Conduit.Resources.User where

import Prelude
import Conduit.Capability.User (class ManageUser, registerUser)
import Conduit.Data.Registration (Registration)
import Control.Monad.Except (ExceptT)
import Data.Argonaut (class EncodeJson, encodeJson)
import Nodetrout (HTTPError)

resources :: forall m. ManageUser m => { registration :: Registration -> { "POST" :: ExceptT HTTPError m Unit } }
resources = { registration: \r -> { "POST": registerUser r *> pure unit } }
