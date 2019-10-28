module Conduit.Capability.User where

import Prelude
import Conduit.Store.User (Registration)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Class (lift)

class Monad m <= ManageUser m where
  registerUser :: Registration -> m Unit

instance manageUserExcept :: ManageUser m => ManageUser (ExceptT e m) where
  registerUser = lift <<< registerUser
