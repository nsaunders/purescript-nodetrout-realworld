module Conduit.AppM where

import Prelude
import Conduit.Capability.Account (class Account)
import Conduit.Env (Env)
import Conduit.Store.User as UserStore
import Control.Monad.Reader.Class (class MonadAsk, asks)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Type.Equality (class TypeEquals, from)

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance functorAppM :: Functor AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance manageUserAppM :: Account AppM where
  login = UserStore.logIn
  register = UserStore.register

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM appM) = runReaderT appM env
