{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Model.Ldap.IdentityApi where

import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))
import Data.Pool (Pool, createPool, withResource)
import Model.IdentityApi

import qualified Config as C
import qualified LDAP as L
import qualified Model.Ldap.User as MLU

type LdapBackend = ReaderT LdapData

data LdapData = LdapData
               { ldconf :: C.LdapConfig
               , pool   :: Pool L.LDAP
               }

instance ( MonadBase IO m
         , MonadIO m
         , MonadBaseControl IO m
         ) => IdentityApi (LdapBackend m) where
  type IdentityApiHandle (LdapBackend m) = L.LDAP

  createUser u = undefined

  findUserById i = do
    d <- ask
    withHandle $ \l -> liftIO $ MLU.findUserById (ldconf d) l i

  listUsers mName = do
    d <- ask
    withHandle $ \l -> liftIO $ MLU.listUsers mName (ldconf d) l

  updateUser i d = undefined

  deleteUser i = undefined

  checkUserPassword mid mUserName password = do
    d <- ask
    withHandle $ \l -> liftIO $ MLU.checkUserPassword (ldconf d) l mid mUserName password

  withHandle action = do
    d <- ask
    withResource (pool d) action

runLdapBackend :: C.LdapConfig -> LdapBackend IO a -> IO a
runLdapBackend ldapConfig action = do
  let h = C.ldapHost ldapConfig
  p <- createPool (doConnect h) (\_ -> return ()) 1 60 10 -- stripe count, time to live, max resource count
  runReaderT action (LdapData ldapConfig p)
  where
    doConnect h = do
      l <- L.ldapInit h 389
      L.ldapSimpleBind l "cn=admin,dc=test,dc=com" "qwerty"
      return l
