{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Keystone.Model.Mongo.IdentityApi where

import Keystone.Model.IdentityApi
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))
import Data.Pool (Pool, withResource)

import qualified Config as C
import qualified Model.Mongo.Common as CD
import qualified Keystone.Model.Mongo.User as MMU

type MongoIdentityApi = ReaderT MongoData

data MongoData = MongoData
               { mddb :: C.Database
               , pool :: Pool CD.Connection
               }

instance ( MonadBase IO m
         , MonadIO m
         , MonadBaseControl IO m
         ) => IdentityApi (MongoIdentityApi m) where
  type IdentityApiHandle (MongoIdentityApi m) = CD.Connection

  createUser u = withHandle $ \p -> liftIO $ CD.runDB p $ MMU.createUser u

  findUserById i = withHandle $ \p -> liftIO $ CD.runDB p $ MMU.findUserById i

  listUsers mName = withHandle $ \p -> liftIO $ CD.runDB p $ MMU.listUsers mName

  updateUser i d = withHandle $ \p -> liftIO $ CD.runDB p $ MMU.updateUser i d

  deleteUser i = withHandle $ \p -> liftIO $ CD.runDB p $ MMU.deleteUser i

  checkUserPassword mid mUserName password =
          withHandle $ \p -> liftIO $ CD.runDB p $ MMU.checkUserPassword mid mUserName password

  withHandle action = do
    d <- ask
    withResource (pool d) action

runMongoBackend :: Pool CD.Connection -> C.Database -> MongoIdentityApi IO a -> IO a
runMongoBackend p mongoConfig action = do
  runReaderT action (MongoData mongoConfig p)
