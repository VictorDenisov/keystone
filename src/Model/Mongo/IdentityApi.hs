{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Model.Mongo.IdentityApi where

import Keystone.Model.IdentityApi
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))
import Data.Pool (Pool, createPool, withResource)

import qualified Config as C
import qualified Model.Mongo.Common as CD
import qualified Database.MongoDB as M
import qualified Model.Mongo.User as MMU

type MongoIdentityApi = ReaderT MongoData

data MongoData = MongoData
               { mddb :: C.Database
               , pool :: Pool M.Pipe
               }

instance ( MonadBase IO m
         , MonadIO m
         , MonadBaseControl IO m
         ) => IdentityApi (MongoIdentityApi m) where
  type IdentityApiHandle (MongoIdentityApi m) = M.Pipe

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

runMongoBackend :: C.Database -> MongoIdentityApi IO a -> IO a
runMongoBackend mongoConfig action = do
  p <- createPool (CD.connect mongoConfig) M.close 1 60 10 -- stripe count, time to live, max resource count
  runReaderT action (MongoData mongoConfig p)
