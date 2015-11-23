{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MongoBackend where

import Backend
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(runReaderT), MonadReader(ask))
import Data.Pool (Pool, createPool, withResource)

import qualified Auth as A
import qualified Config as C
import qualified Common.Database as CD
import qualified Database.MongoDB as M
import qualified Model.User as MU

type MongoBackend = ReaderT MongoData

data MongoData = MongoData
               { mddb :: C.Database
               , pool :: Pool M.Pipe
               }

instance ( MonadBase IO m
         , MonadIO m
         , MonadBaseControl IO m
         ) => BackendApi (MongoBackend m) where
  type BackendHandle (MongoBackend m) = M.Pipe

  authenticate s m = withHandle $ \p -> liftIO $ A.authenticate p s m

  createUser u = withHandle $ \p -> liftIO $ CD.runDB p $ MU.createUser u

  findUserById i = withHandle $ \p -> liftIO $ CD.runDB p $ MU.findUserById i

  listUsers mName = withHandle $ \p -> liftIO $ CD.runDB p $ MU.listUsers mName

  updateUser i d = withHandle $ \p -> liftIO $ CD.runDB p $ MU.updateUser i d

  withHandle action = do
    d <- ask
    withResource (pool d) action

runMongoBackend :: C.Database -> MongoBackend IO a -> IO a
runMongoBackend mongoConfig action = do
  p <- createPool (CD.connect mongoConfig) M.close 1 60 10 -- stripe count, time to live, max resource count
  runReaderT action (MongoData mongoConfig p)
