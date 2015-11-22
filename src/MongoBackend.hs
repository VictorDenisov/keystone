{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module MongoBackend where

import Backend
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, MonadReader(ask))

import qualified Auth as A
import qualified Config as C
import qualified Common.Database as CD
import qualified Database.MongoDB as M

type MongoBackend = ReaderT MongoData

data MongoData = MongoData
               { mddb :: C.Database }

instance MonadIO m => BackendApi (MongoBackend m) where
  type BackendHandle (MongoBackend m) = M.Pipe
  connect = do
    md <- ask
    liftIO $ CD.connect (mddb md)

  authenticate p s m = liftIO $ A.authenticate p s m
  close p = liftIO $ M.close p
