module Model.Common
where

import Model.Mongo.Common (idF, inC)
import Control.Monad (forM)

import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..))
import Data.List.Split (chunksOf)

import qualified Database.MongoDB as M

newtype TransactionId = TransactionId M.ObjectId

data CaptureStatus = Captured
                   | CaptureFailed
                     deriving (Eq, Show)

newtype ProjectId = ProjectId M.ObjectId

data OpStatus = Success
              | NotFound
              | Duplicate String -- What field was duplicate
                deriving Show

listExistingIds :: M.Collection -> [M.ObjectId] -> M.Action IO [M.ObjectId]
listExistingIds collection ids = do
  chunks <- forM (chunksOf 50000 ids) $ \chunk -> do -- we need to split long list into chunks because mongodb limits the request size
    cur <- M.find (M.select [ idF =: [inC =: (M.Array $ map M.ObjId chunk)] ] collection)
    docs <- M.rest cur
    return $ map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
  return $ concat chunks

listObjects :: Bson a => M.Collection -> [M.ObjectId] -> M.Action IO [a]
listObjects collection ids = do
  chunks <- forM (chunksOf 50000 ids) $ \chunk -> do -- we need to split long list into chunks because mongodb limits the request size
    cur <- M.find (M.select [ idF =: [inC =: (M.Array $ map M.ObjId chunk)] ] collection)
    docs <- M.rest cur
    mapM fromBson docs
  return $ concat chunks
