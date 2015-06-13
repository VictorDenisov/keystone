module Model.Common
where

import Common.Database (idF, inC)

import Data.Bson ((=:), ObjectId)

import qualified Database.MongoDB as M

newtype TransactionId = TransactionId M.ObjectId

data CaptureStatus = Captured
                   | CaptureFailed
                     deriving (Eq, Show)

newtype ProjectId = ProjectId M.ObjectId

data OpStatus = Success
              | NotFound
              | Busy
              | Duplicate String -- What field was duplicate
                deriving Show

listExistingIds :: M.Collection -> [M.ObjectId] -> M.Action IO [M.ObjectId]
listExistingIds collection ids = do
  cur <- M.find (M.select [ idF =: [inC =: (M.Array $ map M.ObjId ids)] ] collection)
  docs <- M.rest cur
  return $ map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
