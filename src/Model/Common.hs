module Model.Common
where

import qualified Database.MongoDB as M

newtype TransactionId = TransactionId M.ObjectId

data CaptureStatus = Captured
                   | CaptureFailed
                     deriving (Eq, Show)

newtype ProjectId = ProjectId M.ObjectId

