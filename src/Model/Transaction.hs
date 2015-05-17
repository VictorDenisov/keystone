{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Transaction
where

import Common (capitalize, camelToUnderscore)
import Common.Database (affectedDocs)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Bson (Val(..), (=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Char (toLower)
import Data.Data (Typeable)
import Data.Time.Clock (getCurrentTime)
import Model.Common (TransactionId(..), CaptureStatus(..), ProjectId(..))
import Model.Role (captureRole, rollbackCaptureRole, RoleId(..))
import Model.User (captureUser, UserId(..))
import Text.Read (readMaybe)

import qualified Data.Text as T
import qualified Database.MongoDB as M

data State = Initial
           | Pending
           | Applied
           | Done
           | Cancelling
           | Cancelled
             deriving (Show, Read, Eq, Ord, Typeable)

instance Val State where
  val st = M.String $ T.pack $ map toLower $ show st
  cast' (M.String s) = readMaybe $ capitalize $ T.unpack s
  cast' _ = Nothing

data Transaction = AddRole { userId    :: M.ObjectId
                           , roleId    :: M.ObjectId
                           , projectId :: M.ObjectId
                           } deriving (Show, Read, Eq, Ord, Typeable)
-- aux fields
state :: T.Text
state = "state"
lastModified :: T.Text
lastModified = "lastModified"

$(deriveBson id ''Transaction)

collectionName :: M.Collection
collectionName = "transaction"
