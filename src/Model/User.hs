{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
module Model.User
where

import Control.Applicative ((<$>))
import Control.Monad (mapM, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bson ((=:), ObjectId)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import qualified Database.MongoDB as M

collectionName :: M.Collection
collectionName = "user"

data User = User { description :: Maybe String
                 , email :: Maybe String
                 , enabled :: Bool
                 , name :: String
                 , password :: Maybe String -- hash of the password
                 } deriving (Show, Read, Eq, Ord, Typeable)

$(deriveBson ''User)

-- User should implement a class document convertable

createUser :: MonadIO m => User -> M.Action m M.Value
createUser u =
  M.insert collectionName $ toBson u

listUsers :: (MonadIO m, MonadBaseControl IO m) => M.Action m [User]
listUsers = do
  cursor <- M.find (M.select [] collectionName)
  docs <- M.rest cursor
  mapM fromBson docs

getUserById :: (MonadIO m) => String -> M.Action m (Maybe User)
getUserById uid = do
  let oid = (read uid) :: ObjectId
  mUser <- M.findOne (M.select ["_id" =: oid] collectionName)
  case mUser of
    Nothing -> return Nothing
    Just v -> Just `liftM` (fromBson v)
