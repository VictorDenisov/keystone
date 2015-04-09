{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
module Model.User
where

import Common (maybeNothing)
import Control.Applicative ((<$>))
import Control.Monad (mapM, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bson ((=:), ObjectId)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Text.Read (readMaybe)
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

createUser :: MonadIO m => User -> M.Action m M.Value
createUser u =
  M.insert collectionName $ toBson u

listUsers :: (MonadIO m, MonadBaseControl IO m) => M.Action m [User]
listUsers = do
  cursor <- M.find (M.select [] collectionName)
  docs <- M.rest cursor
  mapM fromBson docs

findUserById :: (MonadIO m) => String -> M.Action m (Maybe User)
findUserById uid = do
  maybeNothing (readMaybe uid :: Maybe ObjectId) $ \oid -> do
    mUser <- M.findOne (M.select ["_id" =: oid] collectionName)
    maybeNothing mUser $ \v -> Just `liftM` (fromBson v)
