{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
module Model.User
where

import Control.Applicative ((<$>))
import Control.Monad (mapM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
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

findUserById :: (MonadIO m) => ObjectId -> M.Action m (Maybe User)
findUserById uid = runMaybeT $ do
  mUser <- MaybeT $ M.findOne (M.select ["_id" =: uid] collectionName)
  fromBson mUser
