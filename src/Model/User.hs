{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
module Model.User
where

import Control.Monad (mapM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
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
