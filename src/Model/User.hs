{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
module Model.User
where

import Common (fromObject)
import Common.Database (affectedDocs)
import Control.Applicative ((<$>))
import Control.Monad (mapM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Bson ((=:), ObjectId)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "user"

data User = User { description :: Maybe String
                 , email :: Maybe String
                 , enabled :: Bool
                 , name :: String
                 , password :: Maybe String -- hash of the password
                 } deriving (Show, Read, Eq, Ord, Typeable)

$(deriveBson ''User)

$(deriveJSON defaultOptions ''User)

produceUserJson :: User -> M.ObjectId -> String -> Value
produceUserJson (u@User{..}) uid baseUrl
  = Object
    $ insert "id" (String $ T.pack $ show uid)
    $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/users/" ++ (show uid)) ])
    $ fromObject $ toJSON u

produceUserReply :: User -> M.ObjectId -> String -> Value
produceUserReply (user@User{..}) uid baseUrl
  = object [ "user" .= produceUserJson user uid baseUrl ]

produceUsersReply :: [(M.ObjectId, User)] -> String -> Value
produceUsersReply users baseUrl
  = object [ "links" .= (object [ "next"     .= Null
                                , "previous" .= Null
                                , "self"     .= (baseUrl ++ "/v3/users")
                                ]
                        )
           , "users" .= usersEntry
           ]
  where
    usersEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\(i, s) -> produceUserJson s i) users


createUser :: MonadIO m => User -> M.Action m M.ObjectId
createUser u = do
  M.ObjId oid <- M.insert collectionName $ toBson u
  return oid

listUsers :: (MonadIO m, MonadBaseControl IO m) => M.Action m [(M.ObjectId, User)]
listUsers = do
  cursor <- M.find (M.select [] collectionName)
  docs <- M.rest cursor
  users <- mapM fromBson docs
  let ids = map ((\(M.ObjId i) -> i) . (M.valueAt "_id")) docs
  return $ zip ids users

findUserById :: (MonadIO m) => ObjectId -> M.Action m (Maybe User)
findUserById uid = runMaybeT $ do
  mUser <- MaybeT $ M.findOne (M.select ["_id" =: uid] collectionName)
  fromBson mUser

updateUser :: (MonadIO m)
           => M.ObjectId -> M.Document -> M.Action m (Maybe User)
updateUser uid userUpdate = do
  M.modify (M.select ["_id" =: uid] collectionName) [ "$set" =: userUpdate ]
  -- If the user is deleted between these commands we assume it's never been updated
  findUserById uid

deleteUser :: (MonadIO m) => ObjectId -> M.Action m Int
deleteUser uid = do
  M.delete $ M.select ["_id" =: uid] collectionName
  affectedDocs
