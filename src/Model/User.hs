{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
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
import Language.Haskell.TH.Syntax (nameBase)
import Model.Common (CaptureStatus(..), TransactionId(..), OpStatus(..))
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

-- aux fields
pendingTransactions = "pendingTransactions"
refCount = "refCount"

newtype UserId = UserId M.ObjectId

$(deriveBson id ''User)

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

listUsers :: (MonadIO m, MonadBaseControl IO m) => (Maybe String) -> M.Action m [(M.ObjectId, User)]
listUsers mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cursor <- M.find (M.select nameFilter collectionName)
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

getRefCount :: M.Document -> Int
getRefCount doc = case (M.look refCount doc) of
  Nothing -> 0
  Just (M.Int32 v) -> fromIntegral v
  Just _ -> 1

getPendingTransactions :: M.Document -> [M.Value]
getPendingTransactions doc =
  case M.look pendingTransactions doc of
    Nothing -> []
    Just (M.Array xs) -> xs
    Just v -> error $ (T.unpack pendingTransactions) ++ " field in user should be an array."

deleteUser :: (MonadIO m) => ObjectId -> M.Action m OpStatus
deleteUser uid = do
  mUserDoc <- M.findOne (M.select ["_id" =: uid] collectionName)
  case mUserDoc of
    Nothing -> return NotFound
    Just userDoc ->
      if ((getRefCount userDoc) /= 0) || (not $ null $ getPendingTransactions userDoc)
        then
          return Busy
        else do
          M.delete $ M.select ["_id" =: uid] collectionName
          ad <- affectedDocs
          if ad == 0
            then return NotFound
            else return Success

captureUser :: (MonadIO m) => M.ObjectId -> TransactionId -> M.Action m CaptureStatus
captureUser uid (TransactionId tid) = do
  M.modify (M.select ["_id" =: uid, pendingTransactions =: ["$ne" =: tid] ] collectionName)
                                      [ "$inc"  =: [refCount =: (M.Int32 1)]
                                      , "$push" =: [pendingTransactions =: tid]
                                      ]
  count <- affectedDocs
  if count == 1
    then return Captured
    else return CaptureFailed

rollbackCaptureUser :: (MonadIO m) => M.ObjectId -> TransactionId -> M.Action m CaptureStatus
rollbackCaptureUser uid (TransactionId tid) = do
  M.modify (M.select ["_id" =: uid, pendingTransactions =: tid] collectionName)
                                      [ "$dec"  =: [refCount =: (M.Int32 1)]
                                      , "$pull" =: [pendingTransactions =: tid]
                                      ]
  count <- affectedDocs
  if count == 1
    then return Captured
    else return CaptureFailed

pullTransaction :: (MonadIO m) => M.ObjectId -> TransactionId -> M.Action m ()
pullTransaction uid (TransactionId tid) = do
  M.modify (M.select ["_id" =: uid, pendingTransactions =: tid] collectionName)
                                      ["$pull" =: [pendingTransactions =: tid]]

