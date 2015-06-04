{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
module Model.User
where

import Common (fromObject, skipUnderscoreOptions)
import Common.Database (affectedDocs, decC, idF, inC, incC, neC, pullC, pushC, setC)
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

data User = User { _id :: M.ObjectId
                 , description :: Maybe String
                 , email :: Maybe String
                 , enabled :: Bool
                 , name :: String
                 , password :: Maybe String -- hash of the password
                 } deriving (Show, Read, Eq, Ord, Typeable)

newtype UserId = UserId M.ObjectId
                 deriving (Show, Typeable, Eq)

instance M.Val UserId where
  val (UserId uid) = M.String $ T.pack $ show uid
  cast' (M.String s) = do
    uid <- readMaybe $ T.unpack s
    return $ UserId uid
  cast' _ = Nothing

$(deriveBson id ''User)

$(deriveJSON skipUnderscoreOptions ''User)

produceUserJson :: User -> String -> Value
produceUserJson (u@User{..}) baseUrl
  = Object
    $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/users/" ++ (show _id)) ])
    $ fromObject $ toJSON u

produceUserReply :: User -> String -> Value
produceUserReply (user@User{..}) baseUrl
  = object [ "user" .= produceUserJson user baseUrl ]

produceUsersReply :: [User] -> String -> Value
produceUsersReply users baseUrl
  = object [ "links" .= (object [ "next"     .= Null
                                , "previous" .= Null
                                , "self"     .= (baseUrl ++ "/v3/users")
                                ]
                        )
           , "users" .= usersEntry
           ]
  where
    usersEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceUserJson users


createUser :: MonadIO m => User -> M.Action m M.ObjectId
createUser u = do
  M.ObjId oid <- M.insert collectionName $ toBson u
  return oid

listUsers :: (MonadIO m, MonadBaseControl IO m)
          => (Maybe String) -> M.Action m [User]
listUsers mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cursor <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cursor
  mapM fromBson docs

findUserById :: (MonadIO m) => ObjectId -> M.Action m (Maybe User)
findUserById uid = runMaybeT $ do
  mUser <- MaybeT $ M.findOne (M.select [idF =: uid] collectionName)
  fromBson mUser

updateUser :: (MonadIO m)
           => M.ObjectId -> M.Document -> M.Action m (Maybe User)
updateUser uid userUpdate = do
  M.modify (M.select [idF =: uid] collectionName) [ setC =: userUpdate ]
  -- If the user is deleted between these commands we assume it's never been updated
  findUserById uid

deleteUser :: (MonadIO m) => ObjectId -> M.Action m OpStatus
deleteUser uid = do
  M.delete $ M.select [idF =: uid] collectionName
  ad <- affectedDocs
  if ad == 0
    then return NotFound
    else return Success

listExistingUserIds :: (MonadIO m, MonadBaseControl IO m)
                    => [M.ObjectId] -> M.Action m [M.ObjectId]
listExistingUserIds userIds = do
  cur <- M.find (M.select [ idF =: [inC =: (M.Array $ map M.ObjId userIds)] ] collectionName)
  docs <- M.rest cur
  return $ map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
