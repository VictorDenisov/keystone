{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveDataTypeable #-}
module Model.User
where

import Common (fromObject, skipUnderscoreOptions, UrlBasedValue)
import Common.Database (affectedDocs, idF, setC)

import Control.Applicative ((<$>))
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Aeson (ToJSON(..), Value(..))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (object, (.=))
import Data.Bson ((=:), ObjectId)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)

import Language.Haskell.TH.Syntax (nameBase)

import Model.Common (OpStatus(..), listExistingIds)

import qualified Error as E
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Admin as MA
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
  val (UserId uid) = M.ObjId uid
  cast' (M.ObjId uid) = return $ UserId uid
  cast' _ = Nothing

$(deriveBson id ''User)

$(deriveJSON skipUnderscoreOptions ''User)

produceUserJson :: User -> String -> Value
produceUserJson (u@User{..}) baseUrl
  = Object
    $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/users/" ++ (show _id)) ])
    $ fromObject $ toJSON u

produceUserReply :: User -> UrlBasedValue
produceUserReply (user@User{..}) baseUrl
  = object [ "user" .= produceUserJson user baseUrl ]

produceUsersReply :: [User] -> String -> UrlBasedValue
produceUsersReply users queryString baseUrl
  = object [ "links" .= (object [ "next"     .= Null
                                , "previous" .= Null
                                , "self"     .= (baseUrl ++ "/v3/users" ++ queryString)
                                ]
                        )
           , "users" .= usersEntry
           ]
  where
    usersEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceUserJson users


createUser :: User -> M.Action IO (Either E.Error M.ObjectId)
createUser u = (do
    M.ObjId oid <- M.insert collectionName $ toBson u
    return $ Right oid
  ) `catch` (\f -> do
    case f of
      M.WriteFailure duplicateE message ->
          return $ Left $ E.conflict $ "Insert of user with the duplicate " ++ (nameBase 'name) ++ " is not allowed."
      _ -> throwM f
  )

listUsers :: (Maybe String) -> M.Action IO [User]
listUsers mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cursor <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cursor
  mapM fromBson docs

findUserById :: ObjectId -> M.Action IO (Maybe User)
findUserById uid = runMaybeT $ do
  mUser <- MaybeT $ M.findOne (M.select [idF =: uid] collectionName)
  fromBson mUser

updateUser :: M.ObjectId -> M.Document -> M.Action IO (Maybe User)
updateUser uid userUpdate = do
  res <- M.findAndModify (M.select [idF =: uid] collectionName) [ setC =: userUpdate ]
  case res of
    Left _  -> return Nothing
    Right v -> Just <$> fromBson v

deleteUser :: ObjectId -> M.Action IO OpStatus
deleteUser uid = do
  M.delete $ M.select [idF =: uid] collectionName
  ad <- affectedDocs
  if ad == 0
    then return NotFound
    else return Success

listExistingUserIds :: [M.ObjectId] -> M.Action IO [M.ObjectId]
listExistingUserIds = listExistingIds collectionName

verifyDatabase :: M.Action IO ()
verifyDatabase = do
  MA.ensureIndex $ (MA.index
                          collectionName
                          [(T.pack $ nameBase 'name) =: (M.Int32 1)])
                      {MA.iUnique = True} -- TODO this should be modified as soon as domains are implemented
                      -- different domains can have users with the same name
  users <- listUsers Nothing
  return ()
