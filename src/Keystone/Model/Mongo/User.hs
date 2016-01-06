{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Keystone.Model.Mongo.User
where

import Model.Mongo.Common (affectedDocs, idF, setC)
import Control.Applicative ((<$>))
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.PasswordStore (verifyPassword)

import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..))
import Data.ByteString.Char8 (pack)
import Data.Maybe (listToMaybe)

import Language.Haskell.TH.Syntax (nameBase)

import Model.Common (OpStatus(..), listExistingIds)
import Keystone.Model.User (User(..))

import qualified Error as E
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Admin as MA
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "user"

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

findUserById :: M.ObjectId -> M.Action IO (Maybe User)
findUserById uid = runMaybeT $ do
  mUser <- MaybeT $ M.findOne (M.select [idF =: uid] collectionName)
  fromBson mUser

updateUser :: M.ObjectId -> M.Document -> M.Action IO (Maybe User)
updateUser uid userUpdate = do
  res <- M.findAndModify (M.select [idF =: uid] collectionName) [ setC =: userUpdate ]
  case res of
    Left _  -> return Nothing
    Right v -> Just <$> fromBson v

deleteUser :: M.ObjectId -> M.Action IO OpStatus
deleteUser uid = do
  M.delete $ M.select [idF =: uid] collectionName
  ad <- affectedDocs
  if ad == 0
    then return NotFound
    else return Success

listExistingUserIds :: [M.ObjectId] -> M.Action IO [M.ObjectId]
listExistingUserIds = listExistingIds collectionName

checkUserPassword :: Maybe M.ObjectId
                  -> Maybe String
                  -> String
                  -> M.Action IO (Either String User)
checkUserPassword mUserId mUserName passwordToCheck = do
  mu <- case mUserId of
            Just userId -> findUserById userId
            Nothing -> do
              users <- listUsers mUserName
              return $ listToMaybe users
  case mu of
    Nothing -> return $ Left "User is not found."
    Just user  ->
      case password user of
        Just p ->
          if verifyPassword (pack passwordToCheck) (pack p)
            then return $ Right user
            else return $ Left "The password if incorrect"
        Nothing -> return $ Left "User exists, but doesn't have any password."

verifyDatabase :: M.Action IO ()
verifyDatabase = do
  MA.ensureIndex $ (MA.index
                          collectionName
                          [(T.pack $ nameBase 'name) =: (M.Int32 1)])
                      {MA.iUnique = True} -- TODO this should be modified as soon as domains are implemented
                      -- different domains can have users with the same name
  users <- listUsers Nothing
  return ()
