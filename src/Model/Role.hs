{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Role
where

import Common (fromObject, skipUnderscoreOptions, UrlBasedValue, UrlInfo(..))
import Common.Database (affectedDocs, idF)

import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Aeson (ToJSON(..), Value(..))
import Data.Aeson.Types (object, (.=))
import Data.Aeson.TH (deriveJSON)
import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)

import Language.Haskell.TH.Syntax (nameBase)

import Model.Common (OpStatus(NotFound, Success), listExistingIds)

import qualified Error as E
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Admin as MA
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "role"

data Role = Role
          { _id         :: M.ObjectId
          , name        :: String
          , description :: Maybe String
          , enabled     :: Bool
          } deriving (Show, Read, Eq, Ord, Typeable)

newtype RoleId = RoleId M.ObjectId
                 deriving (Show, Typeable, Eq)

instance M.Val RoleId where
  val (RoleId rid) = M.ObjId rid
  cast' (M.ObjId rid) = return $ RoleId rid
  cast' _ = Nothing

$(deriveBson id ''Role)

$(deriveJSON skipUnderscoreOptions ''Role)

produceRoleJson :: Role -> String -> Value
produceRoleJson (role@Role{..}) baseUrl
      = Object
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/roles/" ++ (show _id)) ])
        $ fromObject $ toJSON role

produceRoleReply :: Role -> UrlBasedValue
produceRoleReply role (UrlInfo {baseUrl})
      = object [ "role" .= produceRoleJson role baseUrl ]

produceRolesReply :: [Role] -> UrlBasedValue
produceRolesReply roles (UrlInfo {baseUrl, path, query})
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ path ++ query)
                                  ]
                          )
             , "roles" .= rolesEntry
             ]
  where
    rolesEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceRoleJson roles

createRole :: Role -> M.Action IO (Either E.Error M.ObjectId)
createRole r = (do
    M.ObjId rid <- M.insert collectionName $ toBson r
    return $ Right rid
  ) `catch` (\f -> do
    case f of
      M.WriteFailure duplicateE message ->
          return $ Left $ E.conflict $ "Insert of role with the duplicate " ++ (nameBase 'name) ++ " is not allowed."
      _ -> throwM f
  )

listRoles :: (Maybe String) -> M.Action IO [Role]
listRoles mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cur <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cur
  mapM fromBson docs

findRoleById :: M.ObjectId -> M.Action IO (Maybe Role)
findRoleById rid = runMaybeT $ do
  mRole <- MaybeT $ M.findOne (M.select [idF =: rid] collectionName)
  fromBson mRole

deleteRole :: M.ObjectId -> M.Action IO OpStatus
deleteRole rid = do
  M.delete $ M.select [idF =: rid] collectionName
  ad <- affectedDocs
  if ad == 0
    then return NotFound
    else return Success

listExistingRoleIds :: [M.ObjectId] -> M.Action IO [M.ObjectId]
listExistingRoleIds = listExistingIds collectionName

verifyDatabase :: M.Action IO ()
verifyDatabase = do
  MA.ensureIndex $ (MA.index
                          collectionName
                          [(T.pack $ nameBase 'name) =: (M.Int32 1)])
                      {MA.iUnique = True}
  roles <- listRoles Nothing
  return ()
