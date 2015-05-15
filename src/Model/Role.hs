{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Role
where

import Common (capitalize, fromObject)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "role"

data Role = Role
          { name        :: String
          , description :: Maybe String
          , enabled     :: Bool
          } deriving (Show, Read, Eq, Ord, Typeable)

$(deriveBson id ''Role)

$(deriveJSON defaultOptions ''Role)

produceRoleJson :: Role -> M.ObjectId -> String -> Value
produceRoleJson (role@Role{..}) rid baseUrl
      = Object
        $ insert "id" (String $ T.pack $ show rid)
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/roles/" ++ (show rid)) ])
        $ fromObject $ toJSON role

produceRoleReply :: Role -> M.ObjectId -> String -> Value
produceRoleReply role rid baseUrl
      = object [ "role" .= produceRoleJson role rid baseUrl ]

produceRolesReply :: [(M.ObjectId, Role)] -> String -> Value
produceRolesReply roles baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/roles")
                                  ]
                          )
             , "roles" .= rolesEntry
             ]
  where
    rolesEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\(i, s) -> produceRoleJson s i) roles

createRole :: MonadIO m => Role -> M.Action m M.ObjectId
createRole r = do
  M.ObjId rid <- M.insert collectionName $ toBson r
  return rid

listRoles :: (MonadIO m, MonadBaseControl IO m)
          => M.Action m [(M.ObjectId, Role)]
listRoles = do
  cur <- M.find $ M.select [] collectionName
  docs <- M.rest cur
  roles <- mapM fromBson docs
  let ids = map ((\(M.ObjId i) -> i) . (M.valueAt "_id")) docs
  return $ zip ids roles

findRoleById :: (MonadIO m) => M.ObjectId -> M.Action m (Maybe Role)
findRoleById rid = runMaybeT $ do
  mRole <- MaybeT $ M.findOne (M.select ["_id" =: rid] collectionName)
  fromBson mRole
