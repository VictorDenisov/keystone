{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Role
where

import Common (capitalize, fromObject, loggerName, skipUnderscoreOptions)
import Common.Database (affectedDocs, decC, idF, inC, incC, neC, pullC, pushC)

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
import Language.Haskell.TH.Syntax (nameBase)
import Model.Common (TransactionId(..), CaptureStatus(..))
import System.Log.Logger (criticalM)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
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

produceRoleReply :: Role -> String -> Value
produceRoleReply role baseUrl
      = object [ "role" .= produceRoleJson role baseUrl ]

produceRolesReply :: [Role] -> String -> Value
produceRolesReply roles baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/roles")
                                  ]
                          )
             , "roles" .= rolesEntry
             ]
  where
    rolesEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceRoleJson roles

createRole :: MonadIO m => Role -> M.Action m M.ObjectId
createRole r = do
  M.ObjId rid <- M.insert collectionName $ toBson r
  return rid

listRoles :: (MonadIO m, MonadBaseControl IO m)
          => (Maybe String) -> M.Action m [Role]
listRoles mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cur <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cur
  mapM fromBson docs

findRoleById :: (MonadIO m) => M.ObjectId -> M.Action m (Maybe Role)
findRoleById rid = runMaybeT $ do
  mRole <- MaybeT $ M.findOne (M.select [idF =: rid] collectionName)
  fromBson mRole

listExistingRoleIds :: (MonadIO m, MonadBaseControl IO m)
                    => [M.ObjectId] -> M.Action m [M.ObjectId]
listExistingRoleIds roleIds = do
  cur <- M.find (M.select [ idF =: [inC =: (M.Array $ map M.ObjId roleIds)] ] collectionName)
  docs <- M.rest cur
  return $ map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
