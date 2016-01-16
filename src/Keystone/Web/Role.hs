{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Keystone.Web.Role
( module Keystone.Web.Role
, module Keystone.Web.Role.Types
) where

import Common (fromObject, underscoreOptions)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON(..), Value(..), (.:), ToJSON(..), Value(..))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (object, typeMismatch, (.=))
import Data.HashMap.Strict (insert)
import Data.Maybe (fromMaybe)
import Data.Vector (fromList)
import Language.Haskell.TH.Syntax (nameBase)
import Model.Common (OpStatus(Success, NotFound))
import Keystone.Config (KeystoneConfig(..))
import Keystone.Model.IdentityApi (IdentityApi(..))
import Network.HTTP.Types.Status ( status200, status201, status204, status404)
import Web.Common ( UrlBasedValue, UrlInfo(..), parseId, parseRequest
                  , withHostUrl, parseMaybeString, ActionM)
import Keystone.Web.Role.Types

import qualified Database.MongoDB as M
import qualified Error as E
import qualified Model.Mongo.Common as CD
import qualified Keystone.Model.Role as MR
import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S

createRoleH :: (Functor m, MonadIO m, IdentityApi m)
            => AT.Policy -> KeystoneConfig -> ActionM m ()
createRoleH policy config = A.requireToken config $ \token -> do
    (rcr :: RoleCreateRequest) <- parseRequest
    role <- liftIO $ newRequestToRole rcr
    A.authorize policy AT.AddRole token AT.EmptyResource $ do
      mRid <- liftIO $ liftIO $ CD.withDB (database config) $ MR.createRole role
      case mRid of
        Left err -> do
          S.json err
          S.status $ E.code err
        Right rid -> do
          S.status status201
          withHostUrl config $ produceRoleReply role

listRolesH :: (Functor m, MonadIO m, IdentityApi m)
            => AT.Policy -> KeystoneConfig -> ActionM m ()
listRolesH policy config = A.requireToken config $ \token -> do
    roleName <- parseMaybeString "name"
    A.authorize policy AT.ListRoles token AT.EmptyResource $ do
      roles <- liftIO $ CD.withDB (database config) $ MR.listRoles roleName
      S.status status200
      withHostUrl config $ produceRolesReply roles

roleDetailsH :: (Functor m, MonadIO m, IdentityApi m)
             => AT.Policy -> KeystoneConfig -> ActionM m ()
roleDetailsH policy config = A.requireToken config $ \token -> do
    (rid :: M.ObjectId) <- parseId "rid"
    A.authorize policy AT.ShowRoleDetails token AT.EmptyResource $ do
      mRole <- liftIO $ CD.withDB (database config) $ MR.findRoleById rid
      case mRole of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Role not found"
        Just role -> do
          S.status status200
          withHostUrl config $ produceRoleReply role

deleteRoleH :: (Functor m, MonadIO m, IdentityApi m)
             => AT.Policy -> KeystoneConfig -> ActionM m ()
deleteRoleH policy config = A.requireToken config $ \token -> do
    (rid :: M.ObjectId) <- parseId "rid"
    A.authorize policy AT.DeleteRole token AT.EmptyResource $ do
      st <- liftIO $ CD.withDB (database config) $ MR.deleteRole rid
      case st of
        Success  -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find role, " ++ (show rid) ++ "."
          S.status status404

newRequestToRole :: RoleCreateRequest -> IO MR.Role
newRequestToRole RoleCreateRequest{..} = do
  roleId <- M.genObjectId
  return $ MR.Role roleId name description (fromMaybe True enabled)

instance FromJSON RoleCreateRequest where
  parseJSON (Object v) = do
    role <- v .: "role"
    parseRcr role
  parseJSON v = typeMismatch (nameBase ''RoleCreateRequest) v

parseRcr = $(mkParseJSON underscoreOptions ''RoleCreateRequest)

produceRoleJson :: MR.Role -> String -> Value
produceRoleJson (role@MR.Role{..}) baseUrl
      = Object
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/roles/" ++ (show _id)) ])
        $ fromObject $ toJSON role

produceRoleReply :: MR.Role -> UrlBasedValue
produceRoleReply role (UrlInfo {baseUrl})
      = object [ "role" .= produceRoleJson role baseUrl ]

produceRolesReply :: [MR.Role] -> UrlBasedValue
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

