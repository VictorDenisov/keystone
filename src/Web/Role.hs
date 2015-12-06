{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Role
( module Web.Role
, module Web.Role.Types
) where

import Common (fromObject, underscoreOptions)
import Data.Aeson (FromJSON(..), Value(..), (.:), ToJSON(..), Value(..))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (object, typeMismatch, (.=))
import Data.HashMap.Strict (insert)
import Data.Maybe (fromMaybe)
import Data.Vector (fromList)
import Language.Haskell.TH.Syntax (nameBase)
import Web.Common (UrlBasedValue, UrlInfo(..))
import Web.Role.Types

import qualified Database.MongoDB as M
import qualified Model.Role as MR

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

