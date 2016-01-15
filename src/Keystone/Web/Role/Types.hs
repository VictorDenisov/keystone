{-# LANGUAGE DeriveDataTypeable #-}
module Keystone.Web.Role.Types
where

import Data.Data (Typeable)

data RoleCreateRequest = RoleCreateRequest
                          { description :: Maybe String
                          , name        :: String
                          , enabled     :: Maybe Bool
                          } deriving (Show, Read, Eq, Ord, Typeable)
