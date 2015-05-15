{-# Language DeriveDataTypeable #-}
module Role.Types
where

import Data.Data (Typeable)

import qualified Database.MongoDB as M

data RoleCreateRequest = RoleCreateRequest
                          { description :: Maybe String
                          , name        :: String
                          , enabled     :: Maybe Bool
                          } deriving (Show, Read, Eq, Ord, Typeable)
