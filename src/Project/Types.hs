{-# Language DeriveDataTypeable #-}
module Project.Types
where

import Data.Data (Typeable)

import qualified Database.MongoDB as M

data ProjectCreateRequest = ProjectCreateRequest
                          { description :: Maybe String
                          , domainId    :: Maybe String
                          , name        :: String
                          , enabled     :: Maybe Bool
                          } deriving (Show, Read, Eq, Ord, Typeable)
