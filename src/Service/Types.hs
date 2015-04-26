{-# Language DeriveDataTypeable #-}
module Service.Types
where

import Data.Data (Typeable)

import qualified Model.Service as MS

data ServiceCreateRequest = ServiceCreateRequest
                          { description :: Maybe String
                          , enabled     :: Maybe Bool
                          , name        :: Maybe String
                          , type'       :: MS.ServiceType
                          } deriving (Show, Read, Eq, Ord, Typeable)

data ServiceUpdateRequest = ServiceUpdateRequest
                          { udescription :: Maybe String
                          , uenabled     :: Maybe Bool
                          , uname        :: Maybe String
                          , utype        :: Maybe MS.ServiceType
                          } deriving (Show, Read, Eq, Ord, Typeable)
