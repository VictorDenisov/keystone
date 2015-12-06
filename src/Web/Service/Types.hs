{-# LANGUAGE DeriveDataTypeable #-}
module Web.Service.Types
where

import Data.Data (Typeable)

import qualified Model.Service as MS
import qualified Database.MongoDB as M

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

data EndpointCreateRequest = EndpointCreateRequest
                           { einterface :: MS.Interface
                           , ename      :: Maybe String
                           , eregion    :: Maybe String
                           , eurl       :: String
                           , eserviceId :: M.ObjectId
                           , eenabled   :: Maybe Bool
                           } deriving (Show, Read, Eq, Ord, Typeable)
