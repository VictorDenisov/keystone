{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Service
where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (typeMismatch)
import Data.Data (Typeable)
import Data.Text (unpack)

import qualified Model.Service as MS

data ServiceCreateRequest = ServiceCreateRequest
                          { description :: Maybe String
                          , enabled     :: Maybe Bool
                          , name        :: Maybe String
                          , stype       :: MS.ServiceType
                          } deriving (Show, Read, Eq, Ord, Typeable)

instance FromJSON ServiceCreateRequest where
  parseJSON (Object v) = do
    service <- v .: "service"
    ServiceCreateRequest
        <$> (service .:? "description")
        <*> (service .:? "enabled")
        <*> (service .:? "name")
        <*> (service .:  "type")
  parseJSON v = typeMismatch "ServiceCreateRequest" v

newRequestToService :: ServiceCreateRequest -> MS.Service
newRequestToService ServiceCreateRequest{..} =
  MS.Service description (maybe True id enabled) name stype
