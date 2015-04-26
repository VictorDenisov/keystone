{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Service
where

import Control.Applicative ((<*>), (<$>))
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types (typeMismatch)
import Data.Data (Typeable)
import Data.Text (pack, unpack)
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Model.Service as MS

data ServiceCreateRequest = ServiceCreateRequest
                          { description :: Maybe String
                          , enabled     :: Maybe Bool
                          , name        :: Maybe String
                          , stype       :: MS.ServiceType
                          } deriving (Show, Read, Eq, Ord, Typeable)

data ServiceUpdateRequest = ServiceUpdateRequest
                          { udescription :: Maybe String
                          , uenabled     :: Maybe Bool
                          , uname        :: Maybe String
                          , utype        :: Maybe MS.ServiceType
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

instance FromJSON ServiceUpdateRequest where
  parseJSON (Object v) = do
    service <- v .: "service"
    ServiceUpdateRequest
        <$> (service .:? "description")
        <*> (service .:? "enabled")
        <*> (service .:? "name")
        <*> (service .:? "type")
  parseJSON v = typeMismatch "ServiceUpdateRequest" v

newRequestToService :: ServiceCreateRequest -> MS.Service
newRequestToService ServiceCreateRequest{..} =
  MS.Service description (maybe True id enabled) name stype

updateRequestToDocument :: ServiceUpdateRequest -> M.Document
updateRequestToDocument ServiceUpdateRequest{..} = concat
  [ (pack $ nameBase 'MS.description) M.=? udescription
  , (pack $ nameBase 'MS.enabled)     M.=? uenabled
  , (pack $ nameBase 'MS.name)        M.=? uname
  , (pack $ nameBase 'MS.type')       M.=? utype
  ]
