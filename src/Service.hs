{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Service
( module Service
, module Service.Types
) where

import Common (skipTickOptions, dropOptions, underscoreOptions, (<.>))
import Service.Types
import Data.Aeson ( FromJSON(..), (.:), Value(..))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Text (pack)
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Model.Service as MS

newRequestToService :: ServiceCreateRequest -> IO MS.Service
newRequestToService ServiceCreateRequest{..} = do
  serviceId <- M.genObjectId
  return $ MS.Service serviceId description (maybe True id enabled) name type' []

newRequestToEndpoint :: EndpointCreateRequest -> IO MS.Endpoint
newRequestToEndpoint EndpointCreateRequest{..} = do
  endpointId <- M.genObjectId
  return $ MS.Endpoint einterface eurl (maybe True id eenabled) endpointId

updateRequestToDocument :: ServiceUpdateRequest -> M.Document
updateRequestToDocument ServiceUpdateRequest{..} = concat
  [ (pack $ nameBase 'MS.description) M.=? udescription
  , (pack $ nameBase 'MS.enabled)     M.=? uenabled
  , (pack $ nameBase 'MS.name)        M.=? uname
  , (pack $ nameBase 'MS.type')       M.=? utype
  ]

instance FromJSON ServiceCreateRequest where
  parseJSON (Object v) = do
    service <- v .: "service"
    parseScr service
  parseJSON v = typeMismatch (nameBase ''ServiceCreateRequest) v

instance FromJSON ServiceUpdateRequest where
  parseJSON (Object v) = do
    service <- v .: "service"
    parseSur service
  parseJSON v = typeMismatch (nameBase ''ServiceUpdateRequest) v

instance FromJSON EndpointCreateRequest where
  parseJSON (Object v) = do
    endpoint <- v .: "endpoint"
    parseEcr endpoint
  parseJSON v = typeMismatch (nameBase ''EndpointCreateRequest) v

parseScr = $(mkParseJSON skipTickOptions ''ServiceCreateRequest)

parseSur = $(mkParseJSON (dropOptions 1) ''ServiceUpdateRequest)

parseEcr = $(mkParseJSON (dropOptions 1 <.> underscoreOptions) ''EndpointCreateRequest)
