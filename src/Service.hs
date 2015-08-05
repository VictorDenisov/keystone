{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Service
( module Service
, module Service.Types
) where

import Common ( skipTickOptions, dropOptions, fromObject, underscoreOptions
              , (<.>), UrlInfo(..), UrlBasedValue)
import Service.Types
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), Value(..))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (object, typeMismatch, (.=))
import Data.HashMap.Strict (insert, delete)
import Data.Text (pack)
import Data.Vector (fromList)
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Model.Service as MS
import qualified Data.Text as T

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

produceServiceJson :: MS.Service -> String -> Value
produceServiceJson (s@MS.Service{_id}) baseUrl
      = Object
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/services/" ++ (show _id)) ])
        $ delete (T.pack $ nameBase 'MS.endpoints)
        $ fromObject $ toJSON s

produceServiceReply :: MS.Service -> UrlBasedValue
produceServiceReply (service@MS.Service{..}) (UrlInfo {baseUrl})
      = object [ "service" .= produceServiceJson service baseUrl ]

produceEndpointJson :: MS.Endpoint -> M.ObjectId -> String -> Value
produceEndpointJson (s@MS.Endpoint{..}) serviceId baseUrl
      = Object
        -- Endpoint already has its own id in its structure
        $ insert "service_id" (String $ T.pack $ show serviceId)
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/endpoints/" ++ (show eid)) ])
        $ fromObject $ toJSON s

produceEndpointReply :: MS.Endpoint -> M.ObjectId -> UrlBasedValue
produceEndpointReply (endpoint@MS.Endpoint{..}) serviceId (UrlInfo {baseUrl})
      = object [ "endpoint" .= produceEndpointJson endpoint serviceId baseUrl ]

produceEndpointsReply :: [(M.ObjectId, MS.Endpoint)] -> UrlBasedValue
produceEndpointsReply endpoints (UrlInfo {baseUrl, path, query})
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ path ++ query)
                                  ]
                          )
             , "endpoints" .= endpointsEntry
             ]
  where
    endpointsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\(i, s) -> produceEndpointJson s i) endpoints

produceServicesReply :: [MS.Service] -> UrlBasedValue
produceServicesReply services (UrlInfo {baseUrl, path, query})
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ path ++ query)
                                  ]
                          )
             , "services" .= servicesEntry
             ]
  where
    servicesEntry = Array $ fromList
                              $ map (\f -> f baseUrl)
                                  $ map produceServiceJson services

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
