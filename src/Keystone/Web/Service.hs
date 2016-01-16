{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Keystone.Web.Service
( module Keystone.Web.Service
, module Keystone.Web.Service.Types
) where

import Common ( skipTickOptions, dropOptions, fromObject, underscoreOptions
              , (<.>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), Value(..))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (object, typeMismatch, (.=))
import Data.HashMap.Strict (insert, delete)
import Data.Text (pack)
import Data.Vector (fromList)
import Keystone.Config (KeystoneConfig(database))
import Language.Haskell.TH.Syntax (nameBase)
import Model.Common (OpStatus(Success, NotFound))
import Keystone.Model.IdentityApi (IdentityApi)
import Network.HTTP.Types.Status (status200, status201, status204, status404)
import Web.Common ( UrlInfo(..), UrlBasedValue, parseId, withHostUrl
                  , parseRequest, ActionM, parseMaybeString)
import Keystone.Web.Service.Types

import qualified Data.Text as T
import qualified Database.MongoDB as M
import qualified Error as E
import qualified Model.Mongo.Common as CD
import qualified Keystone.Model.Service as MS
import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S

createServiceH :: (Functor m, MonadIO m, IdentityApi m)
              => AT.Policy -> KeystoneConfig -> ActionM m ()
createServiceH policy config = A.requireToken config $ \token ->
    A.authorize policy AT.AddService token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      (scr :: ServiceCreateRequest) <- parseRequest
      service <- liftIO $ newRequestToService scr
      sid <- liftIO $ CD.withDB (database config) $ MS.createService service
      S.status status201
      withHostUrl config $ produceServiceReply service

listServicesH :: (Functor m, MonadIO m, IdentityApi m)
               => AT.Policy -> KeystoneConfig -> ActionM m ()
listServicesH policy config = A.requireToken config $ \token -> do
    serviceName <- parseMaybeString "name"
    A.authorize policy AT.ListServices token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      services <- liftIO $ CD.withDB (database config) $ MS.listServices serviceName
      S.status status200
      withHostUrl config $ produceServicesReply services

serviceDetailsH :: (Functor m, MonadIO m, IdentityApi m)
               => AT.Policy -> KeystoneConfig -> ActionM m ()
serviceDetailsH policy config = A.requireToken config $ \token -> do
    (sid :: M.ObjectId) <- parseId "sid"
    A.authorize policy AT.ShowServiceDetails token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      mService <- liftIO $ CD.withDB (database config) $ MS.findServiceById sid
      case mService of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Service not found"
        Just service -> do
            S.status status200
            withHostUrl config $ produceServiceReply service

updateServiceH :: (Functor m, MonadIO m, IdentityApi m)
              => AT.Policy -> KeystoneConfig -> ActionM m ()
updateServiceH policy config = A.requireToken config $ \token -> do
    (sid :: M.ObjectId) <- parseId "sid"
    (sur :: ServiceUpdateRequest) <- parseRequest
    A.authorize policy AT.UpdateService token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      mService <- liftIO $ CD.withDB (database config) $ MS.updateService sid (updateRequestToDocument sur)
      case mService of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Service not found"
        Just service -> do
          S.status status200
          withHostUrl config $ produceServiceReply service

deleteServiceH :: (Functor m, MonadIO m, IdentityApi m)
              => AT.Policy -> KeystoneConfig -> ActionM m ()
deleteServiceH policy config = A.requireToken config $ \token -> do
    (sid :: M.ObjectId) <- parseId "sid"
    A.authorize policy AT.DeleteService token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      n <- liftIO $ CD.withDB (database config) $ MS.deleteService sid
      case n of
        Success -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find service, " ++ (show sid) ++ "."
          S.status status404

createEndpointH :: (Functor m, MonadIO m, IdentityApi m)
               => AT.Policy -> KeystoneConfig -> ActionM m ()
createEndpointH policy config = A.requireToken config $ \token -> do
    (ecr :: EndpointCreateRequest) <- parseRequest
    endpoint <- liftIO $ newRequestToEndpoint ecr
    A.authorize policy AT.AddEndpoint token AT.EmptyResource $ do
      mEid <- liftIO $ CD.withDB (database config) $ MS.addEndpoint (eserviceId ecr) endpoint
      case mEid of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Service not found"
        Just _eid -> do
          S.status status201
          withHostUrl config $ produceEndpointReply endpoint (eserviceId ecr)

listEndpointsH :: (Functor m, MonadIO m, IdentityApi m)
              => AT.Policy -> KeystoneConfig -> ActionM m ()
listEndpointsH policy config = A.requireToken config $ \token -> do
    A.authorize policy AT.ListEndpoints token AT.EmptyResource $ do
      endpoints <- liftIO $ CD.withDB (database config) $ MS.listEndpoints
      S.status status200
      withHostUrl config $ produceEndpointsReply endpoints

endpointDetailsH :: (Functor m, MonadIO m, IdentityApi m)
                => AT.Policy -> KeystoneConfig -> ActionM m ()
endpointDetailsH policy config = A.requireToken config $ \token -> do
    (eid :: M.ObjectId) <- parseId "eid"
    A.authorize policy AT.ShowEndpoint token AT.EmptyResource $ do
      mEndpoint <- liftIO $ CD.withDB (database config) $ MS.findEndpointById eid
      case mEndpoint of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Endpoint not found"
        Just (serviceId, endpoint) -> do
          S.status status200
          withHostUrl config $ produceEndpointReply endpoint serviceId

deleteEndpointH :: (Functor m, MonadIO m, IdentityApi m)
               => AT.Policy -> KeystoneConfig -> ActionM m ()
deleteEndpointH policy config = A.requireToken config $ \token -> do
    (eid :: M.ObjectId) <- parseId "eid"
    A.authorize policy AT.DeleteEndpoint token AT.EmptyResource $ do
      n <- liftIO $ CD.withDB (database config) $ MS.deleteEndpoint eid
      case n of
        Success -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find endpoint, " ++ (show eid) ++ "."
          S.status status404

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
