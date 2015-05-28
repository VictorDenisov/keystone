{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell #-}
module Model.Service
where

import Common (skipTickOptions, capitalize, fromObject, dropOptions)
import Common.Database (affectedDocs, pushC, setC, projectC, unwindC, idF)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Bson (Val(..), (=:), ObjectId)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Char (toLower)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)
import Language.Haskell.TH.Syntax (nameBase)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "service"

data Service = Service
             { description :: Maybe String
             , enabled     :: Bool
             , name        :: Maybe String
             , type'       :: ServiceType
             , endpoints   :: [Endpoint]
             } deriving (Show, Read, Eq, Ord, Typeable)

endpointsF = T.pack $ nameBase 'endpoints
endpointsA = (M.String $ '$' `T.cons` endpointsF) -- endpoints command for aggregation

data ServiceType = Identity
                 | Compute
                 | Image
                 | Volume
                 | Network
                   deriving (Show, Read, Eq, Ord, Typeable)

data Endpoint = Endpoint
              { einterface :: Interface
              , eurl       :: String
              , eenabled   :: Bool
              , eid        :: ObjectId
              } deriving (Show, Read, Eq, Ord, Typeable)

data Interface = Admin
               | Internal
               | Public
                 deriving (Show, Read, Eq, Ord, Typeable)

instance FromJSON ServiceType where
  parseJSON (String s) = case readMaybe $ capitalize $ T.unpack s of
                          Just v -> return v
                          Nothing -> fail $ "Unknown ServiceType " ++ (T.unpack s)
  parseJSON v = typeMismatch (nameBase ''ServiceType) v

instance ToJSON ServiceType where
  toJSON v = String $ T.pack $ map toLower $ show v

instance Val ServiceType where
  val st = M.String $ T.pack $ map toLower $ show st
  cast' (M.String s) = readMaybe $ capitalize $ T.unpack s
  cast' _ = Nothing

$(deriveBson id ''Service)

$(deriveJSON skipTickOptions ''Service)

instance FromJSON Interface where
  parseJSON (String s) = case readMaybe $ capitalize $ T.unpack s of
                          Just v -> return v
                          Nothing -> fail $ "Unknown Interface " ++ (T.unpack s)
  parseJSON v = typeMismatch (nameBase ''Interface) v

instance ToJSON Interface where
  toJSON v = String $ T.pack $ map toLower $ show v

instance Val Interface where
  val st = M.String $ T.pack $ map toLower $ show st
  cast' (M.String s) = readMaybe $ capitalize $ T.unpack s
  cast' _ = Nothing

$(deriveBson (drop 1) ''Endpoint)

$(deriveJSON (dropOptions 1) ''Endpoint)

produceServiceJson :: Service -> M.ObjectId -> String -> Value
produceServiceJson (s@Service{..}) oid baseUrl
      = Object
        $ insert "id" (String $ T.pack $ show oid)
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/services/" ++ (show oid)) ])
        $ fromObject $ toJSON s

produceServiceReply :: Service -> M.ObjectId -> String -> Value
produceServiceReply (service@Service{..}) oid baseUrl
      = object [ "service" .= produceServiceJson service oid baseUrl ]

produceEndpointJson :: Endpoint -> M.ObjectId -> String -> Value
produceEndpointJson (s@Endpoint{..}) serviceId baseUrl
      = Object
        -- Endpoint already has its own id in its structure
        $ insert "service_id" (String $ T.pack $ show serviceId)
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/endpoints/" ++ (show eid)) ])
        $ fromObject $ toJSON s

produceEndpointReply :: Endpoint -> M.ObjectId -> String -> Value
produceEndpointReply (endpoint@Endpoint{..}) serviceId baseUrl
      = object [ "endpoint" .= produceEndpointJson endpoint serviceId baseUrl ]

produceEndpointsReply :: [(M.ObjectId, Endpoint)] -> String -> Value
produceEndpointsReply endpoints baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/endpoints")
                                  ]
                          )
             , "endpoints" .= endpointsEntry
             ]
  where
    endpointsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\(i, s) -> produceEndpointJson s i) endpoints

produceServicesReply :: [(M.ObjectId, Service)] -> String -> Value
produceServicesReply services baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/services")
                                  ]
                          )
             , "services" .= servicesEntry
             ]
  where
    servicesEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\(i, s) -> produceServiceJson s i) services

createService :: MonadIO m => Service -> M.Action m M.ObjectId
createService s = do
  M.ObjId oid <- M.insert collectionName $ toBson s
  return oid

listServices :: (MonadIO m, MonadBaseControl IO m)
             => M.Action m [(M.ObjectId, Service)]
listServices = do
  cur <- M.find $ M.select [] collectionName
  docs <- M.rest cur
  services <- mapM fromBson docs
  let ids = map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
  return $ zip ids services

findServiceById :: (MonadIO m) => ObjectId -> M.Action m (Maybe Service)
findServiceById sid = runMaybeT $ do
  mService <- MaybeT $ M.findOne (M.select [idF =: sid] collectionName)
  fromBson mService

updateService :: (MonadIO m)
              => M.ObjectId -> M.Document -> M.Action m (Maybe Service)
updateService sid serviceUpdate = do
  M.modify (M.select [idF =: sid] collectionName) [ setC =: serviceUpdate ]
  -- If the service is deleted between these commands we assume it's never been updated
  -- TODO Remove this from here. It should be handled by a higher layer.
  findServiceById sid

deleteService :: (MonadIO m) => ObjectId -> M.Action m Int
deleteService sid = do
  M.delete $ M.select [idF =: sid] collectionName
  affectedDocs

addEndpoint :: (MonadIO m) => ObjectId -> Endpoint -> M.Action m (Maybe M.ObjectId)
addEndpoint sid endpoint = do
  M.modify (M.select [idF =: sid] collectionName) [ pushC =: [endpointsF =: (toBson endpoint)] ]
  count <- affectedDocs
  return $
    if count == 1
      then Just $ eid endpoint
      else Nothing

listEndpoints :: (MonadIO m) => M.Action m [(M.ObjectId, Endpoint)]
listEndpoints = do
  docs <- M.aggregate collectionName [ [projectC =: [endpointsF =: (M.Int32 1)]]
                                     , [unwindC  =: endpointsA]
                                     ]
  let eDocs = map ((\(M.Doc d) -> d) . (M.valueAt endpointsF)) docs
  endpoints <- mapM fromBson eDocs
  let serviceIds = map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
  return $ zip serviceIds endpoints
