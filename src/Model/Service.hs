{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell #-}
module Model.Service
where

import Common ( capitalize, dropOptions, fromObject, skipTickOptions
              , skipUnderscoreOptions, (<.>))
import Common.Database ( affectedDocs, pushC, setC, projectC, unwindC, idF
                       , (+++))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad (liftM)
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
import Model.Common (OpStatus(..))
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "service"

data Service = Service
             { _id         :: M.ObjectId
             , description :: Maybe String
             , enabled     :: Bool
             , name        :: Maybe String
             , type'       :: ServiceType
             , endpoints   :: [Endpoint]
             } deriving (Show, Read, Eq, Ord, Typeable)

endpointsF = T.pack $ nameBase 'endpoints
endpointsA = (M.String $ "$" +++ endpointsF) -- endpoints command for aggregation

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

$(deriveJSON (skipTickOptions <.> skipUnderscoreOptions) ''Service)

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

produceServiceJson :: Service -> String -> Value
produceServiceJson (s@Service{..}) baseUrl
      = Object
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/services/" ++ (show _id)) ])
        $ fromObject $ toJSON s

produceServiceReply :: Service -> String -> Value
produceServiceReply (service@Service{..}) baseUrl
      = object [ "service" .= produceServiceJson service baseUrl ]

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

produceServicesReply :: [Service] -> String -> Value
produceServicesReply services baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/services")
                                  ]
                          )
             , "services" .= servicesEntry
             ]
  where
    servicesEntry = Array $ fromList
                              $ map (\f -> f baseUrl)
                                  $ map produceServiceJson services

createService :: Service -> M.Action IO M.ObjectId
createService s = do
  M.ObjId oid <- M.insert collectionName $ toBson s
  return oid

listServices :: M.Action IO [Service]
listServices = do
  cur <- M.find $ M.select [] collectionName
  docs <- M.rest cur
  mapM fromBson docs

findServiceById :: ObjectId -> M.Action IO (Maybe Service)
findServiceById sid = runMaybeT $ do
  mService <- MaybeT $ M.findOne (M.select [idF =: sid] collectionName)
  fromBson mService

updateService :: M.ObjectId -> M.Document -> M.Action IO (Maybe Service)
updateService sid serviceUpdate = do
  M.modify (M.select [idF =: sid] collectionName) [ setC =: serviceUpdate ]
  -- If the service is deleted between these commands we assume it's never been updated
  -- TODO Remove this from here. It should be handled by a higher layer.
  findServiceById sid

deleteService :: ObjectId -> M.Action IO OpStatus
deleteService sid = do
  M.delete $ M.select [idF =: sid] collectionName
  ad <- affectedDocs
  if ad == 0
    then return NotFound
    else return Success


addEndpoint :: ObjectId -> Endpoint -> M.Action IO (Maybe M.ObjectId)
addEndpoint sid endpoint = do
  M.modify (M.select [idF =: sid] collectionName) [ pushC =: [endpointsF =: (toBson endpoint)] ]
  count <- affectedDocs
  return $
    if count == 1
      then Just $ eid endpoint
      else Nothing

listEndpoints :: M.Action IO [(M.ObjectId, Endpoint)]
listEndpoints = do
  docs <- M.aggregate collectionName [ [projectC =: [endpointsF =: (M.Int32 1)]]
                                     , [unwindC  =: endpointsA]
                                     ]
  let eDocs = map ((\(M.Doc d) -> d) . (M.valueAt endpointsF)) docs
  endpoints <- mapM fromBson eDocs
  let serviceIds = map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
  return $ zip serviceIds endpoints
