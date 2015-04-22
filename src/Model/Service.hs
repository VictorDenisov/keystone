{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell #-}
module Model.Service
where

import Common (aesonOptions, capitalize)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Bson (Val(..))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Char (toLower)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)
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
             } deriving (Show, Read, Eq, Ord, Typeable)

data ServiceType = Identity
                 | Compute
                 | Image
                 | Volume
                 | Network
                   deriving (Show, Read, Eq, Ord, Typeable)

instance FromJSON ServiceType where
  parseJSON (String s) = case readMaybe $ capitalize $ T.unpack s of
                          Just v -> return v
                          Nothing -> fail $ "Unknown ServiceType " ++ (T.unpack s)
  parseJSON v = typeMismatch "ServiceType" v

instance ToJSON ServiceType where
  toJSON v = String $ T.pack $ map toLower $ show v

instance Val ServiceType where
  val st = M.String $ T.pack $ map toLower $ show st
  cast' (M.String s) = readMaybe $ capitalize $ T.unpack s
  cast' _ = Nothing

$(deriveBson ''Service)

$(deriveJSON aesonOptions ''Service)

produceServiceJson :: Service -> M.ObjectId -> String -> Value
produceServiceJson (s@Service{..}) oid baseUrl
      = Object
        $ insert "id" (String $ T.pack $ show oid)
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/services/" ++ (show oid)) ])
        $ fromObject $ toJSON s

fromObject :: Value -> Object
fromObject (Object o) = o

produceServiceReply :: Service -> M.ObjectId -> String -> Value
produceServiceReply (service@Service{..}) oid baseUrl
      = object [ "service" .= produceServiceJson service oid baseUrl ]

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
  let ids = map ((\(M.ObjId i) -> i) . (M.valueAt "_id")) docs
  return $ zip ids services
