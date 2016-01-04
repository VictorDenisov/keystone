{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Keystone.Model.Service
( module Keystone.Model.Service
, module Model.Service.Types
) where

import Common ( capitalize, dropOptions, skipTickOptions
              , skipUnderscoreOptions, (<.>))
import Model.Mongo.Common ( affectedDocs, pullC, pushC, setC, projectC, unwindC
                          , idF, (+.+))
import Control.Applicative ((<$>))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Bson (Val(..), (=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Language.Haskell.TH.Syntax (nameBase)
import Model.Common (OpStatus(Success, NotFound))

import Model.Service.Types

import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Database.MongoDB.Admin as MA
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "service"

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

$(deriveBson endpointFieldMod ''Endpoint)

$(deriveJSON (dropOptions 1) ''Endpoint)

createService :: Service -> M.Action IO M.ObjectId
createService s = do
  M.ObjId oid <- M.insert collectionName $ toBson s
  return oid

listServices :: (Maybe String) -> M.Action IO [Service]
listServices mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cur <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cur
  mapM fromBson docs

findServiceById :: M.ObjectId -> M.Action IO (Maybe Service)
findServiceById sid = runMaybeT $ do
  mService <- MaybeT $ M.findOne (M.select [idF =: sid] collectionName)
  fromBson mService

updateService :: M.ObjectId -> M.Document -> M.Action IO (Maybe Service)
updateService sid serviceUpdate = do
  res <- M.findAndModify (M.select [idF =: sid] collectionName) [ setC =: serviceUpdate ]
  case res of
    Left _  -> return Nothing
    Right v -> Just <$> fromBson v

deleteService :: M.ObjectId -> M.Action IO OpStatus
deleteService sid = do
  M.delete $ M.select [idF =: sid] collectionName
  ad <- affectedDocs
  if ad == 0
    then return NotFound
    else return Success

addEndpoint :: M.ObjectId -> Endpoint -> M.Action IO (Maybe M.ObjectId)
addEndpoint sid endpoint = do
  M.modify
    (M.select [idF =: sid] collectionName)
    [ pushC =: [endpointsF =: (toBson endpoint)] ]
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

findEndpointById :: M.ObjectId -> M.Action IO (Maybe (M.ObjectId, Endpoint))
findEndpointById eidToFind = runMaybeT $ do
  serviceDoc <- MaybeT $ M.findOne
                          ((M.select
                              [(endpointsF +.+ "id") =: eidToFind]
                              collectionName
                              )
                            {M.project = [(endpointsF +.+ "$") =: (M.Int32 1)]})
  endpointDoc <- MaybeT $ return
                            $ listToMaybe
                              $ (\(M.Array a) -> a)
                                $ M.valueAt endpointsF serviceDoc
  endpoint <- fromBson $ (\(M.Doc d) -> d) endpointDoc
  let serviceId = (\(M.ObjId i) -> i) $  (M.valueAt idF) $ serviceDoc
  return (serviceId, endpoint)

deleteEndpoint :: M.ObjectId -> M.Action IO OpStatus
deleteEndpoint eid = do
  M.modify
    (M.select [(endpointsF +.+ eidF) =: eid] collectionName)
    [pullC =: [endpointsF =: [eidF =: eid]]]
  ad <- affectedDocs
  if ad == 0
    then return NotFound
    else return Success

verifyDatabase :: M.Action IO ()
verifyDatabase = do
  MA.ensureIndex $ (MA.index
                          collectionName
                          [(T.pack $ nameBase 'name) =: (M.Int32 1)])
  services <- listServices Nothing
  return ()
