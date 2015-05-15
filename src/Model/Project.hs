{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Project
where

import Common (capitalize, fromObject)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "project"

data Project = Project
             { name :: String
             , description :: Maybe String
             , enabled :: Bool
             } deriving (Show, Read, Eq, Ord, Typeable)

$(deriveBson id ''Project)

$(deriveJSON defaultOptions ''Project)

produceProjectJson :: Project -> M.ObjectId -> String -> Value
produceProjectJson (project@Project{..}) pid baseUrl
      = Object
        $ insert "id" (String $ T.pack $ show pid)
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/projects/" ++ (show pid)) ])
        $ fromObject $ toJSON project

produceProjectReply :: Project -> M.ObjectId -> String -> Value
produceProjectReply project pid baseUrl
      = object [ "project" .= produceProjectJson project pid baseUrl ]

produceProjectsReply :: [(M.ObjectId, Project)] -> String -> Value
produceProjectsReply projects baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/projects")
                                  ]
                          )
             , "projects" .= projectsEntry
             ]
  where
    projectsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\(i, s) -> produceProjectJson s i) projects

createProject :: MonadIO m => Project -> M.Action m M.ObjectId
createProject p = do
  M.ObjId pid <- M.insert collectionName $ toBson p
  return pid

listProjects :: (MonadIO m, MonadBaseControl IO m)
             => M.Action m [(M.ObjectId, Project)]
listProjects = do
  cur <- M.find $ M.select [] collectionName
  docs <- M.rest cur
  projects <- mapM fromBson docs
  let ids = map ((\(M.ObjId i) -> i) . (M.valueAt "_id")) docs
  return $ zip ids projects

findProjectById :: (MonadIO m) => M.ObjectId -> M.Action m (Maybe Project)
findProjectById pid = runMaybeT $ do
  mProject <- MaybeT $ M.findOne (M.select ["_id" =: pid] collectionName)
  fromBson mProject
