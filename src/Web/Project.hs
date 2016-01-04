{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Project
( module Web.Project
, module Web.Project.Types
) where

import Common (fromObject, underscoreOptions)
import Config (KeystoneConfig(database))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (object, typeMismatch, (.=))
import Data.HashMap.Strict (insert)
import Data.Maybe (fromMaybe)
import Data.Vector (fromList)
import Model.Common (OpStatus(Success, NotFound))
import Model.IdentityApi (IdentityApi)
import Network.HTTP.Types.Status (status200, status201, status204, status404)
import Web.Common ( UrlBasedValue, UrlInfo(..), withHostUrl, parseRequest
                  , ActionM, parseMaybeString, parseId)
import Web.Project.Types
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Error as E
import qualified Model.Mongo.Common as CD
import qualified Keystone.Model.Project as MP
import qualified Web.Auth as A
import qualified Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S

createProjectH :: (Functor m, MonadIO m, IdentityApi m)
               => AT.Policy -> KeystoneConfig -> ActionM m ()
createProjectH policy config = A.requireToken config $ \token -> do
    (pcr :: ProjectCreateRequest) <- parseRequest
    project <- liftIO $ newRequestToProject pcr
    A.authorize policy AT.AddProject token AT.EmptyResource $ do
      mPid <- liftIO $ CD.withDB (database config) $ MP.createProject project
      case mPid of
        Left err -> do
          S.json err
          S.status $ E.code err
        Right rid -> do
          S.status status201
          withHostUrl config $ produceProjectReply project

listProjectsH :: (Functor m, MonadIO m, IdentityApi m)
              => AT.Policy -> KeystoneConfig -> ActionM m ()
listProjectsH policy config = A.requireToken config $ \token -> do
    projectName <- parseMaybeString "name"
    A.authorize policy AT.ListProjects token AT.EmptyResource $ do
      projects <- liftIO $ CD.withDB (database config) $ MP.listProjects projectName
      S.status status200
      withHostUrl config $ produceProjectsReply projects

projectDetailsH :: (Functor m, MonadIO m, IdentityApi m)
                => AT.Policy -> KeystoneConfig -> ActionM m ()
projectDetailsH policy config = A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    A.authorize policy AT.ShowProjectDetails token AT.EmptyResource $ do
      mProject <- liftIO $ CD.withDB (database config) $ MP.findProjectById pid
      case mProject of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Project not found"
        Just project -> do
          S.status status200
          withHostUrl config $ produceProjectReply project

deleteProjectH :: (Functor m, MonadIO m, IdentityApi m)
               => AT.Policy -> KeystoneConfig -> ActionM m ()
deleteProjectH policy config = A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    A.authorize policy AT.DeleteProject token AT.EmptyResource $ do
      n <- liftIO $ CD.withDB (database config) $ MP.deleteProject pid
      case n of
        Success -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find project, " ++ (show pid) ++ "."
          S.status status404

newRequestToProject :: ProjectCreateRequest -> IO MP.Project
newRequestToProject ProjectCreateRequest{..} = do
  projectId <- M.genObjectId
  return $ MP.Project projectId name description (fromMaybe True enabled)

instance FromJSON ProjectCreateRequest where
  parseJSON (Object v) = do
    project <- v .: "project"
    parsePcr project
  parseJSON v = typeMismatch (nameBase ''ProjectCreateRequest) v

parsePcr = $(mkParseJSON underscoreOptions ''ProjectCreateRequest)

produceProjectJson :: MP.Project -> String -> Value
produceProjectJson (project@MP.Project{..}) baseUrl
      = Object
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/projects/" ++ (show _id)) ])
        $ fromObject $ toJSON project

produceProjectReply :: MP.Project -> UrlBasedValue
produceProjectReply project (UrlInfo {baseUrl})
      = object [ "project" .= produceProjectJson project baseUrl ]

produceProjectsReply :: [MP.Project] -> UrlBasedValue
produceProjectsReply projects (UrlInfo {baseUrl, path, query})
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ path ++ query)
                                  ]
                          )
             , "projects" .= projectsEntry
             ]
  where
    projectsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceProjectJson projects

