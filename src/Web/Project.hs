{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Project
( module Web.Project
, module Web.Project.Types
) where

import Common (fromObject, underscoreOptions, UrlBasedValue, UrlInfo(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (object, typeMismatch, (.=))
import Data.HashMap.Strict (insert)
import Data.Maybe (fromMaybe)
import Data.Vector (fromList)
import Web.Project.Types
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Model.Project as MP

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

