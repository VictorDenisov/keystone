{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Project
( module Project
, module Project.Types
) where

import Common (underscoreOptions)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (fromMaybe)
import Project.Types
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

