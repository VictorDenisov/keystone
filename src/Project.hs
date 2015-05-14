{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Project
( module Project
, module Project.Types
) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object, (.:))
import Data.Aeson.TH (mkParseJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (fromMaybe)
import Project.Types
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Model.Project as MP

newRequestToProject :: ProjectCreateRequest -> MP.Project
newRequestToProject ProjectCreateRequest{..} =
  MP.Project name description (fromMaybe True enabled)

instance FromJSON ProjectCreateRequest where
  parseJSON (Object v) = do
    project <- v .: "project"
    parsePcr project
  parseJSON v = typeMismatch (nameBase ''ProjectCreateRequest) v

parsePcr = $(mkParseJSON defaultOptions ''ProjectCreateRequest)

