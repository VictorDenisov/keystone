{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Project
where

import Common (capitalize, fromObject)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
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

createProject :: MonadIO m => Project -> M.Action m M.ObjectId
createProject p = do
  M.ObjId pid <- M.insert collectionName $ toBson p
  return pid
