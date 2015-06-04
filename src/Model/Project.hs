{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Project
where

import Common (capitalize, fromObject, loggerName, skipUnderscoreOptions)
import Common.Database ( affectedDocs, currentDateC, idF, inC, matchC, neC, pullC
                       , pushC, setC, unwindC, (+.+), (+++))

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Maybe (catMaybes)
import Data.Time.Clock (getCurrentTime)
import Data.Vector (fromList)
import Language.Haskell.TH.Syntax (nameBase)
import Model.Common (TransactionId(..), CaptureStatus(..))
import System.Log.Logger (debugM)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

import qualified Model.Role as MR
import qualified Model.User as MU

collectionName :: M.Collection
collectionName = "project"

data Project = Project
             { _id  :: M.ObjectId
             , name :: String
             , description :: Maybe String
             , enabled :: Bool
             } deriving (Show, Read, Eq, Ord, Typeable)

-- aux fields
userRoleAssignments :: T.Text
userRoleAssignments = "userRoleAssignments"
pendingTransactions :: T.Text
pendingTransactions = "pendingTransactions"

userIdF :: T.Text
userIdF = "userId"
roleIdF :: T.Text
roleIdF = "roleId"

newtype ProjectId = ProjectId M.ObjectId
                    deriving (Show, Typeable, Eq)

instance M.Val ProjectId where
  val (ProjectId pid) = M.ObjId pid
  cast' (M.ObjId pid) = return $ ProjectId pid
  cast' _ = Nothing

$(deriveBson id ''Project)

$(deriveJSON skipUnderscoreOptions ''Project)

produceProjectJson :: Project -> String -> Value
produceProjectJson (project@Project{..}) baseUrl
      = Object
        $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/projects/" ++ (show _id)) ])
        $ fromObject $ toJSON project

produceProjectReply :: Project -> String -> Value
produceProjectReply project baseUrl
      = object [ "project" .= produceProjectJson project baseUrl ]

produceProjectsReply :: [Project] -> String -> Value
produceProjectsReply projects baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/projects")
                                  ]
                          )
             , "projects" .= projectsEntry
             ]
  where
    projectsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceProjectJson projects

createProject ::  MonadIO m => Project -> M.Action m M.ObjectId
createProject p = do
  M.ObjId pid <- M.insert collectionName $ toBson p
  return pid

listProjects :: (MonadIO m, MonadBaseControl IO m)
             => (Maybe String) -> M.Action m [Project]
listProjects mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cur <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cur
  mapM fromBson docs

findProjectById :: (MonadIO m) => M.ObjectId -> M.Action m (Maybe Project)
findProjectById pid = runMaybeT $ do
  mProject <- MaybeT $ M.findOne (M.select [idF =: pid] collectionName)
  fromBson mProject

listExistingProjectIds :: (MonadIO m, MonadBaseControl IO m)
                       => [M.ObjectId] -> M.Action m [M.ObjectId]
listExistingProjectIds projectIds = do
  cur <- M.find (M.select [ idF =: [inC =: (M.Array $ map M.ObjId projectIds)] ] collectionName)
  docs <- M.rest cur
  return $ map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
