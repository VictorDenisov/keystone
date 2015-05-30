{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Project
where

import Common (capitalize, fromObject)
import Common.Database ( affectedDocs, currentDateC, idF, matchC, neC, pullC
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
import Model.Transaction (Transaction(..))
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

import qualified Model.Role as MR
import qualified Model.Transaction as Tr
import qualified Model.User as MU

collectionName :: M.Collection
collectionName = "project"

data Project = Project
             { name :: String
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


$(deriveBson id ''Project)

$(deriveJSON defaultOptions ''Project)

data Assignment = Assignment
                { userId :: MU.UserId
                , roleId :: MR.RoleId
                , projectId :: ProjectId
                }

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
             => (Maybe String) -> M.Action m [(M.ObjectId, Project)]
listProjects mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cur <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cur
  projects <- mapM fromBson docs
  let ids = map ((\(M.ObjId i) -> i) . (M.valueAt idF)) docs
  return $ zip ids projects

findProjectById :: (MonadIO m) => M.ObjectId -> M.Action m (Maybe Project)
findProjectById pid = runMaybeT $ do
  mProject <- MaybeT $ M.findOne (M.select [idF =: pid] collectionName)
  fromBson mProject

addUserWithRole :: (MonadIO m)
                => ProjectId -> MU.UserId -> MR.RoleId -> M.Action m (Either String Int)
addUserWithRole (ProjectId pid) (MU.UserId uid) (MR.RoleId rid) = runExceptT $ do
  let t = (AddRole uid rid pid)
  curTime <- liftIO getCurrentTime
  (M.ObjId tid) <- lift $ M.insert Tr.collectionName $ [Tr.state =: Tr.Initial, Tr.lastModified =: curTime] ++ (toBson t)
  lift $ M.modify (M.select [idF =: tid, Tr.state =: Tr.Initial] Tr.collectionName)
                                              [ setC         =: [Tr.state =: Tr.Pending]
                                              , currentDateC =: [Tr.lastModified =: True]]
  pendingCount <- lift affectedDocs
  when (pendingCount /= 1) $ fail "Failed to move to pending state. Couldn't find created transaction"
  rcs <- lift $ MR.captureRole rid (TransactionId tid)
  when (rcs == CaptureFailed) $ do
    lift $ M.delete (M.select [idF =: tid] collectionName)
    fail $ "Failed to capture role with id " ++ (show rid)
  ucs <- lift $ MU.captureUser uid (TransactionId tid)
  when (ucs == CaptureFailed) $ do
    lift $ MR.rollbackCaptureRole rid (TransactionId tid)
    lift $ M.delete (M.select [idF =: tid] collectionName)
    fail $ "Failed to capture user with id " ++ (show uid)
  urcs <- lift $ addUserRolePair (ProjectId pid) (MU.UserId uid) (MR.RoleId rid) (TransactionId tid)
  when (urcs == CaptureFailed) $ do
    lift $ MU.rollbackCaptureUser uid (TransactionId tid)
    lift $ MR.rollbackCaptureRole rid (TransactionId tid)
    lift $ M.delete (M.select [idF =: tid] collectionName)
    fail "Failed to update project with user and role"
  lift $ M.modify (M.select [idF =: tid, Tr.state =: Tr.Pending] Tr.collectionName)
                                              [ setC         =: [Tr.state =: Tr.Applied]
                                              , currentDateC =: [Tr.lastModified =: True]]
  appliedCount <- lift affectedDocs
  when (appliedCount /= 1) $ do
    lift $ MU.rollbackCaptureUser uid (TransactionId tid)
    lift $ MR.rollbackCaptureRole rid (TransactionId tid)
    lift $ M.delete (M.select [idF =: tid] collectionName)
    fail "Failed to move transaction to applied state"
  lift $ MR.pullTransaction rid (TransactionId tid)
  lift $ MU.pullTransaction uid (TransactionId tid)
  lift $ pullTransaction pid (TransactionId tid)
  lift $ M.modify (M.select [idF =: tid, Tr.state =: Tr.Applied] Tr.collectionName)
                                              [ setC         =: [Tr.state =: Tr.Done]
                                              , currentDateC =: [Tr.lastModified =: True]]
  return 1

addUserRolePair :: (MonadIO m)
                => ProjectId
                -> MU.UserId
                -> MR.RoleId
                -> TransactionId
                -> M.Action m CaptureStatus
addUserRolePair (ProjectId pid)
                (MU.UserId uid)
                (MR.RoleId rid)
                (TransactionId tid) = do
  M.modify (M.select [idF =: pid, pendingTransactions =: [neC =: tid] ] collectionName)
                                      [ pushC =: [userRoleAssignments =: [ userIdF =: uid
                                                                         , roleIdF =: rid
                                                                         ]]
                                      , pushC =: [pendingTransactions =: tid]
                                      ]
  count <- affectedDocs
  if count == 1
    then return Captured
    else return CaptureFailed

pullTransaction :: (MonadIO m) => M.ObjectId -> TransactionId -> M.Action m ()
pullTransaction pid (TransactionId tid) = do
  M.modify (M.select [idF =: pid, pendingTransactions =: tid] collectionName)
                                      [pullC =: [pendingTransactions =: tid]]

listUserRoles :: (MonadIO m) => ProjectId -> MU.UserId -> M.Action m [(M.ObjectId, MR.Role)]
listUserRoles (ProjectId pid) (MU.UserId uid) = do
  docs <- M.aggregate collectionName [ [matchC  =: [idF =: pid]]
                                     , [unwindC =: (M.String $ "$" +++ userRoleAssignments)]
                                     , [matchC  =: [(userRoleAssignments +.+ userIdF) =: uid]]
                                     ]
  let roleIds = map ((\(M.ObjId o) -> o) . (M.valueAt roleIdF) . (\(M.Doc d) -> d) . (M.valueAt userRoleAssignments)) docs

  roles <- mapM MR.findRoleById roleIds
  let res = catMaybes ((flip map) (zip roleIds roles)
                                            (\(a, b') -> case b' of
                                                  Just b -> Just (a, b)
                                                  Nothing -> Nothing))
  return res

produceAssignmentJson :: Assignment -> String -> Value
produceAssignmentJson (Assignment (MU.UserId uid) (MR.RoleId rid) (ProjectId pid)) baseUrl
      = object [ "role" .= (object ["id" .= rid])
               , "user" .= (object ["id" .= uid])
               , "scope" .= (object ["project" .= (object ["id" .= pid] )])
               , "links" .= (object ["assignment" .= (baseUrl ++ "/v3/projects/" ++ (show pid) ++ "/users/" ++ (show uid) ++ "/roles/" ++ (show rid))])
               ]

produceAssignmentsReply :: [Assignment] -> String -> Value
produceAssignmentsReply assignments baseUrl
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ "/v3/role_assignments")
                                  ]
                          )
             , "role_assignments" .= assignmentsEntry
             ]
  where
    assignmentsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\a -> produceAssignmentJson a) assignments

listAssignments :: (MonadIO m) => (Maybe ProjectId) -> (Maybe MU.UserId) -> M.Action m [Assignment]
listAssignments pid' uid' = do
  let projectFilter = case pid' of
                        Just (ProjectId v) -> [[matchC =: [idF =: v]]]
                        Nothing -> []
  let userFilter = case uid' of
                        Just (MU.UserId v) -> [ [matchC =: [(userRoleAssignments +.+ userIdF) =: v]]]
                        Nothing -> []
  docs <- M.aggregate collectionName $ projectFilter ++ [[unwindC =: (M.String $ "$" +++ userRoleAssignments)] ] ++ userFilter
  let assignments = map (\x -> Assignment (MU.UserId $ getUserId x) (MR.RoleId $ getRoleId x) (ProjectId $ getProjectId x)) docs
  return assignments
  where
    getUserId = (\(M.ObjId u) -> u) . (M.valueAt userIdF) . (\(M.Doc d) -> d) . (M.valueAt userRoleAssignments)
    getRoleId = (\(M.ObjId r) -> r) . (M.valueAt roleIdF) . (\(M.Doc d) -> d) . (M.valueAt userRoleAssignments)
    getProjectId = (\(M.ObjId d) -> d) . (M.valueAt idF)
