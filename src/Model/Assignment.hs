{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Assignment
where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.Maybe (catMaybes)
import Data.Vector (fromList)

import Language.Haskell.TH.Syntax (nameBase)

import qualified Data.Text as T
import qualified Database.MongoDB as M

import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Model.User as MU


collectionName :: M.Collection
collectionName = "assignment"

data Assignment = Assignment
                { projectId :: MP.ProjectId
                , userId    :: MU.UserId
                , roleId    :: MR.RoleId
                } deriving (Show, Typeable, Eq)

$(deriveBson id ''Assignment)

produceAssignmentJson :: Assignment -> String -> Value
produceAssignmentJson (Assignment (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid)) baseUrl
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

listAssignments :: (MonadIO m, MonadBaseControl IO m)
                => (Maybe MP.ProjectId)
                -> (Maybe MU.UserId)
                -> M.Action m [Assignment]
listAssignments mPid mUid = do
  let projectFilter = case mPid of
                        Just (MP.ProjectId pid) -> [(T.pack $ nameBase 'projectId) =: pid]
                        Nothing -> []

  let userFilter = case mUid of
                        Just (MU.UserId uid) -> [(T.pack $ nameBase 'userId) =: uid]
                        Nothing -> []

  cur <- M.find $ M.select (projectFilter ++ userFilter) collectionName
  docs <- M.rest cur
  assignments <- mapM fromBson docs
  -- TODO optimize it for the case when project or user are not null
  let roleIds    = map ((\(MR.RoleId    rid) -> rid) . roleId)    assignments
  let userIds    = map ((\(MU.UserId    uid) -> uid) . userId)    assignments
  let projectIds = map ((\(MP.ProjectId pid) -> pid) . projectId) assignments
  existingRoleIds    <- MR.listExistingRoleIds    roleIds
  existingUserIds    <- MU.listExistingUserIds    userIds
  existingProjectIds <- MP.listExistingProjectIds projectIds
  return $ filter (\(Assignment (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid))
                      -> (   (rid `elem` existingRoleIds)
                          && (uid `elem` existingUserIds)
                          && (pid `elem` existingProjectIds)
                         )) assignments

listUserRoles :: (MonadIO m, MonadBaseControl IO m)
              => MP.ProjectId -> MU.UserId -> M.Action m [MR.Role]
listUserRoles pid uid = do
  assignments <- listAssignments (Just pid) (Just uid)
  mRoles <- mapM MR.findRoleById
              $ map ((\(MR.RoleId rid) -> rid) . roleId) assignments
  return $ catMaybes mRoles

addAssignment :: (MonadIO m)
              => Assignment -> M.Action m M.ObjectId
addAssignment a = do
  M.ObjId aid <- M.insert collectionName $ toBson a
  return aid
