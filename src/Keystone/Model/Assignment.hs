{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Keystone.Model.Assignment
where

import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.Maybe (catMaybes)

import Language.Haskell.TH.Syntax (nameBase)

import Model.Common (listObjects)

import qualified Data.Text as T
import qualified Database.MongoDB as M
import qualified Database.MongoDB.Admin as MA

import qualified Keystone.Model.Project as MP
import qualified Keystone.Model.Role as MR
import qualified Keystone.Model.User as MU
import qualified Keystone.Model.Mongo.User as MMU

collectionName :: M.Collection
collectionName = "assignment"

data Assignment = Assignment
                { projectId :: MP.ProjectId
                , userId    :: MU.UserId
                , roleId    :: MR.RoleId
                } deriving (Show, Typeable, Eq)

$(deriveBson id ''Assignment)

listAssignments :: (Maybe MP.ProjectId)
                -> (Maybe MU.UserId)
                -> M.Action IO [Assignment]
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
  let roleIds    = map ((\(MR.RoleId    rid) -> rid) . roleId)    assignments
  let userIds    = map ((\(MU.UserId    uid) -> uid) . userId)    assignments
  let projectIds = map ((\(MP.ProjectId pid) -> pid) . projectId) assignments
  existingRoleIds    <- MR.listExistingRoleIds    roleIds
  existingUserIds    <- MMU.listExistingUserIds   userIds
  existingProjectIds <- MP.listExistingProjectIds projectIds
  return $ filter (\(Assignment (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid))
                      -> (   (rid `elem` existingRoleIds)
                          && (uid `elem` existingUserIds)
                          && (pid `elem` existingProjectIds)
                         )) assignments

listProjectsForUser :: MU.UserId -> M.Action IO [MP.Project]
listProjectsForUser uid = do
  assignments <- listAssignments Nothing (Just uid)
  let pids = map ((\(MP.ProjectId pid) -> pid) . projectId) assignments
  listObjects MP.collectionName pids

listUserRoles :: MP.ProjectId -> MU.UserId -> M.Action IO [MR.Role]
listUserRoles pid uid = do
  assignments <- listAssignments (Just pid) (Just uid)
  mRoles <- mapM MR.findRoleById
              $ map ((\(MR.RoleId rid) -> rid) . roleId) assignments
  return $ catMaybes mRoles

addAssignment :: Assignment -> M.Action IO M.ObjectId
addAssignment a = do
  M.ObjId aid <- M.insert collectionName $ toBson a
  return aid

verifyDatabase :: M.Action IO ()
verifyDatabase = do
  MA.ensureIndex $ (MA.index
                          collectionName
                          [ (T.pack $ nameBase 'projectId) =: (M.Int32 1)
                          , (T.pack $ nameBase 'userId)    =: (M.Int32 1)])
                      {MA.iUnique = True}
  MA.ensureIndex $ (MA.index
                          collectionName
                          [ (T.pack $ nameBase 'userId)    =: (M.Int32 1)
                          , (T.pack $ nameBase 'projectId) =: (M.Int32 1)])
                      {MA.iUnique = True}
  assignments <- listAssignments Nothing Nothing
  return ()
