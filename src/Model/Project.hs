{-# Language DeriveDataTypeable #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Project
where

import Common (fromObject, skipUnderscoreOptions, UrlBasedValue, UrlInfo(..))
import Common.Database (idF)

import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Aeson (ToJSON(..), Value(..))
import Data.Aeson.Types (object, (.=))
import Data.Aeson.TH (deriveJSON)
import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)
import Data.HashMap.Strict (insert)
import Data.Vector (fromList)

import Language.Haskell.TH.Syntax (nameBase)

import Model.Common (listExistingIds)

import qualified Error as E

import qualified Database.MongoDB as M
import qualified Database.MongoDB.Admin as MA
import qualified Data.Text as T

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

produceProjectReply :: Project -> UrlBasedValue
produceProjectReply project (UrlInfo {baseUrl})
      = object [ "project" .= produceProjectJson project baseUrl ]

produceProjectsReply :: [Project] -> String -> String -> UrlBasedValue
produceProjectsReply projects pathString queryString (UrlInfo {baseUrl})
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ pathString ++ queryString)
                                  ]
                          )
             , "projects" .= projectsEntry
             ]
  where
    projectsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceProjectJson projects

createProject :: Project -> M.Action IO (Either E.Error M.ObjectId)
createProject p = (do
    M.ObjId pid <- M.insert collectionName $ toBson p
    return $ Right pid
  ) `catch` (\f -> do
    case f of
      M.WriteFailure duplicateE message ->
          return $ Left $ E.conflict $ "Insert of project with the duplicate " ++ (nameBase 'name) ++ " is not allowed."
      _ -> throwM f
  )

listProjects :: (Maybe String) -> M.Action IO [Project]
listProjects mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(T.pack $ nameBase 'name) =: (M.String $ T.pack nm)]
  cur <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cur
  mapM fromBson docs

findProjectById :: M.ObjectId -> M.Action IO (Maybe Project)
findProjectById pid = runMaybeT $ do
  mProject <- MaybeT $ M.findOne (M.select [idF =: pid] collectionName)
  fromBson mProject

-- | Returns subset of the provided list.
-- | The subset contains only existing project ids.
listExistingProjectIds ::[M.ObjectId] -> M.Action IO [M.ObjectId]
listExistingProjectIds = listExistingIds collectionName


verifyDatabase :: M.Action IO ()
verifyDatabase = do
  MA.ensureIndex $ (MA.index
                          collectionName
                          [(T.pack $ nameBase 'name) =: (M.Int32 1)])
                      {MA.iUnique = True} -- TODO this should be modified as soon as domains are implemented
                      -- different domains can have projects with the same name
  projects <- listProjects Nothing
  return ()
