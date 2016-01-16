{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Keystone.Web.Assignment
where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value(..))
import Data.Aeson.Types (object, (.=))
import Data.Vector (fromList)
import Keystone.Config (KeystoneConfig(..))
import Keystone.Model.IdentityApi (IdentityApi)
import Network.HTTP.Types.Status (status200, status204)
import Text.Read (readMaybe)

import Web.Common (UrlBasedValue, UrlInfo(..), parseId, withHostUrl, ActionM)

import qualified Data.Text.Lazy as TL
import qualified Database.MongoDB as M

import qualified Error as E

import qualified Keystone.Model.Assignment as MA
import qualified Model.Mongo.Common as CD
import qualified Keystone.Model.Project as MP
import qualified Keystone.Model.Role as MR
import qualified Keystone.Model.User as MU

import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Keystone.Web.Project as P
import qualified Keystone.Web.Role as R
import qualified Web.Scotty.Trans as S

listProjectUserRolesH :: (Functor m, MonadIO m, IdentityApi m)
                      => AT.Policy -> KeystoneConfig -> ActionM m ()
listProjectUserRolesH policy config = A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.ListRolesForProjectUser token AT.EmptyResource $ do
      roles <- liftIO $ CD.withDB (database config) $ MA.listUserRoles (MP.ProjectId pid) (MU.UserId uid)
      S.status status200
      withHostUrl config $ R.produceRolesReply roles

createAssignmentH :: (Functor m, MonadIO m, IdentityApi m)
                  => AT.Policy -> KeystoneConfig -> ActionM m ()
createAssignmentH policy config = A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    (uid :: M.ObjectId) <- parseId "uid"
    (rid :: M.ObjectId) <- parseId "rid"
    A.authorize policy AT.GrantRoleToProjectUser token AT.EmptyResource $ do
      res <- liftIO $ CD.withDB (database config) $ MA.addAssignment (MA.Assignment (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid))
      S.status status204
  
listUserProjects :: (Functor m, MonadIO m, IdentityApi m)
                 => AT.Policy -> KeystoneConfig -> ActionM m ()
listUserProjects policy config = A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.ListProjectsForUser token (AT.UserId uid) $ do
      projects <- liftIO $ CD.withDB (database config) $ MA.listProjectsForUser (MU.UserId uid)
      S.status status200
      withHostUrl config $ P.produceProjectsReply projects

listAssignmentsH :: (Functor m, MonadIO m, IdentityApi m)
                 => AT.Policy -> KeystoneConfig -> ActionM m ()
listAssignmentsH policy config = A.requireToken config $ \token -> do
    userId <- parseMaybeParam "user.id"
    projectId <- parseMaybeParam "scope.project.id"
    A.authorize policy AT.ListRoleAssignments token AT.EmptyResource $ do
      assignments <- liftIO $ CD.withDB (database config) $ MA.listAssignments (MP.ProjectId <$> projectId) (MU.UserId <$> userId)
      S.status status200
      withHostUrl config $ produceAssignmentsReply assignments

produceAssignmentJson :: MA.Assignment -> String -> Value
produceAssignmentJson
          (MA.Assignment (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid))
          baseUrl
      = object [ "role" .= (object ["id" .= rid])
               , "user" .= (object ["id" .= uid])
               , "scope" .= (object ["project" .= (object ["id" .= pid] )])
               , "links" .= (object ["assignment" .= (baseUrl ++ "/v3/projects/" ++ (show pid) ++ "/users/" ++ (show uid) ++ "/roles/" ++ (show rid))])
               ]

produceAssignmentsReply :: [MA.Assignment] -> UrlBasedValue
produceAssignmentsReply assignments (UrlInfo {baseUrl, path, query})
    = object [ "links" .= (object [ "next"     .= Null
                                  , "previous" .= Null
                                  , "self"     .= (baseUrl ++ path ++ query)
                                  ]
                          )
             , "role_assignments" .= assignmentsEntry
             ]
  where
    assignmentsEntry = Array $ fromList $ map (\f -> f baseUrl) $ map (\a -> produceAssignmentJson a) assignments

parseMaybeParam :: (MonadIO m, Read a) => TL.Text -> ActionM m (Maybe a)
parseMaybeParam paramName =
  (flip S.rescue) (\msg -> return Nothing) $ do
    (value :: String) <- S.param paramName
    case readMaybe value of
      Nothing -> S.raise $ E.badRequest $ "Failed to parse value from " ++ (TL.unpack paramName)
      Just v  -> return $ Just v
