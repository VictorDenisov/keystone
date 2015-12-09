{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
where

import Common (loggerName)
import Config (readConfig, KeystoneConfig(..), ServerType(..))
import Control.Applicative ((<$>))
import Control.Monad (when, MonadPlus(mzero))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Except (runExceptT, MonadError(throwError))
import Control.Monad.Trans.Resource (runResourceT, allocate, release)
import Data.Maybe (isNothing, fromJust)
import Data.Time.Clock (getCurrentTime)
import Model.Common (OpStatus(..))
import Model.IdentityApi
import Model.Mongo.IdentityApi
import Network.HTTP.Types.Method (StdMethod(HEAD))
import Network.HTTP.Types.Status ( status200, status201, status204, status401
                                 , status404, statusCode)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings)
import System.IO (stdout)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger ( debugM, errorM, setLevel, updateGlobalLogger
                         , noticeM, setHandlers, removeAllHandlers)
import System.Log.Formatter (simpleLogFormatter)

import Text.Read (readMaybe)

import Web.Version (versionHandlers)

import Web.Common ( ScottyM, ActionM, withHostUrl, getBaseUrl
                  , parseMaybeString, parseId, parseRequest)

import qualified Model.Mongo.Common as CD

import qualified Data.Text.Lazy as TL

import qualified Database.MongoDB as M
import qualified Error as E

import qualified Model.Assignment as MA
import qualified Model.Domain as MD
import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Model.Service as MS
import qualified Model.Token as MT
import qualified Model.User as MU
import qualified Model.Mongo.User as MMU

import qualified Web.Auth as A
import qualified Web.Auth.Types as AT
import qualified Web.Assignment as Assig
import qualified Web.Domain as D
import qualified Web.Project as P
import qualified Web.Role as R
import qualified Web.Service as Srv
import qualified Web.User as U
import qualified Web.Scotty.Trans as S

main = do
  config <- readConfig
  let logFormatter = simpleLogFormatter "$utcTime (pid $pid, $tid) $prio: $msg"
  stdoutHandler <- streamHandler stdout (logLevel config)
  fileHandler <- fileHandler "keystone.log" (logLevel config)
  removeAllHandlers
  updateGlobalLogger loggerName $ setLevel (logLevel config) . setHandlers
    [ setFormatter stdoutHandler logFormatter
    , setFormatter fileHandler   logFormatter
    ]

  !policy <- A.loadPolicy
  -- ^ bang pattern is because we want to know if the policy is correct now
  -- ^ we need the evaluation to happen immediatelly
  verifyDatabase config

  let runMongo = runMongoBackend $ database config
  app <- S.scottyAppT runMongo runMongo (application policy config)
  let settings = tlsSettings
                      (certificateFile config)
                      (keyFile config)
  let serverSettings = setPort (port config) defaultSettings
  noticeM loggerName "Starting web server"
  case serverType config of
    Tls   -> runTLS settings serverSettings app
    Plain -> runSettings serverSettings app

application :: ( MonadBase IO (b IO)
               , MonadBaseControl IO (b IO)
               , MonadThrow (b IO)
               , MonadIO (b IO)
               , IdentityApi (b IO))
            => AT.Policy
            -> KeystoneConfig
            -> ScottyM (b IO) ()
application policy config = do
  S.middleware logRequestResponse
  S.defaultHandler $ \e -> do
    S.status $ E.code e
    case statusCode $ E.code e of
      500 -> do
        time <- liftIO $ getCurrentTime
        liftIO $ errorM loggerName $ E.message e
        S.json $ e {E.message = "Internal error. Server time - " ++ (show time)}
      _ -> do
        S.json e
  -- Version API
  versionHandlers config
  -- Token API
  S.post "/v3/auth/tokens" $ do
    (au :: AT.AuthRequest) <- parseRequest
    baseUrl <- getBaseUrl config
    runResourceT $ do
      (releaseKey, pipe) <- allocate (CD.connect $ database config) M.close
      res <- lift $ lift $ mapM (A.authenticate pipe (AT.scope au)) (AT.methods au)
      release releaseKey
      case head res of
        Right (tokenId, t) -> lift $ do
          let resp = A.produceTokenResponse t baseUrl
          S.json resp
          S.addHeader "X-Subject-Token" (TL.pack tokenId)
          S.status status200
        Left errorMessage -> lift $ do
          S.json $ E.unauthorized errorMessage
          S.status status401
  S.get "/v3/auth/tokens" $ A.requireToken config $ \token -> do
    mSubjectToken <- S.header hXSubjectToken
    baseUrl <- getBaseUrl config
    res <- runExceptT $ do
      when (isNothing mSubjectToken) $ throwError "Could not find token, ."
      let mst = readMaybe $ TL.unpack $ fromJust mSubjectToken

      when (isNothing mst) $ throwError "Token is not an object id"
      let st = fromJust mst
      mToken <- liftIO $ CD.withDB (database config) $ MT.findTokenById st

      when (isNothing mToken) $ throwError $ "Could not find token, " ++ (show st) ++ "."
      let token = fromJust mToken
      currentTime <- liftIO getCurrentTime

      when (currentTime > (MT.expiresAt token)) $ throwError $ "Could not find token, " ++ (show st) ++ "."
      return token

    case res of
      Left errorMessage -> do
        S.status status404
        S.json $ E.notFound errorMessage
      Right tokenToVerify -> A.authorize policy AT.ValidateToken token (AT.Token tokenToVerify) $ do
        S.status status200
        S.json $ A.produceTokenResponse tokenToVerify baseUrl
  S.addroute HEAD "/v3/auth/tokens" $ A.requireToken config $ \token -> do
    mSubjectToken <- S.header hXSubjectToken
    res <- runMaybeT $ do
      subjectToken <- MaybeT $ return mSubjectToken
      st <- MaybeT $ return $ readMaybe $ TL.unpack subjectToken
      token <- MaybeT $ liftIO $ CD.withDB (database config) $ MT.findTokenById st
      currentTime <- liftIO getCurrentTime
      when (currentTime > (MT.expiresAt token)) mzero
      return token

    case res of
      Nothing -> do
        S.status status404
      Just tokenToVerify -> do
        A.authorize policy AT.CheckToken token (AT.Token tokenToVerify) $ S.status status204
  -- Service Catalog API
  Srv.serviceCatalogHandlers policy config
  -- Domain API
  S.get "/v3/domains" $ A.requireToken config $ \token -> do
    A.authorize policy AT.ListDomains token AT.EmptyResource $ do
      S.status status200
      withHostUrl config $ D.produceDomainsReply []
  S.get "/v3/domains/:did" $ A.requireToken config $ \token -> do
    (did :: M.ObjectId) <- parseId "did"
    A.authorize policy AT.ShowDomainDetails token AT.EmptyResource $ do
      S.status status200
      withHostUrl config $ D.produceDomainReply MD.Domain
  -- Project API
  S.post "/v3/projects" $ A.requireToken config $ \token -> do
    (pcr :: P.ProjectCreateRequest) <- parseRequest
    project <- liftIO $ P.newRequestToProject pcr
    A.authorize policy AT.AddProject token AT.EmptyResource $ do
      mPid <- liftIO $ CD.withDB (database config) $ MP.createProject project
      case mPid of
        Left err -> do
          S.json err
          S.status $ E.code err
        Right rid -> do
          S.status status201
          withHostUrl config $ P.produceProjectReply project
  S.get "/v3/projects" $ A.requireToken config $ \token -> do
    projectName <- parseMaybeString "name"
    A.authorize policy AT.ListProjects token AT.EmptyResource $ do
      projects <- liftIO $ CD.withDB (database config) $ MP.listProjects projectName
      S.status status200
      withHostUrl config $ P.produceProjectsReply projects
  S.get "/v3/projects/:pid" $ A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    A.authorize policy AT.ShowProjectDetails token AT.EmptyResource $ do
      mProject <- liftIO $ CD.withDB (database config) $ MP.findProjectById pid
      case mProject of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Project not found"
        Just project -> do
          S.status status200
          withHostUrl config $ P.produceProjectReply project
  S.delete "/v3/projects/:pid" $ A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    A.authorize policy AT.DeleteProject token AT.EmptyResource $ do
      n <- liftIO $ CD.withDB (database config) $ MP.deleteProject pid
      case n of
        Success -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find project, " ++ (show pid) ++ "."
          S.status status404
  S.get "/v3/projects/:pid/users/:uid/roles" $ A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.ListRolesForProjectUser token AT.EmptyResource $ do
      roles <- liftIO $ CD.withDB (database config) $ MA.listUserRoles (MP.ProjectId pid) (MU.UserId uid)
      S.status status200
      withHostUrl config $ R.produceRolesReply roles
  S.put "/v3/projects/:pid/users/:uid/roles/:rid" $ A.requireToken config $ \token -> do
    (pid :: M.ObjectId) <- parseId "pid"
    (uid :: M.ObjectId) <- parseId "uid"
    (rid :: M.ObjectId) <- parseId "rid"
    A.authorize policy AT.GrantRoleToProjectUser token AT.EmptyResource $ do
      res <- liftIO $ CD.withDB (database config) $ MA.addAssignment (MA.Assignment (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid))
      S.status status204
  -- User API
  S.post "/v3/users" $ A.requireToken config $ \token -> do
    (d :: U.UserCreateRequest) <- parseRequest
    user <- liftIO $ U.newRequestToUser d
    A.authorize policy AT.AddUser token AT.EmptyResource $ do
      mUid <- lift $ createUser user
      case mUid of
        Left err -> do
          S.json err
          S.status $ E.code err
        Right rid -> do
          S.status status201
          withHostUrl config $ U.produceUserReply user
  S.get "/v3/users" $ A.requireToken config $ \token -> do
    userName <- parseMaybeString "name"
    A.authorize policy AT.ListUsers token AT.EmptyResource $ do
      users <- lift $ listUsers userName
      S.status status200
      withHostUrl config $ U.produceUsersReply users
  S.get "/v3/users/:uid" $ A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.ShowUserDetails token AT.EmptyResource $ do
      mUser <- lift $ findUserById uid
      case mUser of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "User not found"
        Just user -> do
          S.status status200
          withHostUrl config $ U.produceUserReply user
  S.patch "/v3/users/:uid" $ A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    (uur :: U.UserUpdateRequest) <- parseRequest
    A.authorize policy AT.UpdateUser token AT.EmptyResource $ do
      updateDocument <- liftIO $ U.updateRequestToDocument uur
      mUser <- lift $ updateUser uid updateDocument
      case mUser of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "User not found"
        Just user -> do
          S.status status200
          withHostUrl config $ U.produceUserReply user
  S.delete "/v3/users/:uid" $ A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.DeleteUser token AT.EmptyResource $ do
      st <- lift $ deleteUser uid
      case st of
        Success  -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find user, " ++ (show uid) ++ "."
          S.status status404
  S.get "/v3/users/:uid/projects" $ A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.ListProjectsForUser token (AT.UserId uid) $ do
      projects <- liftIO $ CD.withDB (database config) $ MA.listProjectsForUser (MU.UserId uid)
      S.status status200
      withHostUrl config $ P.produceProjectsReply projects
  S.post "/v3/users/:uid/password" $ A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    (cpr :: U.ChangePasswordRequest) <- parseRequest
    A.authorize policy AT.ChangePassword token (AT.UserId uid) $ do
      res <- lift $ A.checkUserPassword (Just uid) Nothing (U.poriginalPassword cpr)
      case res of
        Left errorMessage -> do
          S.status status404
          S.json $ E.notFound "User not found"
        Right _ -> do
          updateDocument <- liftIO $ U.changePasswordRequestToDocument cpr
          mModifiedUser <- lift $ updateUser uid updateDocument
          case mModifiedUser of
            Nothing -> do
              S.status status404
              S.json $ E.notFound "User not found"
            Just modifiedUser -> do
              S.status status200
              withHostUrl config $ U.produceUserReply modifiedUser
  -- Role API
  S.post "/v3/roles" $ A.requireToken config $ \token -> do
    (rcr :: R.RoleCreateRequest) <- parseRequest
    role <- liftIO $ R.newRequestToRole rcr
    A.authorize policy AT.AddRole token AT.EmptyResource $ do
      mRid <- liftIO $ liftIO $ CD.withDB (database config) $ MR.createRole role
      case mRid of
        Left err -> do
          S.json err
          S.status $ E.code err
        Right rid -> do
          S.status status201
          withHostUrl config $ R.produceRoleReply role
  S.get "/v3/roles" $ A.requireToken config $ \token -> do
    roleName <- parseMaybeString "name"
    A.authorize policy AT.ListRoles token AT.EmptyResource $ do
      roles <- liftIO $ CD.withDB (database config) $ MR.listRoles roleName
      S.status status200
      withHostUrl config $ R.produceRolesReply roles
  S.get "/v3/roles/:rid" $ A.requireToken config $ \token -> do
    (rid :: M.ObjectId) <- parseId "rid"
    A.authorize policy AT.ShowRoleDetails token AT.EmptyResource $ do
      mRole <- liftIO $ CD.withDB (database config) $ MR.findRoleById rid
      case mRole of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Role not found"
        Just role -> do
          S.status status200
          withHostUrl config $ R.produceRoleReply role
  S.delete "/v3/roles/:rid" $ A.requireToken config $ \token -> do
    (rid :: M.ObjectId) <- parseId "rid"
    A.authorize policy AT.DeleteRole token AT.EmptyResource $ do
      st <- liftIO $ CD.withDB (database config) $ MR.deleteRole rid
      case st of
        Success  -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find role, " ++ (show rid) ++ "."
          S.status status404
  S.get "/v3/role_assignments" $ A.requireToken config $ \token -> do
    userId <- parseMaybeParam "user.id"
    projectId <- parseMaybeParam "scope.project.id"
    A.authorize policy AT.ListRoleAssignments token AT.EmptyResource $ do
      assignments <- liftIO $ CD.withDB (database config) $ MA.listAssignments (MP.ProjectId <$> projectId) (MU.UserId <$> userId)
      S.status status200
      withHostUrl config $ Assig.produceAssignmentsReply assignments

verifyDatabase :: KeystoneConfig -> IO ()
verifyDatabase KeystoneConfig{..} = liftIO $ CD.withDB database $ do
  liftIO $ noticeM loggerName "Verifying user collection"
  MMU.verifyDatabase
  liftIO $ noticeM loggerName "Verifying role collection"
  MR.verifyDatabase
  liftIO $ noticeM loggerName "Verifying project collection"
  MP.verifyDatabase
  liftIO $ noticeM loggerName "Verifying service collection"
  MS.verifyDatabase
  liftIO $ noticeM loggerName "Verifying assignment collection"
  MA.verifyDatabase
  when verifyTokenCollection $ do
    liftIO $ noticeM loggerName "Verifying token collection"
    MT.verifyDatabase

logRequestResponse :: Middleware
logRequestResponse app request responder = do
  debugM loggerName $ show request
  app request responder

parseMaybeParam :: (MonadIO m, Read a) => TL.Text -> ActionM m (Maybe a)
parseMaybeParam paramName =
  (flip S.rescue) (\msg -> return Nothing) $ do
    (value :: String) <- S.param paramName
    case readMaybe value of
      Nothing -> S.raise $ E.badRequest $ "Failed to parse value from " ++ (TL.unpack paramName)
      Just v  -> return $ Just v

hXSubjectToken :: TL.Text
hXSubjectToken = "X-Subject-Token"
