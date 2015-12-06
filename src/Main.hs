{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
where

import Backend
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
import Data.Aeson.Types (FromJSON(..))
import Data.Maybe (isNothing, fromJust)
import Data.Time.Clock (getCurrentTime)
import Model.Common (OpStatus(..))
import MongoBackend
import Network.HTTP.Types.Method (StdMethod(HEAD))
import Network.HTTP.Types.Status ( status200, status201, status204, status401
                                 , status404, statusCode)
import Network.Wai (Middleware, rawPathInfo, rawQueryString)
import Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings)
import System.IO (stdout)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger ( debugM, errorM, setLevel, updateGlobalLogger
                         , noticeM, setHandlers, removeAllHandlers)
import System.Log.Formatter (simpleLogFormatter)

import Text.Read (readMaybe)

import Version (apiV3Reply, apiVersions)

import Web.Common (ScottyM, ActionM, UrlBasedValue, UrlInfo(..))

import qualified Common.Database as CD

import qualified Data.ByteString.Char8 as BS
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

import qualified Web.Auth as A
import qualified Web.Auth.Types as AT
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
               , BackendApi (b IO))
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
  S.get "/" $ do
    with_host_url config apiVersions
  S.get "/v3" $ do
    with_host_url config apiV3Reply
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
  -- Service API
  S.post "/v3/services" $ A.requireToken config $ \token ->
    A.authorize policy AT.AddService token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      (scr :: Srv.ServiceCreateRequest) <- parseRequest
      service <- liftIO $ Srv.newRequestToService scr
      sid <- liftIO $ CD.withDB (database config) $ MS.createService service
      S.status status201
      with_host_url config $ Srv.produceServiceReply service
  S.get "/v3/services" $ A.requireToken config $ \token -> do
    serviceName <- parseMaybeString "name"
    A.authorize policy AT.ListServices token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      services <- liftIO $ CD.withDB (database config) $ MS.listServices serviceName
      S.status status200
      with_host_url config $ Srv.produceServicesReply services
  S.get "/v3/services/:sid" $ A.requireToken config $ \token -> do
    (sid :: M.ObjectId) <- parseId "sid"
    A.authorize policy AT.ShowServiceDetails token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      mService <- liftIO $ CD.withDB (database config) $ MS.findServiceById sid
      case mService of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Service not found"
        Just service -> do
            S.status status200
            with_host_url config $ Srv.produceServiceReply service
  S.patch "/v3/services/:sid" $ A.requireToken config $ \token -> do
    (sid :: M.ObjectId) <- parseId "sid"
    (sur :: Srv.ServiceUpdateRequest) <- parseRequest
    A.authorize policy AT.UpdateService token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      mService <- liftIO $ CD.withDB (database config) $ MS.updateService sid (Srv.updateRequestToDocument sur)
      case mService of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Service not found"
        Just service -> do
          S.status status200
          with_host_url config $ Srv.produceServiceReply service
  S.delete "/v3/services/:sid" $ A.requireToken config $ \token -> do
    (sid :: M.ObjectId) <- parseId "sid"
    A.authorize policy AT.DeleteService token AT.EmptyResource $ do
    -- Most likely we will never need to restrict access based on service. Role based access is enough
      n <- liftIO $ CD.withDB (database config) $ MS.deleteService sid
      case n of
        Success -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find service, " ++ (show sid) ++ "."
          S.status status404
  --- Endpoint API
  S.post "/v3/endpoints" $ A.requireToken config $ \token -> do
    (ecr :: Srv.EndpointCreateRequest) <- parseRequest
    endpoint <- liftIO $ Srv.newRequestToEndpoint ecr
    A.authorize policy AT.AddEndpoint token AT.EmptyResource $ do
      mEid <- liftIO $ CD.withDB (database config) $ MS.addEndpoint (Srv.eserviceId ecr) endpoint
      case mEid of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Service not found"
        Just _eid -> do
          S.status status201
          with_host_url config $ Srv.produceEndpointReply endpoint (Srv.eserviceId ecr)
  S.get "/v3/endpoints" $ A.requireToken config $ \token -> do
    A.authorize policy AT.ListEndpoints token AT.EmptyResource $ do
      endpoints <- liftIO $ CD.withDB (database config) $ MS.listEndpoints
      S.status status200
      with_host_url config $ Srv.produceEndpointsReply endpoints
  S.get "/v3/endpoints/:eid" $ A.requireToken config $ \token -> do
    (eid :: M.ObjectId) <- parseId "eid"
    A.authorize policy AT.ShowEndpoint token AT.EmptyResource $ do
      mEndpoint <- liftIO $ CD.withDB (database config) $ MS.findEndpointById eid
      case mEndpoint of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "Endpoint not found"
        Just (serviceId, endpoint) -> do
          S.status status200
          with_host_url config $ Srv.produceEndpointReply endpoint serviceId
  S.delete "/v3/endpoints/:eid" $ A.requireToken config $ \token -> do
    (eid :: M.ObjectId) <- parseId "eid"
    A.authorize policy AT.DeleteEndpoint token AT.EmptyResource $ do
      n <- liftIO $ CD.withDB (database config) $ MS.deleteEndpoint eid
      case n of
        Success -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find endpoint, " ++ (show eid) ++ "."
          S.status status404
  -- Domain API
  S.get "/v3/domains" $ A.requireToken config $ \token -> do
    A.authorize policy AT.ListDomains token AT.EmptyResource $ do
      S.status status200
      with_host_url config $ D.produceDomainsReply []
  S.get "/v3/domains/:did" $ A.requireToken config $ \token -> do
    (did :: M.ObjectId) <- parseId "did"
    A.authorize policy AT.ShowDomainDetails token AT.EmptyResource $ do
      S.status status200
      with_host_url config $ D.produceDomainReply MD.Domain
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
          with_host_url config $ P.produceProjectReply project
  S.get "/v3/projects" $ A.requireToken config $ \token -> do
    projectName <- parseMaybeString "name"
    A.authorize policy AT.ListProjects token AT.EmptyResource $ do
      projects <- liftIO $ CD.withDB (database config) $ MP.listProjects projectName
      S.status status200
      with_host_url config $ P.produceProjectsReply projects
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
          with_host_url config $ P.produceProjectReply project
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
      with_host_url config $ R.produceRolesReply roles
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
          with_host_url config $ U.produceUserReply user
  S.get "/v3/users" $ A.requireToken config $ \token -> do
    userName <- parseMaybeString "name"
    A.authorize policy AT.ListUsers token AT.EmptyResource $ do
      users <- lift $ listUsers userName
      S.status status200
      with_host_url config $ U.produceUsersReply users
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
          with_host_url config $ U.produceUserReply user
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
          with_host_url config $ U.produceUserReply user
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
      with_host_url config $ P.produceProjectsReply projects
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
              with_host_url config $ U.produceUserReply modifiedUser
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
          with_host_url config $ R.produceRoleReply role
  S.get "/v3/roles" $ A.requireToken config $ \token -> do
    roleName <- parseMaybeString "name"
    A.authorize policy AT.ListRoles token AT.EmptyResource $ do
      roles <- liftIO $ CD.withDB (database config) $ MR.listRoles roleName
      S.status status200
      with_host_url config $ R.produceRolesReply roles
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
          with_host_url config $ R.produceRoleReply role
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
      with_host_url config $ MA.produceAssignmentsReply assignments

verifyDatabase :: KeystoneConfig -> IO ()
verifyDatabase KeystoneConfig{..} = liftIO $ CD.withDB database $ do
  liftIO $ noticeM loggerName "Verifying user collection"
  MU.verifyDatabase
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

parseMaybeString :: (MonadIO m) => TL.Text -> ActionM m (Maybe String)
parseMaybeString paramName =
  (flip S.rescue) (\msg -> return Nothing) $ do
    (value :: String) <- S.param paramName
    return $ Just value

parseMaybeParam :: (MonadIO m, Read a) => TL.Text -> ActionM m (Maybe a)
parseMaybeParam paramName =
  (flip S.rescue) (\msg -> return Nothing) $ do
    (value :: String) <- S.param paramName
    case readMaybe value of
      Nothing -> S.raise $ E.badRequest $ "Failed to parse value from " ++ (TL.unpack paramName)
      Just v  -> return $ Just v

parseId :: (MonadIO m, Read a) => TL.Text -> ActionM m a
parseId paramName = do
  s <- S.param paramName
  case readMaybe s of
    Nothing -> S.raise $ E.badRequest $ "Failed to parse ObjectId from " ++ (TL.unpack paramName)
    Just v  -> return v

parseRequest :: ( Show a
                , FromJSON a
                , BackendApi (b IO)
                , MonadIO (b IO))
                => ActionM (b IO) a
parseRequest = do
  request <- S.rescue S.jsonData $ \e ->
    S.raise $ E.badRequest $ E.message e
  liftIO $ debugM loggerName $ "Parsed request body: " ++ (show request)
  return request

hXSubjectToken :: TL.Text
hXSubjectToken = "X-Subject-Token"

host_url :: MonadIO b => ServerType -> ActionM b (Maybe String)
host_url st = do
  mh <- S.header "host"
  let protocol =
          case st of
            Plain -> "http"
            Tls   -> "https"
  return $ fmap (\h -> protocol ++ "://" ++ (TL.unpack h)) mh

getBaseUrl :: MonadIO b => KeystoneConfig -> ActionM b String
getBaseUrl config = do
  case endpoint config of
    Just e -> return e
    Nothing -> do
      mh <- host_url $ serverType config
      case mh of
        Just h -> return h
        Nothing -> S.raise $ E.badRequest "Host header is required or endpoint should be set"

with_host_url :: ( Functor b
                 , MonadIO b)
                 => KeystoneConfig -> UrlBasedValue -> ActionM b ()
with_host_url config v = do
  pathString <- BS.unpack <$> rawPathInfo <$> S.request
  queryString <- BS.unpack <$> rawQueryString <$> S.request
  url <- getBaseUrl config
  S.json $ v (UrlInfo url pathString queryString)
