{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Common (loggerName, ScottyM, ActionM)
import Config (readConfig, KeystoneConfig(..), Database(..), ServerType(..))
import Control.Applicative ((<*>), (<$>))
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, release)
import Crypto.PasswordStore (makePassword)
import Data.Aeson.Types (Value, FromJSON(..))
import Data.Bson ((=:))
import Data.ByteString.Char8 (pack, unpack)
import Data.List (lookup, or)
import Data.Maybe (isNothing, maybe, fromJust)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status ( status200, status201, status204, status401
                                 , status404, status500, statusCode)
import Network.Wai ( Middleware, requestHeaders, responseLBS, rawQueryString
                   , rawPathInfo, requestMethod
                   )
import Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger ( debugM, errorM, setLevel, updateGlobalLogger
                         , Priority(..), addHandler)
import System.Log.Formatter (simpleLogFormatter)

import Text.Read (readMaybe)

import Version (apiV3, apiVersions)
import Web.Scotty.Internal.Types (ActionT(..))

import qualified Auth as A
import qualified Common.Database as CD
import qualified Data.Text.Lazy as T
import qualified Database.MongoDB as M
import qualified Error as E
import qualified Model.Project as MP
import qualified Model.Role as MR
import qualified Model.Service as MS
import qualified Model.Token as MT
import qualified Model.User as MU
import qualified Project as P
import qualified Role as R
import qualified Service as Srv
import qualified User as U
import qualified Web.Scotty.Trans as S

main = do
  config <- readConfig
  CD.verifyDatabase $ database config
  updateGlobalLogger loggerName $ setLevel $ logLevel config
  fh <- fileHandler "keystone.log" DEBUG
  updateGlobalLogger loggerName $ addHandler $ setFormatter fh (simpleLogFormatter "$utcTime (pid $pid, $tid) $prio: $msg")

  app <- S.scottyAppT id id (application config)
  let settings = tlsSettings
                      (certificateFile config)
                      (keyFile config)
  let serverSettings = setPort (port config) defaultSettings
  case serverType config of
    Tls   -> runTLS settings serverSettings app
    Plain -> runSettings serverSettings app

application :: KeystoneConfig -> ScottyM ()
application config = do
  S.middleware (withAuth config)
  S.defaultHandler $ \e -> do
    S.status $ E.code e
    case statusCode $ E.code e of
      500 -> do
        time <- liftIO $ getCurrentTime
        liftIO $ errorM loggerName $ E.message e
        S.json $ e {E.message = "Internal error. Server time - " ++ (show time)}
      _ -> do
        S.json e
  S.get "/" $ do
    with_host_url config apiVersions
  S.get "/v3" $ do
    with_host_url config apiV3
  S.post "/v3/auth/tokens" $ do
    (au :: A.AuthRequest) <- parseRequest
    liftIO $ debugM loggerName $ show au
    baseUrl <- getBaseUrl config
    runResourceT $ do
      (releaseKey, pipe) <- allocate (CD.connect $ database config) M.close
      res <- mapM (A.authenticate (A.scope au) pipe) (A.methods au)
      case head res of
        Right (tokenId, t) -> lift $ do
          resp <- CD.runDB pipe $ A.produceTokenResponse t baseUrl
          S.json resp
          S.addHeader "X-Subject-Token" (T.pack tokenId)
          S.status status200
        Left errorMessage -> lift $ do
          S.json $ E.unauthorized errorMessage
          S.status status401
      release releaseKey
  S.get "/v3/auth/tokens" $ do
    mSubjectToken <- S.header hXSubjectToken
    baseUrl <- getBaseUrl config
    res <- runResourceT $ do
      (releaseKey, pipe) <- allocate (CD.connect $ database config) M.close
      runExceptT $ do
        when (isNothing mSubjectToken) $ fail "Could not find token, ."
        let mst = readMaybe $ T.unpack $ fromJust mSubjectToken

        when (isNothing mst) $ fail "Token is not an object id"
        let st = fromJust mst
        mToken <- lift $ CD.runDB pipe $ MT.findTokenById st

        when (isNothing mToken) $ fail $ "Could not find token, " ++ (show st) ++ "."
        let token = fromJust mToken
        currentTime <- liftIO getCurrentTime

        when (currentTime > (MT.expiresAt token)) $ fail $ "Could not find token, " ++ (show st) ++ "."
        lift $ CD.runDB pipe $ A.produceTokenResponse token baseUrl
        lift $ release releaseKey

    case res of
      Left errorMessage -> do
        S.status status404
        S.json $ E.notFound errorMessage
      Right resp -> do
        S.status status200
        S.json resp
  -- Service API
  S.post "/v3/services" $ do
    (scr :: Srv.ServiceCreateRequest) <- parseRequest
    let service = Srv.newRequestToService scr
    sid <- CD.withDB (database config) $ MS.createService service
    S.status status201
    with_host_url config $ MS.produceServiceReply service sid
  S.get "/v3/services" $ do
    services <- CD.withDB (database config) $ MS.listServices
    S.status status200
    with_host_url config $ MS.produceServicesReply services
  S.get "/v3/services/:sid" $ do
    (sid :: M.ObjectId) <- parseId "sid"
    mService <- CD.withDB (database config) $ MS.findServiceById sid
    case mService of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "Service not found"
      Just service -> do
        S.status status200
        with_host_url config $ MS.produceServiceReply service sid
  S.patch "/v3/services/:sid" $ do
    (sid :: M.ObjectId) <- parseId "sid"
    (sur :: Srv.ServiceUpdateRequest) <- parseRequest
    mService <- CD.withDB (database config) $ MS.updateService sid (Srv.updateRequestToDocument sur)
    case mService of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "Service not found"
      Just service -> do
        S.status status200
        with_host_url config $ MS.produceServiceReply service sid
  S.delete "/v3/services/:sid" $ do
    (sid :: M.ObjectId) <- parseId "sid"
    n <- CD.withDB (database config) $ MS.deleteService sid
    if n < 1
      then do
        S.json $ E.notFound $ "Could not find service, " ++ (show sid) ++ "."
        S.status status404
      else S.status status204
  --- Endpoint API
  S.post "/v3/endpoints" $ do
    (ecr :: Srv.EndpointCreateRequest) <- parseRequest
    let endpoint = Srv.newRequestToEndpoint ecr
    mEid <- CD.withDB (database config) $ MS.addEndpoint (Srv.eserviceId ecr) endpoint
    case mEid of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "Service not found"
      Just _eid -> do
        S.status status201
        with_host_url config $ MS.produceEndpointReply endpoint (Srv.eserviceId ecr)
  S.get "/v3/endpoints" $ do
    endpoints <- CD.withDB (database config) $ MS.listEndpoints
    S.status status200
    with_host_url config $ MS.produceEndpointsReply endpoints
  -- Project API
  S.post "/v3/projects" $ do
    (pcr :: P.ProjectCreateRequest) <- parseRequest
    let project = P.newRequestToProject pcr
    pid <- CD.withDB (database config) $ MP.createProject project
    S.status status201
    with_host_url config $ MP.produceProjectReply project pid
  S.get "/v3/projects" $ do
    projectName <- parseMaybeString "name"
    projects <- CD.withDB (database config) $ MP.listProjects projectName
    S.status status200
    with_host_url config $ MP.produceProjectsReply projects
  S.get "/v3/projects/:pid" $ do
    (pid :: M.ObjectId) <- parseId "pid"
    mProject <- CD.withDB (database config) $ MP.findProjectById pid
    case mProject of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "Project not found"
      Just project -> do
        S.status status200
        with_host_url config $ MP.produceProjectReply project pid
  S.get "/v3/projects/:pid/users/:uid/roles" $ do
    (pid :: M.ObjectId) <- parseId "pid"
    (uid :: M.ObjectId) <- parseId "uid"
    roles <- CD.withDB (database config) $ MP.listUserRoles (MP.ProjectId pid) (MU.UserId uid)
    S.status status200
    with_host_url config $ MR.produceRolesReply roles -- TODO base url should be revised here
  S.put "/v3/projects/:pid/users/:uid/roles/:rid" $ do
    (pid :: M.ObjectId) <- parseId "pid"
    (uid :: M.ObjectId) <- parseId "uid"
    (rid :: M.ObjectId) <- parseId "rid"
    res <- CD.withDB (database config) $ MP.addUserWithRole (MP.ProjectId pid) (MU.UserId uid) (MR.RoleId rid)
    S.status status204
  -- User API
  S.post "/v3/users" $ do
    (d :: U.UserCreateRequest) <- parseRequest
    cryptedPassword <- runMaybeT $ do
      p <- MaybeT $ return $ U.password d
      p1 <- liftIO $ makePassword (pack p) 17
      return $ unpack p1
    let user = MU.User
                (U.description d)
                (U.email d)
                (U.enabled d)
                (U.name d)
                (cryptedPassword)
    uid <- CD.withDB (database config) $ MU.createUser user
    S.status status201
    with_host_url config $ MU.produceUserReply user uid
  S.get "/v3/users" $ do
    userName <- parseMaybeString "name"
    liftIO $ putStrLn $ show userName
    users <- CD.withDB (database config) $ MU.listUsers userName
    S.status status200
    with_host_url config $ MU.produceUsersReply users
  S.get "/v3/users/:uid" $ do
    (uid :: M.ObjectId) <- parseId "uid"
    mUser <- CD.withDB (database config) $ MU.findUserById uid
    case mUser of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "User not found"
      Just user -> do
        S.status status200
        with_host_url config $ MU.produceUserReply user uid
  S.patch "/v3/users/:uid" $ do
    (uid :: M.ObjectId) <- parseId "uid"
    (uur :: U.UserUpdateRequest) <- parseRequest
    mUser <- CD.withDB (database config) $ MU.updateUser uid (U.updateRequestToDocument uur)
    case mUser of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "User not found"
      Just user -> do
        S.status status200
        with_host_url config $ MU.produceUserReply user uid
  S.delete "/v3/users/:uid" $ do
    (uid :: M.ObjectId) <- parseId "uid"
    n <- CD.withDB (database config) $ MU.deleteUser uid
    if n < 1
      then do
        S.json $ E.notFound $ "Could not find user, " ++ (show uid) ++ "."
        S.status status404
      else S.status status204
  -- Role API
  S.post "/v3/roles" $ do
    (rcr :: R.RoleCreateRequest) <- parseRequest
    let role = R.newRequestToRole rcr
    rid <- CD.withDB (database config) $ MR.createRole role
    S.status status201
    with_host_url config $ MR.produceRoleReply role rid
  S.get "/v3/roles" $ do
    roles <- CD.withDB (database config) $ MR.listRoles
    S.status status200
    with_host_url config $ MR.produceRolesReply roles
  S.get "/v3/roles/:rid" $ do
    (rid :: M.ObjectId) <- parseId "rid"
    mRole <- CD.withDB (database config) $ MR.findRoleById rid
    case mRole of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "Role not found"
      Just role -> do
        S.status status200
        with_host_url config $ MR.produceRoleReply role rid
  S.get "/v3/role_assignments" $ do
    userId <- parseMaybeParam "user.id"
    projectId <- parseMaybeParam "scope.project.id"
    roles <- CD.withDB (database config) $ MP.listAssignments (MP.ProjectId <$> projectId) (MU.UserId <$> userId)
    S.status status200
    with_host_url config $ MP.produceAssignmentsReply roles -- TODO base url should be revised here

parseMaybeString :: T.Text -> ActionM (Maybe String)
parseMaybeString paramName =
  (flip S.rescue) (\msg -> return Nothing) $ do
    (value :: String) <- S.param paramName
    return $ Just value

parseMaybeParam :: Read a => T.Text -> ActionM (Maybe a)
parseMaybeParam paramName =
  (flip S.rescue) (\msg -> return Nothing) $ do
    (value :: String) <- S.param paramName
    case readMaybe value of
      Nothing -> S.raise $ E.badRequest $ "Failed to parse value from " ++ (T.unpack paramName)
      Just v  -> return $ Just v

parseId :: Read a => T.Text -> ActionM a
parseId paramName = do
  s <- S.param paramName
  case readMaybe s of
    Nothing -> S.raise $ E.badRequest $ "Failed to parse ObjectId from " ++ (T.unpack paramName)
    Just v  -> return v

parseRequest :: FromJSON a => ActionM a
parseRequest = do
  S.rescue S.jsonData $ \e ->
    S.raise $ E.badRequest $ E.message e

withAuth :: KeystoneConfig -> Middleware
withAuth config app req respond = do
  let adminToken = Config.adminToken config
  liftIO $ debugM loggerName $ unpack $ rawPathInfo req
  if (requestMethod req == methodPost) && (rawPathInfo req == "/v3/auth/tokens")
    then
      app req respond
    else do
      let
        rh = requestHeaders req
        mToken = lookup hXAuthToken rh
      case mToken of
        Nothing -> respond $ responseLBS status401 [] "Token is required"
        Just m ->
          if m == (pack adminToken)
            then app req respond
            else do
              let mTokenId = readMaybe $ unpack m
              case mTokenId of
                Nothing -> respond $ responseLBS status401 [] "Wrong token"
                Just tokenId  -> do
                  mToken <- CD.withDB (database config) $ MT.findTokenById tokenId
                  case mToken of
                    Nothing -> respond $ responseLBS status401 [] "Wrong token"
                    Just token -> app req respond -- TODO verify that this user has access

hXAuthToken :: HeaderName
hXAuthToken = "X-Auth-Token"

hXSubjectToken :: T.Text
hXSubjectToken = "X-Subject-Token"

host_url :: ServerType -> ActionM (Maybe String)
host_url st = do
  mh <- S.header "host"
  let protocol =
          case st of
            Plain -> "http"
            Tls   -> "https"
  return $ fmap (\h -> protocol ++ "://" ++ (T.unpack h)) mh

getBaseUrl :: KeystoneConfig -> ActionM String
getBaseUrl config = do
  case endpoint config of
    Just e -> return e
    Nothing -> do
      mh <- host_url $ serverType config
      case mh of
        Just h -> return h
        Nothing -> S.raise $ E.badRequest "Host header is required or endpoint should be set"

with_host_url :: KeystoneConfig -> (String -> Value) -> ActionM ()
with_host_url config v = do
  url <- getBaseUrl config
  S.json $ v url
