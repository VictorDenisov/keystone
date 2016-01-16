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
import Config (readConfig, ServerType(..))
import Control.Monad (when)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Time.Clock (getCurrentTime)
import Keystone.Config (KeystoneConfig(..), confFileName)
import Keystone.Model.IdentityApi
import Keystone.Model.Mongo.IdentityApi
import Keystone.Model.Ldap.IdentityApi
import Network.HTTP.Types.Method (StdMethod(HEAD))
import Network.HTTP.Types.Status ( statusCode)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings)
import System.IO (stdout)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger ( debugM, errorM, setLevel, updateGlobalLogger
                         , noticeM, setHandlers, removeAllHandlers)
import System.Log.Formatter (simpleLogFormatter)

import Keystone.Web.Version (listVersionsH, v3detailsH)

import Web.Common (ScottyM)

import qualified Model.Mongo.Common as CD

import qualified Error as E

import qualified Keystone.Model.Assignment as MA
import qualified Keystone.Model.Project as MP
import qualified Keystone.Model.Role as MR
import qualified Keystone.Model.Service as MS
import qualified Keystone.Model.Token as MT
import qualified Keystone.Model.Mongo.User as MMU

import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Keystone.Web.Assignment as Assig
import qualified Keystone.Web.Domain as D
import qualified Keystone.Web.Project as P
import qualified Keystone.Web.Role as R
import qualified Web.Scotty.Trans as S
import qualified Keystone.Web.Service as SC
import qualified Keystone.Web.Token as T
import qualified Keystone.Web.User as U

main = do
  config <- readConfig confFileName
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

  app <- case ldap config of
    Nothing -> do
      let runMongo = runMongoBackend $ database config
      S.scottyAppT runMongo runMongo (application policy config)
    Just ldapConfig -> do
      let runLdap = runLdapBackend $ ldapConfig
      S.scottyAppT runLdap runLdap (application policy config)

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
  S.get           "/"                     $ listVersionsH                  config
  S.get           "/v3"                   $ v3detailsH                     config
  -- Token API
  S.post          "/v3/auth/tokens"       $ T.issueTokenH                  config
  S.get           "/v3/auth/tokens"       $ T.receiveExistingTokenH policy config
  S.addroute HEAD "/v3/auth/tokens"       $ T.checkTokenH           policy config
  -- Service Catalog API
  -- Service API
  S.post          "/v3/services"          $ SC.createServiceH       policy config
  S.get           "/v3/services"          $ SC.listServicesH        policy config
  S.get           "/v3/services/:sid"     $ SC.serviceDetailsH      policy config
  S.patch         "/v3/services/:sid"     $ SC.updateServiceH       policy config
  S.delete        "/v3/services/:sid"     $ SC.deleteServiceH       policy config
  -- Endpoint API
  S.post          "/v3/endpoints"         $ SC.createEndpointH      policy config
  S.get           "/v3/endpoints"         $ SC.listEndpointsH       policy config
  S.get           "/v3/endpoints/:eid"    $ SC.endpointDetailsH     policy config
  S.delete        "/v3/endpoints/:eid"    $ SC.deleteEndpointH      policy config
  -- Domain API
  S.get           "/v3/domains"           $ D.listDomainsH          policy config
  S.get           "/v3/domains/:did"      $ D.domainDetailsH        policy config
  -- Project API
  S.post          "/v3/projects"          $ P.createProjectH        policy config
  S.get           "/v3/projects"          $ P.listProjectsH         policy config
  S.get           "/v3/projects/:pid"     $ P.projectDetailsH       policy config
  S.delete        "/v3/projects/:pid"     $ P.deleteProjectH        policy config
  -- User API
  S.post          "/v3/users"               $ U.createUserH         policy config
  S.get           "/v3/users"               $ U.listUsersH          policy config
  S.get           "/v3/users/:uid"          $ U.userDetailsH        policy config
  S.patch         "/v3/users/:uid"          $ U.updateUserH         policy config
  S.delete        "/v3/users/:uid"          $ U.deleteUserH         policy config
  S.post          "/v3/users/:uid/password" $ U.updateUserPasswordH policy config
  -- Role API
  S.post          "/v3/roles"               $ R.createRoleH         policy config
  S.get           "/v3/roles"               $ R.listRolesH          policy config
  S.get           "/v3/roles/:rid"          $ R.roleDetailsH        policy config
  S.delete        "/v3/roles/:rid"          $ R.deleteRoleH         policy config
  -- Assignment API
  S.get "/v3/projects/:pid/users/:uid/roles"      $ Assig.listProjectUserRolesH policy config
  S.put "/v3/projects/:pid/users/:uid/roles/:rid" $ Assig.createAssignmentH     policy config
  S.get "/v3/users/:uid/projects"                 $ Assig.listUserProjects      policy config
  S.get "/v3/role_assignments"                    $ Assig.listAssignmentsH      policy config

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
