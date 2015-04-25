{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Common (loggerName, ScottyM, ActionM)
import Config (readConfig, KeystoneConfig(..), Database(..))
import Control.Applicative ((<*>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Error (ErrorT(..))
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
                                 , status404, status500)
import Network.Wai ( Middleware, requestHeaders, responseLBS, rawQueryString
                   , rawPathInfo, requestMethod
                   )
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger ( debugM, setLevel, updateGlobalLogger, Priority(..)
                         , addHandler)
import System.Log.Formatter (simpleLogFormatter)

import Text.Read (readMaybe)

import Version (apiV3, apiVersions)
import Web.Scotty.Internal.Types (ActionT(..))

import qualified Auth as A
import qualified Common.Database as CD
import qualified Error as E
import qualified Database.MongoDB as M
import qualified Data.Text.Lazy as T
import qualified Web.Scotty.Trans as S
import qualified Service as Srv
import qualified Model.Service as MS
import qualified Model.Token as MT
import qualified Model.User as MU
import qualified User as U

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
  runTLS settings serverSettings app

application :: KeystoneConfig -> ScottyM ()
application config = do
  S.middleware (withAuth $ adminToken config)
  S.defaultHandler $ \e -> do
    S.status $ E.code e
    S.json e
  S.get "/" $ do
    with_host_url config apiVersions
  S.get "/v3" $ do
    with_host_url config apiV3
  S.post "/v3/users" $ do
    pipe <- CD.connect $ database $ config
    (d :: U.UserCreateRequest) <- parseRequest
    cryptedPassword <- runMaybeT $ do
      p <- MaybeT $ return $ U.password d
      p1 <- liftIO $ makePassword (pack p) 17
      return $ unpack p1
    let u = MU.User
                (Just $ U.description d)
                (Just $ U.email d)
                (U.enabled d)
                (U.name d)
                (cryptedPassword)
    e <- CD.runDB pipe $ MU.createUser u
    S.status status201
    liftIO $ M.close pipe
  S.post "/v3/auth/tokens" $ do
    (au :: A.AuthRequest) <- parseRequest
    liftIO $ debugM loggerName $ show au
    pipe <- CD.connect $ database $ config
    res <- mapM (A.authenticate pipe) (A.methods au)
    case head res of
      Right (tokenId, t) -> do
        resp <- CD.runDB pipe $ MT.produceTokenResponse t
        S.json resp
        S.addHeader "X-Subject-Token" (T.pack tokenId)
        S.status status200
      Left errorMessage -> do
        S.json $ E.unauthorized errorMessage
        S.status status401
    liftIO $ M.close pipe
  S.get "/v3/auth/tokens" $ do
    mSubjectToken <- S.header hXSubjectToken
    pipe <- CD.connect $ database $ config
    res <- runErrorT $ do
      when (isNothing mSubjectToken) $ fail "Could not find token, ."
      let mst = readMaybe $ T.unpack $ fromJust mSubjectToken

      when (isNothing mst) $ fail "Token is not an object id"
      let st = fromJust mst
      mToken <- lift $ CD.runDB pipe $ MT.findTokenById st

      when (isNothing mToken) $ fail $ "Could not find token, " ++ (show st) ++ "."
      let token = fromJust mToken
      currentTime <- liftIO getCurrentTime

      when (currentTime > (MT.expiresAt token)) $ fail $ "Could not find token, " ++ (show st) ++ "."
      lift $ CD.runDB pipe $ MT.produceTokenResponse token
    liftIO $ M.close pipe

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
    pipe <- CD.connect $ database $ config
    sid <- CD.runDB pipe $ MS.createService service
    liftIO $ M.close pipe
    S.status status201
    with_host_url config $ MS.produceServiceReply service sid
  S.get "/v3/services" $ do
    pipe <- CD.connect $ database $ config
    services <- CD.runDB pipe $ MS.listServices
    liftIO $ M.close pipe
    S.status status200
    with_host_url config $ MS.produceServicesReply services
  S.get "/v3/services/:sid" $ do
    (sid :: M.ObjectId) <- parseId "sid"
    pipe <- CD.connect $ database $ config
    mService <- CD.runDB pipe $ MS.findServiceById sid
    liftIO $ M.close pipe
    case mService of
      Nothing -> do
        S.status status404
        S.json $ E.notFound "Service not found"
      Just service -> do
        S.status status200
        with_host_url config $ MS.produceServiceReply service sid
  S.delete "/v3/services/:sid" $ do
    (sid :: M.ObjectId) <- parseId "sid"
    pipe <- CD.connect $ database $ config
    n <- CD.runDB pipe $ MS.deleteService sid
    liftIO $ M.close pipe
    if n < 1
      then do
        S.json $ E.notFound $ "Could not find service, " ++ (show sid) ++ "."
        S.status status404
      else S.status status204

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

withAuth :: String -> Middleware
withAuth adminToken app req respond = do
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
          if m /= (pack adminToken)
            then respond $ responseLBS status401 [] "Wrong token"
            else app req respond

hXAuthToken :: HeaderName
hXAuthToken = "X-Auth-Token"

hXSubjectToken :: T.Text
hXSubjectToken = "X-Subject-Token"

host_url :: ActionM (Maybe String)
host_url = do
  mh <- S.header "host"
  return $ fmap (\h -> "https://" ++ (T.unpack h)) mh

with_host_url :: KeystoneConfig -> (String -> Value) -> ActionM ()
with_host_url config v = do
  case endpoint config of
    Just e -> S.json $ v e
    Nothing -> do
      mh <- host_url
      case mh of
        Just h -> S.json $ v h
        Nothing -> S.raise $ E.badRequest "Host header is required or endpoint should be set"
