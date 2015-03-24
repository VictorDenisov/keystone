{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import qualified Auth as A
import Config (readConfig, KeystoneConfig(..), Database(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson.Types (Value)
import Data.Bson ((=:))
import Data.ByteString.Char8 (pack, unpack)
import Data.List (lookup)
import Data.Maybe (isNothing, maybe)
import qualified Database.MongoDB as M
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status201, status401)
import Network.Wai (Middleware, requestHeaders, responseLBS, rawQueryString)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import qualified User as U
import qualified Model.User as MU
import Version (apiV3, apiVersions)
import Web.Scotty.Internal.Types (ActionT(..))

main = do
  config <- readConfig
  app <- S.scottyApp (application config)
  let settings = tlsSettings
                      (certificateFile config)
                      (keyFile config)
  let serverSettings = setPort (port config) defaultSettings
  runTLS settings serverSettings app

application :: KeystoneConfig -> S.ScottyM ()
application config = do
  S.middleware (withAuth $ adminToken config)
  S.get "/" $ do
    with_host_url config apiVersions
  S.get "/v3" $ do
    with_host_url config apiV3
  S.post "/v3/users" $ do
    pipe <- liftIO $ M.connect (M.host $ dbHost $ database $ config)
    (d :: U.UserCreateRequest) <- S.jsonData
    let u = MU.User (Just $ U.description d) (Just $ U.email d) (U.enabled d) (U.name d) (U.password d)
    e <- M.access pipe M.master dbName (MU.createUser u)
    S.status status201
    liftIO $ M.close pipe
  S.post "/v3/auth/tokens" $ do
    pipe <- liftIO $ M.connect (M.host $ dbHost $ database $ config)
    (d :: A.AuthRequest) <- S.jsonData
    --e <- M.access pipe M.master dbName (MU.createUser u)
    S.status status201
    liftIO $ M.close pipe

dbName = "keystone"

withAuth :: String -> Middleware
withAuth adminToken app req respond = do
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

host_url :: S.ActionM (Maybe String)
host_url = do
  mh <- S.header "host"
  return $ fmap (\h -> "https://" ++ (T.unpack h)) mh

with_host_url :: KeystoneConfig -> (String -> Value) -> S.ActionM ()
with_host_url config v = do
  case endpoint config of
    Just e -> S.json $ v e
    Nothing -> do
      mh <- host_url
      case mh of
        Just h -> S.json $ v h
        Nothing -> S.raise "Host header is required or endpoint should be set"
