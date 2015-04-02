{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import qualified Auth as A
import Config (readConfig, KeystoneConfig(..), Database(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.PasswordStore (makePassword)
import Data.Aeson.Types (Value)
import Data.Bson ((=:))
import Data.ByteString.Char8 (pack, unpack)
import Data.List (lookup, or)
import Data.Maybe (isNothing, maybe)
import qualified Database.MongoDB as M
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S
import qualified Model.User as MU
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status200, status201, status401)
import Network.Wai ( Middleware, requestHeaders, responseLBS, rawQueryString
                   , rawPathInfo
                   )
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import qualified User as U
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
    cryptedPassword <- case U.password d of
      Nothing -> return Nothing
      Just p  -> do
        p1 <- liftIO $ makePassword (pack p) 17
        return $ Just $ unpack p1
    let u = MU.User (Just $ U.description d) (Just $ U.email d) (U.enabled d) (U.name d) (cryptedPassword)
    e <- M.access pipe M.master dbName (MU.createUser u)
    S.status status201
    liftIO $ M.close pipe
  S.post "/v3/auth/tokens" $ do
    (au :: A.AuthRequest) <- S.jsonData
    liftIO $ putStrLn $ show au
    pipe <- liftIO $ M.connect (M.host $ dbHost $ database $ config)
    res <- mapM (A.authenticate pipe) (A.methods au)
    if or res
      then S.status status200
      else S.status status401
    liftIO $ M.close pipe

dbName = "keystone"

withAuth :: String -> Middleware
withAuth adminToken app req respond = do
  liftIO $ putStrLn $ unpack $ rawPathInfo req
  if rawPathInfo req == "v3/auth/tokens"
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
