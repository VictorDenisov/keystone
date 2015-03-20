{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Config (readConfig, KeystoneConfig(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bson ((=:))
import Data.ByteString.Char8 (pack, unpack)
import Data.List (lookup)
import Data.Maybe (isNothing)
import qualified Database.MongoDB as M
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status201, status401)
import Network.Wai (Middleware, requestHeaders, responseLBS)
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
    S.json $ apiV3 "http://localhost" -- TODO It should be taken from the web server
  S.get "/v3" $ do
    S.json $ apiVersions "http://localhost" -- TODO It should be taken from the web server
  S.post "/v3/users" $ do
    pipe <- liftIO $ M.connect (M.host "127.0.0.1")
    (d :: U.UserCreateRequest) <- S.jsonData
    let u = MU.User (Just $ U.description d) (Just $ U.email d) (U.enabled d) (U.name d) (U.password d)
    e <- M.access pipe M.master "keystone" (MU.createUser u)
    S.status status201
    liftIO $ M.close pipe

hXAuthToken :: HeaderName
hXAuthToken = "X-Auth-Token"

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
