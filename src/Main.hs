{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Common (loggerName)
import Config (readConfig, KeystoneConfig(..), Database(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.PasswordStore (makePassword)
import Data.Aeson.Types (Value)
import Data.Bson ((=:))
import Data.ByteString.Char8 (pack, unpack)
import Data.List (lookup, or)
import Data.Maybe (isNothing, maybe)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (methodPost)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status ( status200, status201, status401, status404
                                 , status500)
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
import Version (apiV3, apiVersions)
import Web.Scotty.Internal.Types (ActionT(..))
import Web.Scotty.Trans (ScottyError(..))

import qualified Auth as A
import qualified Common.Database as CD
import qualified Error as E
import qualified Database.MongoDB as M
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S
import qualified Model.Token as MT
import qualified Model.User as MU
import qualified User as U

main = do
  config <- readConfig
  CD.verifyDatabase $ database config
  updateGlobalLogger loggerName $ setLevel $ logLevel config
  fh <- fileHandler "keystone.log" DEBUG
  updateGlobalLogger loggerName $ addHandler $ setFormatter fh (simpleLogFormatter "$utcTime $prio: $msg")

  app <- S.scottyApp (application config)
  let settings = tlsSettings
                      (certificateFile config)
                      (keyFile config)
  let serverSettings = setPort (port config) defaultSettings
  runTLS settings serverSettings app

application :: KeystoneConfig -> S.ScottyM ()
application config = do
  S.middleware (withAuth $ adminToken config)
  S.defaultHandler $ \e -> do
    S.status status500
    S.text $ showError e
  S.get "/" $ do
    with_host_url config apiVersions
  S.get "/v3" $ do
    with_host_url config apiV3
  S.post "/v3/users" $ do
    pipe <- CD.connect $ database $ config
    (d :: U.UserCreateRequest) <- S.jsonData
    cryptedPassword <- case U.password d of
      Nothing -> return Nothing
      Just p  -> do
        p1 <- liftIO $ makePassword (pack p) 17
        return $ Just $ unpack p1
    let u = MU.User (Just $ U.description d) (Just $ U.email d) (U.enabled d) (U.name d) (cryptedPassword)
    e <- CD.runDB pipe $ MU.createUser u
    S.status status201
    liftIO $ M.close pipe
  S.post "/v3/auth/tokens" $ do
    (au :: A.AuthRequest) <- S.jsonData
    liftIO $ putStrLn $ show au
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
    case mSubjectToken of
      Nothing -> do
        S.json $ E.notFound "Could not find token, ."
        S.status status404
      Just subjectToken -> do
        let st = T.unpack subjectToken
        pipe <- CD.connect $ database $ config
        mToken <- CD.runDB pipe $ MT.findTokenById st
        case mToken of
          Nothing -> do
            S.json $ E.notFound $ "Could not find token, " ++ st ++ "."
            S.status status404
          Just token -> do
            currentTime <- liftIO getCurrentTime
            if currentTime > (MT.expiresAt token)
              then do
                S.json $ E.notFound $ "Could not find token, " ++ st ++ "."
                S.status status404
              else do
                resp <- CD.runDB pipe $ MT.produceTokenResponse token
                S.status status200
                S.json resp
        liftIO $ M.close pipe

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
