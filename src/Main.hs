{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bson ((=:))
import Data.Maybe (isNothing)
import qualified Database.MongoDB as M
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (status401)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import qualified User as U
import qualified Model.User as MU
import Version (apiV3, apiVersions)
import Web.Scotty.Internal.Types (ActionT(..))

main = do
  app <- S.scottyApp application
  let settings = tlsSettings "server.crt" "server.key"
  runTLS settings defaultSettings app

application = do
  S.get "/" $ do
    S.json $ apiV3 "http://localhost" -- TODO It should be taken from the web server
  S.get "/v3" $ do
    S.json $ apiVersions "http://localhost" -- TODO It should be taken from the web server
  S.post "/v3/users" $ do
    pipe <- liftIO $ M.connect (M.host "127.0.0.1")
    S.rescue (do
        mToken <- S.header "X-Auth-Token"
        when (isNothing mToken) $ S.raise "AUTH: Token is required"
        let Just token = mToken
        when (token /= "ADMIN_TOKEN") $ S.raise "AUTH: Wrong token"
        (d :: U.UserCreateRequest) <- S.jsonData
        liftIO $ putStrLn $ show d
        let u = MU.User (Just $ U.description d) (Just $ U.email d) (U.enabled d) (U.name d) (U.password d)
        e <- M.access pipe M.master "keystone" (MU.createUser u)
        liftIO $ print e
        S.text $ "Hello"
      ) (\e -> do
          when ((T.take 5 e) == "AUTH:") $ S.status status401
          S.text (T.drop 5 e)
        )
    liftIO $ M.close pipe
