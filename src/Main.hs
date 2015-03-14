{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import qualified Data.Text.Lazy as T
import qualified Web.Scotty as S
import Network.HTTP.Types.Status (status401)
import User (UserCreateRequest)
import Version (apiV3, apiVersions)

main = S.scotty 3000 $ do
  S.get "/" $ do
    S.json $ apiV3 "http://localhost" -- TODO It should be taken from the web server
  S.get "/v3" $ do
    S.json $ apiVersions "http://localhost" -- TODO It should be taken from the web server
  S.post "/v3/users" $ do
    S.rescue (do
        mToken <- S.header "X-Auth-Token"
        when (isNothing mToken) $ S.raise "AUTH: Token is required"
        let Just token = mToken
        when (token /= "ADMIN_TOKEN") $ S.raise "AUTH: Wrong token"
        (d :: UserCreateRequest) <- S.jsonData
        liftIO $ putStrLn $ show d
        S.text $ "Hello" 
      ) (\e -> do
          when ((T.take 5 e) == "AUTH:") $ S.status status401
          S.text (T.drop 5 e)
        )
