{-# LANGUAGE OverloadedStrings #-}
module Main
where

import Web.Scotty (scotty, get, json)
import Version (apiV3, apiVersions)

main = scotty 3000 $ do
  get "/" $ do
    json $ apiV3 "http://localhost" -- TODO It should be taken from the web server
  get "/v3" $ do
    json $ apiVersions "http://localhost" -- TODO It should be taken from the web server
