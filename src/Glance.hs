{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Common (loggerName)
import Config (readConfig, ServerType(..))
import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Lazy.Char8 (pack)
import Data.Time.Clock (getCurrentTime)
import Glance.Config (confFileName, GlanceConfig(..))
import Glance.Web.Image ( listImagesH, createImageH, uploadImageH, downloadImageH
                        , imageDetailsH)
import Glance.Web.Schema ( imagesSchemaH, imageSchemaH
                         , metadefsNamespaceSchemaH, metadefsResourceTypeSchemaH
                         , metadefsNamespacesSchemaH, metadefsObjectsSchemaH
                         , metadefsResourceTypesSchemaH, metadefsTagSchemaH
                         , metadefsTagsSchemaH, metadefsObjectSchemaH
                         , metadefsPropertySchemaH, metadefsPropertiesSchemaH
                         , memberSchemaH, membersSchemaH
                         )
import Glance.Web.Version (listVersionsH)
import Network.HTTP.Types.Status (statusCode, status500)
import Network.Wai (Middleware, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import System.IO (stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger ( setLevel, updateGlobalLogger, setHandlers
                         , removeAllHandlers, noticeM, errorM, debugM)
import Web.Common (ScottyM)

import qualified Error as E
import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S

main = do
  (config :: GlanceConfig) <- readConfig confFileName
  let logFormatter = simpleLogFormatter "$utcTime (pid $pid, $tid) $prio: $msg"
  stdoutHandler <- streamHandler stdout (logLevel config)
  fileHandler <- fileHandler "glance.log" (logLevel config)
  removeAllHandlers
  updateGlobalLogger loggerName $ setLevel (logLevel config) . setHandlers
    [ setFormatter stdoutHandler logFormatter
    , setFormatter fileHandler   logFormatter
    ]

  !policy <- A.loadPolicy
  -- ^ bang pattern is because we want to know if the policy is correct now
  -- ^ we need the evaluation to happen immediatelly
  verifyDatabase config

  app <- S.scottyAppT id (application policy config)

  let settings = tlsSettings
                      (certificateFile config)
                      (keyFile config)

  let serverSettings = setPort (port config) defaultSettings
  noticeM loggerName $ "Starting web server @ port " ++ (show $ port config)
  case serverType config of
    Tls   -> runTLS settings serverSettings app
    Plain -> runSettings serverSettings app

application :: AT.Policy
            -> GlanceConfig
            -> ScottyM IO ()
application policy config = do
  S.middleware exceptionCatchMiddleware
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
  S.get   "/"                                   $ listVersionsH                config
  S.get   "/versions"                           $ listVersionsH                config
  S.get   "/v2/schemas/images"                  $ imagesSchemaH                config
  S.get   "/v2/schemas/image"                   $ imageSchemaH                 config
  S.get   "/v2/schemas/members"                 $ membersSchemaH               config
  S.get   "/v2/schemas/member"                  $ memberSchemaH                config
  S.get   "/v2/schemas/metadefs/namespace"      $ metadefsNamespaceSchemaH     config
  S.get   "/v2/schemas/metadefs/namespaces"     $ metadefsNamespacesSchemaH    config
  S.get   "/v2/schemas/metadefs/object"         $ metadefsObjectSchemaH        config
  S.get   "/v2/schemas/metadefs/objects"        $ metadefsObjectsSchemaH       config
  S.get   "/v2/schemas/metadefs/property"       $ metadefsPropertySchemaH      config
  S.get   "/v2/schemas/metadefs/properties"     $ metadefsPropertiesSchemaH    config
  S.get   "/v2/schemas/metadefs/resource_type"  $ metadefsResourceTypeSchemaH  config
  S.get   "/v2/schemas/metadefs/resource_types" $ metadefsResourceTypesSchemaH config
  S.get   "/v2/schemas/metadefs/tag"            $ metadefsTagSchemaH           config
  S.get   "/v2/schemas/metadefs/tags"           $ metadefsTagsSchemaH          config
  S.get   "/v2/images"                          $ listImagesH                  config
  S.post  "/v2/images"                          $ createImageH                 config
  S.put   "/v2/images/:iid/file"                $ uploadImageH                 config
  S.get   "/v2/images/:iid/file"                $ downloadImageH               config
  S.get   "/v2/images/:iid"                     $ imageDetailsH                config

verifyDatabase :: GlanceConfig -> IO ()
verifyDatabase GlanceConfig{..} = return () -- TODO

exceptionCatchMiddleware :: Middleware
exceptionCatchMiddleware app request responder = do
  (app request responder)
    `catch`
      (\(e :: SomeException) -> do
        time <- liftIO $ getCurrentTime
        errorM loggerName $ show e
        let message = "Internal error. Server time - " ++ (show time) ++ "\n"
        responder $ responseLBS status500 [] (pack message)
      )

logRequestResponse :: Middleware
logRequestResponse app request responder = do
  debugM loggerName $ show request
  app request responder
