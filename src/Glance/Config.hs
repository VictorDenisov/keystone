{-# LANGUAGE TemplateHaskell #-}
module Glance.Config
where

import Config (Database(..), ServerType(..), BaseConfig(..))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Default (Default(..))
import System.Log.Logger (Priority(NOTICE))

data GlanceConfig = GlanceConfig
                  { certificateFile       :: FilePath -- TLS runner checks if this file exists
                  , keyFile               :: FilePath -- TLS runner checks if this file exists
                  , port                  :: Int      -- Port won't bind if it's busy
                  , endpoint              :: Maybe String
                  , database              :: Database
                  , logLevel              :: Priority
                  , serverType            :: ServerType
                  }

defaultConfig :: GlanceConfig
defaultConfig = GlanceConfig
              { certificateFile       = "server.crt"
              , keyFile               = "server.key"
              , port                  = defaultPort
              , endpoint              = Nothing
              , database              = Database
                                        { dbHost = "localhost"
                                        , dbPort = 27017
                                        , dbName = "glance"
                                        }
              , logLevel              = NOTICE
              , serverType            = Plain
              }
  where defaultPort = 9191

instance Default GlanceConfig where
  def = defaultConfig

instance BaseConfig GlanceConfig where
  getEndpoint = endpoint
  getServerType = serverType

confFileName :: String
confFileName = "glance.conf"

$(deriveJSON defaultOptions ''GlanceConfig)
