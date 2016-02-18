{-# LANGUAGE TemplateHaskell #-}
module Nova.Config
where

import Config (Database(..), ServerType(..), BaseConfig(..))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Default (Default(..))
import System.Log.Logger (Priority(NOTICE))

data NovaConfig = NovaConfig
                { certificateFile       :: FilePath -- TLS runner checks if this file exists
                , keyFile               :: FilePath -- TLS runner checks if this file exists
                , port                  :: Int      -- Port won't bind if it's busy
                , endpoint              :: Maybe String
                , database              :: Database
                , logLevel              :: Priority
                , serverType            :: ServerType
                }

defaultConfig :: NovaConfig
defaultConfig = NovaConfig
              { certificateFile       = "server.crt"
              , keyFile               = "server.key"
              , port                  = defaultPort
              , endpoint              = Nothing
              , database              = Database
                                        { dbHost = "localhost"
                                        , dbPort = 27017
                                        , dbName = "nova"
                                        }
              , logLevel              = NOTICE
              , serverType            = Plain
              }
  where defaultPort = 8774

instance Default NovaConfig where
  def = defaultConfig

instance BaseConfig NovaConfig where
  getEndpoint = endpoint
  getServerType = serverType

confFileName :: String
confFileName = "nova.conf"

$(deriveJSON defaultOptions ''NovaConfig)
