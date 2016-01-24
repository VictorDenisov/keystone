{-# LANGUAGE TemplateHaskell #-}
module Keystone.Config
where

import Config (Database(..), ServerType(), ServerType(..), BaseConfig(..))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Default (Default(..))
import System.Log.Logger (Priority(NOTICE))

data KeystoneConfig = KeystoneConfig
                    { adminToken            :: String
                    , certificateFile       :: FilePath -- TLS runner checks if this file exists
                    , keyFile               :: FilePath -- TLS runner checks if this file exists
                    , port                  :: Int      -- Port won't bind if it's busy
                    , endpoint              :: Maybe String
                    , database              :: Database
                    , logLevel              :: Priority
                    , serverType            :: ServerType
                    , verifyTokenCollection :: Bool
                    , ldap                  :: Maybe LdapConfig
                    }

data LdapConfig = LdapConfig
                { ldapHost             :: String
                , userDn               :: String
                , password             :: String
                , userTreeDn           :: String
                , userFilter           :: String
                , userObjectClass      :: String
                , userIdAttribute      :: String
                , userNameAttribute    :: String
                , userMailAttribute    :: String
                , userPassAttribute    :: String
                , userEnabledAttribute :: String
                }

confFileName :: String
confFileName = "keystone.conf"

defaultConfig :: KeystoneConfig
defaultConfig =
  KeystoneConfig
    { adminToken            = "ADMIN_TOKEN"
    , certificateFile       = "server.crt"
    , keyFile               = "server.key"
    , port                  = defaultPort
    , endpoint              = Nothing
    , database              = Database
                              { dbHost = "localhost"
                              , dbPort = 27017
                              }
    , logLevel              = NOTICE
    , serverType            = Plain
    , verifyTokenCollection = True
    , ldap                  = Nothing
    }
  where defaultPort = 35357

instance Default KeystoneConfig where
  def = defaultConfig

instance BaseConfig KeystoneConfig where
  getEndpoint = endpoint
  getServerType = serverType

$(deriveJSON defaultOptions ''KeystoneConfig)

$(deriveJSON defaultOptions ''LdapConfig)
