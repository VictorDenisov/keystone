{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Config
where

import Control.Exception (catch, SomeException)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Data.Yaml ( decodeFileEither, YamlException(..), ParseException(..)
                 , YamlMark(..))
import Language.Haskell.TH.Syntax (nameBase)
import System.Log.Logger (Priority(NOTICE))
import Text.Read (readMaybe)

import qualified Data.Text as T

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

data Database = Database
              { dbHost :: String
              , dbPort :: Int
              }

data ServerType = Tls
                | Plain
                  deriving (Read, Show)

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

readConfig :: IO KeystoneConfig
readConfig = do
  mConf <- catch (decodeFileEither confFileName)
               $ \(e::SomeException) -> do
                        putStrLn $ "WARNING!!! Failed to parse config file. Using default values."
                        putStrLn $ show e
                        return $ Right defaultConfig
  case mConf of
    Right conf -> return conf
    Left (InvalidYaml Nothing) -> do
      fail $ "Failed to parse existing config file. Please verify the syntax and mandatory values."
    Left (InvalidYaml (Just (YamlException message))) -> do
      fail $ "Failed to parse existing config file: " ++ message
    Left (InvalidYaml (Just (YamlParseException problem context mark))) -> do
      fail $ "Failed to parse existing config file: "
                                                ++ problem ++ " "
                                                ++ context ++ " "
                                                ++ " - line "
                                                ++ (show $ yamlLine mark)
                                                ++ ", col "
                                                ++ (show $ yamlColumn mark)
    Left (AesonException message) -> do
      fail $ "Failed to parse existing config file: " ++ message
    Left e -> do
      fail $ "Failed to parse existing config file: " ++ (show e)

instance ToJSON ServerType where
  toJSON t = String $ T.pack $ show t

instance FromJSON ServerType where
  parseJSON (String v) = case readMaybe $ T.unpack v of
                           Just v  -> return v
                           Nothing -> fail "Failed to parse ServerType"
  parseJSON v = typeMismatch (nameBase ''ServerType) v

instance ToJSON Priority where
  toJSON p = String $ T.pack $ show p

instance FromJSON Priority where
  parseJSON (String v) = case readMaybe $ T.unpack v of
                           Just v  -> return v
                           Nothing -> fail "Failed to parse Priority"
  parseJSON v = typeMismatch (nameBase ''Priority) v

$(deriveJSON defaultOptions ''KeystoneConfig)

$(deriveJSON defaultOptions ''LdapConfig)

$(deriveJSON defaultOptions ''Database)
