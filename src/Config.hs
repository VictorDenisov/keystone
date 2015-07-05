{-# Language TemplateHaskell #-}
module Config
where

import Common (loggerName)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Data.ByteString (ByteString)
import Data.Yaml (decodeFile)
import Language.Haskell.TH.Syntax (nameBase)
import System.IO (FilePath)
import System.Log.Logger (Priority(..), errorM)
import Text.Read (readMaybe)

import qualified Data.Text as T

data KeystoneConfig = KeystoneConfig
                    { adminToken      :: String
                    , certificateFile :: FilePath -- TLS runner checks if this file exists
                    , keyFile         :: FilePath -- TLS runner checks if this file exists
                    , port            :: Int      -- Port won't bind if it's busy
                    , endpoint        :: Maybe String
                    , database        :: Database
                    , logLevel        :: Priority
                    , serverType      :: ServerType
                    }

data Database = Database
              { dbHost :: String
              , dbPort :: Int
              }

data ServerType = Tls
                | Plain
                  deriving (Read, Show)

instance ToJSON ServerType where
  toJSON t = String $ T.pack $ show t

instance FromJSON ServerType where
  parseJSON (String v) = case readMaybe $ T.unpack v of
                           Just v  -> return v
                           Nothing -> (fail "Failed to parse ServerType")
  parseJSON v = typeMismatch (nameBase ''ServerType) v

instance ToJSON Priority where
  toJSON p = String $ T.pack $ show p

instance FromJSON Priority where
  parseJSON (String v) = case readMaybe $ T.unpack v of
                           Just v  -> return v
                           Nothing -> fail "Failed to parse Priority"
  parseJSON v = typeMismatch (nameBase ''Priority) v

$(deriveJSON defaultOptions ''KeystoneConfig)

$(deriveJSON defaultOptions ''Database)

confFileName :: String
confFileName = "keystone.conf"

readConfig :: IO KeystoneConfig
readConfig = do
  mConf <- decodeFile confFileName
  case mConf of
    Just conf ->
      return conf
    Nothing   -> do
      errorM loggerName $ "Failed to parse config file: " ++ confFileName ++ ". Using default values."
      return $ KeystoneConfig
                   { adminToken      = "ADMIN_TOKEN"
                   , certificateFile = "server.crt"
                   , keyFile         = "server.key"
                   , port            = defaultPort
                   , endpoint        = Nothing
                   , database        = Database
                                     { dbHost = "localhost"
                                     , dbPort = 27017
                                     }
                   , logLevel        = WARNING
                   , serverType      = Plain
                   }
  where defaultPort = 35357
