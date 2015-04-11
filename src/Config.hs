{-# Language TemplateHaskell #-}
module Config
where

import Control.Monad (MonadPlus(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Aeson.TH ( deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Yaml (decodeFile)
import System.Log.Logger (Priority(..))
import Text.Read (readMaybe)

import qualified Data.Text as T

data KeystoneConfig = KeystoneConfig
                    { adminToken      :: String
                    , certificateFile :: String -- TLS runner checks if this file exists
                    , keyFile         :: String -- TLS runner checks if this file exists
                    , port            :: Int    -- Port won't bind if it's busy
                    , endpoint        :: Maybe String
                    , database        :: Database -- TODO Will be verified during creation of the pool
                    , logLevel        :: Priority
                    }

data Database = Database
              { dbHost :: String
              , dbPort :: Int
              }

instance ToJSON Priority where
  toJSON p = String $ T.pack $ show p

instance FromJSON Priority where
  parseJSON (String v) = maybe mzero return $ readMaybe $ T.unpack v

$(deriveJSON defaultOptions ''KeystoneConfig)

$(deriveJSON defaultOptions ''Database)

readConfig :: IO KeystoneConfig
readConfig = do
  mConf <- decodeFile "keystone.yml"
  case mConf of
    Just conf ->
      return conf
    Nothing   -> do
      putStrLn $ "Failed to parse config file. Using default values."
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
                   }
  where defaultPort = 35357
