{-# Language TemplateHaskell #-}
module Config
where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Yaml (decodeFile)

data KeystoneConfig = KeystoneConfig
                    { adminToken      :: String
                    , certificateFile :: String
                    , keyFile         :: String
                    , port            :: Int
                    , endpoint        :: String
                    }

$(deriveJSON defaultOptions ''KeystoneConfig)


readConfig :: IO KeystoneConfig
readConfig = do
  mConf <- decodeFile "keystone.yml"
  case mConf of
    Just conf -> return conf
    Nothing   -> do
      putStrLn $ "Failed to parse config file. Using default values."
      return $ KeystoneConfig
                   { adminToken      = "ADMIN_TOKEN"
                   , certificateFile = "server.crt"
                   , keyFile         = "server.key"
                   , port            = defaultPort
                   , endpoint        = "https://localhost:" ++ (show defaultPort)
                   }
  where defaultPort = 35357
