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
                    }

$(deriveJSON defaultOptions ''KeystoneConfig)


readConfig :: IO KeystoneConfig
readConfig = do
  mConf <- decodeFile "keystone.yml"
  case mConf of
    Just conf -> return conf
    Nothing   -> return $ KeystoneConfig
                               { adminToken      = "ADMIN_TOKEN"
                               , certificateFile = "server.crt"
                               , keyFile         = "server.key"
                               }
