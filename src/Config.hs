{-# Language TemplateHaskell #-}
module Config
where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Yaml (decodeFile)

data KeystoneConfig = KeystoneConfig
                    { adminToken :: String
                    }

$(deriveJSON defaultOptions ''KeystoneConfig)


readConfig :: IO (Maybe KeystoneConfig)
readConfig = do
  decodeFile "keystone.yml"
