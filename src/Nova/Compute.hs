{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Nova.Compute
where

import Common (underscoreOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Data (Typeable)
import Network.Socket (Socket)

data ComputeAgent = ComputeAgent
                  { socket :: Socket
                  , agentName :: String
                  } deriving (Show, Eq)


data Message = HelloMessage
             { name :: String
             } deriving (Show, Read, Eq, Typeable, Ord)

$(deriveJSON underscoreOptions ''Message)

handshake :: Socket -> IO ComputeAgent
handshake s = return $ ComputeAgent s "test"
