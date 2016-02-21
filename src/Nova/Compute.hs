module Nova.Compute
where

import Network.Socket (Socket)

data ComputeAgent = ComputeAgent
                  { socket :: Socket
                  , name   :: String
                  } deriving Show

handshake :: Socket -> IO ComputeAgent
handshake s = return $ ComputeAgent s "test"
