{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Nova.Compute
where

import Common (underscoreOptions, loggerName)
import Data.Aeson (decode)
import Data.Aeson.TH (deriveJSON)
import Data.Binary.Get (runGet, getWord32be)
import Data.Data (Typeable)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import System.Log.Logger (debugM, errorM)

import qualified Data.ByteString.Lazy as BSN

data ComputeAgent = ComputeAgent
                  { socket :: Socket
                  , agentName :: String
                  } deriving (Show, Eq)

data Message = HelloMessage
               { name :: String
               }
             | StartInstance
               { instanceName :: String
               }
               deriving (Show, Read, Eq, Typeable, Ord)

$(deriveJSON underscoreOptions ''Message)

handshake :: Socket -> IO (Maybe ComputeAgent)
handshake s = do
  m <- readMessage s
  case m of
    Nothing -> return Nothing
    Just v  -> return $ Just $ ComputeAgent s $ name v

readMessage :: Socket -> IO (Maybe Message)
readMessage s = do
  ls <- recvFixedLen s 4 ""
  let len = runGet getWord32be ls
  debugM loggerName $ "Received len: " ++ (show len)
  messageString <- recvFixedLen s (fromIntegral len) ""
  debugM loggerName $ "Received message string: " ++ (show messageString)
  case decode messageString of
    Nothing -> do
      errorM loggerName $ "Unknown message from compute node: " ++ (show messageString)
      return Nothing
    Just ms -> return $ Just ms

recvFixedLen :: Socket -> Int -> BSN.ByteString -> IO BSN.ByteString
recvFixedLen s len lastString = do
  res <- BSN.fromStrict <$> recv s len
  let fullString = (lastString `BSN.append` res)
  if (BSN.length fullString < (fromIntegral len)) && (BSN.length res > 0)
    then
      recvFixedLen s (len - (fromIntegral $ BSN.length res)) fullString
    else
      return fullString
