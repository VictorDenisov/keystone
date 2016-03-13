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
import Error (MsgError(..))
import GHC.Int (Int64)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import System.Log.Logger (debugM, errorM)

import qualified Data.ByteString.Lazy as BSN

heartBeatTimeout :: Int64
heartBeatTimeout = 10

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
             | AgentHeartBeat
               deriving (Show, Read, Eq, Typeable, Ord)

$(deriveJSON underscoreOptions ''Message)

handshake :: Socket -> IO (Maybe ComputeAgent)
handshake s = do
  m <- readMessage s
  case m of
    Left  _ -> return Nothing
    Right v -> return $ Just $ ComputeAgent s $ name v

readMessage :: Socket -> IO (Either MsgError Message)
readMessage s = do
  ls <- recvFixedLen s 4 ""
  if (BSN.length ls == 0)
    then
      return $ Left EndOfStream
    else do
      let len = runGet getWord32be ls
      debugM loggerName $ "Received len: " ++ (show len)
      messageString <- recvFixedLen s (fromIntegral len) ""
      debugM loggerName $ "Received message string: " ++ (show messageString)
      case decode messageString of
        Nothing -> do
          errorM loggerName $ "Unknown message from compute node: " ++ (show messageString)
          return $ Left ParseFailure
        Just ms -> return $ Right ms

recvFixedLen :: Socket -> Int -> BSN.ByteString -> IO BSN.ByteString
recvFixedLen s len lastString = do
  res <- BSN.fromStrict <$> recv s len
  let fullString = (lastString `BSN.append` res)
  if (BSN.length fullString < (fromIntegral len)) && (BSN.length res > 0)
    then
      recvFixedLen s (len - (fromIntegral $ BSN.length res)) fullString
    else
      return fullString
