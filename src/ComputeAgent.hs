module Main
where

import Prelude hiding (length)

import Common (loggerName)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.Timer (TimerIO, oneShotStart, newTimer)
import Control.Concurrent.Suspend.Lifted (sDelay)
import Data.Aeson (encode)

import Data.Binary.Put (runPut, putWord32be)

import Data.ByteString.Lazy (toStrict, length)
import Error (MsgError(..))

import Network.Socket.ByteString (send)
import Network.Socket ( Socket, withSocketsDo, getAddrInfo, addrFamily, SocketType(Stream)
                      , defaultProtocol, connect, socket, addrAddress)

import System.Log.Logger (debugM, noticeM, errorM)
import Control.Concurrent.Timer (TimerIO, oneShotStart, newTimer)
import qualified Nova.Compute as NC

heartBeatDelay = sDelay NC.heartBeatTimeout

writeMessage :: TimerIO -> Socket -> NC.Message -> IO ()
writeMessage timer sock msg = do
  let str = encode msg
  let len = length str
  let lenStr = runPut $ putWord32be $ fromIntegral len
  send sock $ toStrict lenStr
  send sock $ toStrict str
  forkIO $ do -- We need to rewind the timer in a separate thread, because if we do it in the same thread then oneShotStart will kill the thread that it's running in.
    putStrLn $ "Winding up timer"
    startRes <- oneShotStart timer (writeMessage timer sock NC.AgentHeartBeat) heartBeatDelay
    putStrLn $ "The result of starting timer is: " ++ (show startRes)
  return ()

main :: IO ()
main = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "localhost") (Just "13000")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  putStrLn $ "Connected waiting to send message"
  l <- getLine
  timer <- newTimer
  writeMessage timer sock $ NC.HelloMessage "MyTestName"

  forever $ do
    m <- NC.readMessage sock
    case m of
      Left ParseFailure ->
        errorM loggerName $ "Failed to read message."
      Left EndOfStream ->
        fail "Server dropped connection."
      Right v  -> do
        handleMessage v

handleMessage = undefined
