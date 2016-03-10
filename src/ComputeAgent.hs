module Main
where

import Prelude hiding (length)

import Common (loggerName)
import Control.Monad (forever)
import Data.Aeson (encode)

import Data.Binary.Put (runPut, putWord32be)

import Data.ByteString.Lazy (toStrict, length)
import Error (MsgError(..))

import Network.Socket.ByteString (send)
import Network.Socket ( Socket, withSocketsDo, getAddrInfo, addrFamily, SocketType(Stream)
                      , defaultProtocol, connect, socket, addrAddress)

import System.Log.Logger (debugM, noticeM, errorM)
import qualified Nova.Compute as NC

writeMessage :: Socket -> NC.Message -> IO ()
writeMessage sock msg = do
  let str = encode msg
  let len = length str
  let lenStr = runPut $ putWord32be $ fromIntegral len
  send sock $ toStrict lenStr
  send sock $ toStrict str
  return ()

main :: IO ()
main = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "localhost") (Just "13000")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  putStrLn $ "Connected waiting to send message"
  l <- getLine
  writeMessage sock $ NC.HelloMessage "MyTestName"

  forever $ do
    m <- NC.readMessage sock
    case m of
      Left ParseFailure ->
        errorM loggerName $ "Failed to read message."
      Left EndOfStream ->
        fail "Can't read messages from the server anymore"
      Right v  -> do
        handleMessage v

handleMessage = undefined
