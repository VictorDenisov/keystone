module Main
where

import Prelude hiding (length)

import Data.Aeson (encode)

import Data.Binary.Put (runPut, putWord32be)

import Data.ByteString.Lazy (toStrict, length)

import Network.Socket.ByteString (send)
import Network.Socket ( Socket, withSocketsDo, getAddrInfo, addrFamily, SocketType(Stream)
                      , defaultProtocol, connect, socket, addrAddress)

import Nova.Compute (Message(HelloMessage))

writeMessage :: Socket -> Message -> IO ()
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
  writeMessage sock $ HelloMessage "MyTestName"
  return ()
