{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Common (loggerName)
import Config (readConfig, ServerType(..))
import Control.Exception (catch, SomeException)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, writeChan, newChan, readChan)
import Control.Concurrent.MVar (newMVar, modifyMVar_, withMVar)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (delete)
import Data.Time.Clock (getCurrentTime)
import Error (MsgError(..))
import Network.Socket ( getAddrInfo, addrAddress, addrFamily, withSocketsDo
                      , defaultProtocol, bindSocket, listen, accept
                      , AddrInfo(..), AddrInfoFlag(AI_PASSIVE), socket
                      , defaultHints, SocketType(Stream), Socket, close)
import Network.HTTP.Types.Status (statusCode, status500)
import Network.Wai (Middleware, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings)
import Network.Wai.Handler.WarpTLS (tlsSettings, runTLS)
import Nova.Config (confFileName, NovaConfig(..))
import Nova.Web.Version (listVersionsH)
import System.IO (stdout)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger ( setLevel, updateGlobalLogger, setHandlers
                         , removeAllHandlers, debugM, noticeM, errorM)
import System.Timeout (timeout)
import Web.Common (ScottyM)

import qualified Error as E
import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S
import qualified Nova.Compute as NC

main = do
  (config :: NovaConfig) <- readConfig confFileName
  let logFormatter = simpleLogFormatter "$utcTime (pid $pid, $tid) $prio: $msg"
  stdoutHandler <- streamHandler stdout (logLevel config)
  fileHandler <- fileHandler "nova.log" (logLevel config)
  removeAllHandlers
  updateGlobalLogger loggerName $ setLevel (logLevel config) . setHandlers
    [ setFormatter stdoutHandler logFormatter
    , setFormatter fileHandler   logFormatter
    ]

  -- Add verify database

  (messageChannel :: Chan NC.Message) <- newChan

  forkIO $ computeServer messageChannel

  !policy <- A.loadPolicy
  -- ^ bang pattern is because we want to know if the policy is correct now
  -- ^ we need the evaluation to happen immediatelly
  verifyDatabase config

  app <- S.scottyAppT id (application policy config)

  let settings = tlsSettings
                      (certificateFile config)
                      (keyFile config)

  let serverSettings = setPort (port config) defaultSettings
  noticeM loggerName $ "Starting web server @ port " ++ (show $ port config)
  case serverType config of
    Tls   -> runTLS settings serverSettings app
    Plain -> runSettings serverSettings app

computeServer :: Chan NC.Message -> IO ()
computeServer messageChannel = withSocketsDo $ do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing
               (Just "13000")
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  bindSocket sock (addrAddress serveraddr)

  agentList <- newMVar []

  listen sock 5

  forkIO $ messagePrinter messageChannel

  forever $ do
    (clientSocket, clientAddr) <- accept sock
    mAgent <- NC.handshake clientSocket
    case mAgent of
      Just agent -> do
        void $ forkIO $ do
          modifyMVar_ agentList $ \list -> return $ agent : list
          withMVar agentList $ putStrLn . show
          (threadReader (NC.socket agent) messageChannel)
           `catch` (
               \(e :: SomeException) ->
                  errorM loggerName $ "Caught exception in thread reader: " ++ (show e)
           )
          close $ NC.socket agent
          modifyMVar_ agentList $ \list -> return $ delete agent list
          withMVar agentList $ putStrLn . show
      Nothing -> do
        errorM loggerName $ "Failed to handshake with remote client: " ++ (show clientAddr)


messagePrinter :: Chan NC.Message -> IO ()
messagePrinter chan = do
  forever $ do
    m <- readChan chan
    debugM loggerName $ show m

threadReader :: Socket -> Chan NC.Message -> IO ()
threadReader sock channel = do
  m <- timeout (2 * (fromIntegral NC.heartBeatTimeout) * 1000000) $ NC.readMessage sock
  case m of
    Nothing ->
      errorM loggerName $ "Agent didn't respond within specified time."
    Just (Left ParseFailure) ->
      errorM loggerName $ "Failed to read message."
    Just (Left EndOfStream) ->
      return ()
    Just (Right v)  -> do
      writeChan channel v
      threadReader sock channel

application :: AT.Policy
            -> NovaConfig
            -> ScottyM IO ()
application policy config = do
  S.middleware exceptionCatchMiddleware
  S.middleware logRequestResponse
  S.defaultHandler $ \e -> do
    S.status $ E.code e
    case statusCode $ E.code e of
      500 -> do
        time <- liftIO $ getCurrentTime
        liftIO $ errorM loggerName $ E.message e
        S.json $ e {E.message = "Internal error. Server time - " ++ (show time)}
      _ -> do
        S.json e
  S.get   "/"                                   $ listVersionsH                config

verifyDatabase :: NovaConfig -> IO ()
verifyDatabase NovaConfig{..} = return () -- TODO

exceptionCatchMiddleware :: Middleware
exceptionCatchMiddleware app request responder = do
  (app request responder)
    `catch`
      (\(e :: SomeException) -> do
        time <- liftIO $ getCurrentTime
        errorM loggerName $ show e
        let message = "Internal error. Server time - " ++ (show time) ++ "\n"
        responder $ responseLBS status500 [] (pack message)
      )

logRequestResponse :: Middleware
logRequestResponse app request responder = do
  debugM loggerName $ show request
  app request responder
