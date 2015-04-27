{-# LANGUAGE OverloadedStrings #-}
module Common.Database
where

import Common (loggerName)
import Config (Database(..))
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import System.IO.Error ( catchIOError, ioError, userError
                       , ioeGetErrorType, ioeGetLocation, ioeGetErrorString)
import System.Log.Logger (errorM)

import qualified Database.MongoDB as M

dbName :: Text
dbName = "keystone"

verifyDatabase :: Database -> IO ()
verifyDatabase dbConf = return ()

connect :: MonadIO m => Database -> m M.Pipe
connect dbConf = liftIO $ do
  putStrLn "Connecting to the database"
  catchIOError (M.connect $ M.Host host $ M.PortNumber $ fromIntegral port) $ \e -> do
    fail $ "Can't connect to the database: " ++ (ioeGetErrorString e)
  where
    host = dbHost dbConf
    port = dbPort dbConf

withDB :: MonadIO m => Database -> M.Action IO a -> m a
withDB dbConf f = liftIO $ bracket (connect dbConf) (M.close) (\pipe -> runDB pipe f)

runDB :: MonadIO m => M.Pipe -> M.Action m a -> m a
runDB p f = M.access p M.master dbName f

affectedDocs :: MonadIO m => M.Action m Int
affectedDocs = do
  le <- M.runCommand ["getLastError" M.=: (M.Int32 1)]
  (M.Int32 n) <- M.look "n" le
  return $ fromIntegral n
