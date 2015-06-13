{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
module Common.Database
where

import Common (loggerName)
import Config (Database(..))
import Control.Applicative (Applicative)
import Control.Exception (bracket)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT, allocate, release, MonadResource, MonadBaseControl, MonadThrow)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Language.Haskell.TH.Syntax (nameBase)
import System.IO.Error ( catchIOError, ioError, userError
                       , ioeGetErrorType, ioeGetLocation, ioeGetErrorString)
import System.Log.Logger (errorM, infoM)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

dbName :: Text
dbName = "keystone"

connect :: Database -> IO M.Pipe
connect dbConf = do
  infoM loggerName "Connecting to the database"
  catchIOError (M.connect $ M.Host host $ M.PortNumber $ fromIntegral port) $ \e -> do
    fail $ "Can't connect to the database: " ++ (ioeGetErrorString e)
  where
    host = dbHost dbConf
    port = dbPort dbConf

withDB :: Database -> M.Action IO a -> IO a
withDB dbConf f = runResourceT $ do
  (releaseKey, pipe) <- allocate (connect dbConf) M.close
  v <- lift $ runDB pipe f
  release releaseKey
  return v
  where
    host = dbHost dbConf
    port = dbPort dbConf

runDB :: MonadIO m => M.Pipe -> M.Action m a -> m a
runDB p f = M.access p M.master dbName f

affectedDocs :: MonadIO m => M.Action m Int
affectedDocs = do
  le <- M.runCommand ["getLastError" M.=: (M.Int32 1)]
  mn <- M.look "n" le
  case mn of
    (M.Int32 n) -> return $ fromIntegral n
    _           -> fail "Mongodb returned non integer n from getLastError commmand"

instance FromJSON M.ObjectId where
  parseJSON (String s) = case readMaybe $ T.unpack s of
                          Just v -> return v
                          Nothing -> fail $ "Invalid object id - " ++ (T.unpack s)
  parseJSON v = typeMismatch (nameBase ''M.ObjectId) v

instance ToJSON M.ObjectId where
  toJSON v = String $ T.pack $ show v

-- Common fields that are used around
idF = "_id"
idF :: Text

-- Common database commands that are user around.
currentDateC = "$currentDate"
decC         = "$dec"
inC          = "$in"
incC         = "$inc"
matchC       = "$match"
neC          = "$ne"
projectC     = "$project"
pullC        = "$pull"
pushC        = "$push"
setC         = "$set"
unwindC      = "$unwind"

currentDateC :: Text
decC         :: Text
inC          :: Text
incC         :: Text
matchC       :: Text
neC          :: Text
projectC     :: Text
pullC        :: Text
pushC        :: Text
setC         :: Text
unwindC      :: Text

(+.+) :: Text -> Text -> Text
a +.+ b = a `T.append` "." `T.append` b

(+++) :: Text -> Text -> Text
a +++ b = a `T.append` b
