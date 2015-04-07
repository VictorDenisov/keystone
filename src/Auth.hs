{-# LANGUAGE OverloadedStrings #-}
module Auth
where
import Control.Applicative ((<*>), (<$>))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.PasswordStore (verifyPassword)
import Data.Aeson (FromJSON(..), (.:), Value(..))
import Data.ByteString.Char8 (pack)
import Data.Time.Clock (getCurrentTime, addUTCTime)

import qualified Database.MongoDB as M
import qualified Model.User as MU
import qualified Model.Token as MT
import qualified Data.Text as T

data AuthRequest = AuthRequest
                 { methods  :: [AuthMethod]
                 , scope    :: Maybe AuthScope
                 } deriving Show

data AuthMethod = PasswordMethod
                { userId     :: String
                , password   :: String
                } deriving Show

data AuthScope = AuthScope
                 deriving Show

instance FromJSON AuthRequest where
  parseJSON (Object v) = do
    auth <- v .: "auth"
    identity <- auth .: "identity"
    mNames <- identity .: "methods"
    ms <- forM mNames $ \m -> do
      mDescr <- identity .: m
      case m of
        "password" -> do
          userSpec <- mDescr .: "user"
          PasswordMethod <$> (userSpec .: "id") <*> (userSpec .: "password")
    return $ AuthRequest ms Nothing

authenticate :: (MonadIO m) => M.Pipe -> AuthMethod -> m (Maybe (String, MT.Token))
authenticate pipe (PasswordMethod userId password) = do
    mu <- M.access pipe M.master "keystone" (MU.findUserById userId)
    case mu of
      Nothing -> return Nothing
      Just u  ->
        case MU.password u of
          Just p ->
            if verifyPassword (pack password) (pack p)
              then do
                currentTime <- liftIO getCurrentTime
                let token = MT.Token currentTime (addUTCTime (fromInteger $ 8 * 60 * 60) currentTime) userId
                mt <- M.access pipe M.master "keystone" (MT.createToken token)
                return $ Just (show mt, token)
              else return Nothing
          Nothing -> return Nothing
authenticate _ _ = return Nothing
