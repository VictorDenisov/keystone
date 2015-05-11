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
import Text.Read (readMaybe)

import qualified Common.Database as CD
import qualified Database.MongoDB as M
import qualified Model.User as MU
import qualified Model.Token as MT
import qualified Data.Text as T

data AuthRequest = AuthRequest
                 { methods  :: [AuthMethod]
                 , scope    :: Maybe AuthScope
                 } deriving Show

data AuthMethod = PasswordMethod
                { userId     :: M.ObjectId
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

authenticate :: (MonadIO m)
             => M.Pipe -> AuthMethod -> m (Either String (String, MT.Token))
authenticate pipe (PasswordMethod userId password) = do
    mu <- CD.runDB pipe $ MU.findUserById userId
    case mu of
      Nothing -> return $ Left "User is not found."
      Just u  ->
        case MU.password u of
          Just p ->
            if verifyPassword (pack password) (pack p)
              then do
                currentTime <- liftIO getCurrentTime
                let token = MT.Token currentTime (addUTCTime (fromInteger $ 8 * 60 * 60) currentTime) userId
                mt <- CD.runDB pipe $ MT.createToken token
                return $ Right (show mt, token)
              else return $ Left "Passwords don't match."
          Nothing -> return $ Left "User exists, but doesn't have any password."
authenticate _ _ = return $ Left "Method is not supported."
