{-# LANGUAGE OverloadedStrings #-}
module Auth
where
import Control.Applicative ((<*>), (<$>))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.PasswordStore (verifyPassword)
import Data.Aeson (FromJSON(..), (.:), Value(..))
import Data.ByteString.Char8 (pack)

import qualified Database.MongoDB as M
import qualified Model.User as MU

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

authenticate :: (MonadIO m) => M.Pipe -> AuthMethod -> m Bool
authenticate pipe am = do
    mu <- M.access pipe M.master "keystone" (MU.findUserById $ userId am)
    case mu of
      Nothing -> return $ False
      Just u  ->
        case MU.password u of
          Just p -> return $ verifyPassword (pack $ password am) (pack p)
          Nothing -> return $ False
