{-# LANGUAGE OverloadedStrings #-}
module Auth
where
import Control.Applicative ((<*>), (<$>))
import Control.Monad (forM)
import Data.Aeson (FromJSON(..), (.:), Value(..))

data AuthRequest = AuthRequest
                 { methods  :: [AuthMethod]
                 , scope    :: Maybe AuthScope
                 }

data AuthMethod = PasswordMethod
                { userId     :: String
                , password   :: String
                }

data AuthScope = AuthScope

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
