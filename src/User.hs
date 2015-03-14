{-# LANGUAGE OverloadedStrings #-}
module User
where

import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))

data UserCreateRequest = UserCreateRequest
                       { projectId :: String
                       , description :: String
                       , domainId :: Maybe String
                       , email :: String
                       , enabled :: Bool
                       , name :: String
                       , password :: Maybe String
                       } deriving Show

instance FromJSON UserCreateRequest where
  parseJSON (Object v) = do
    user <- v .: "user"
    UserCreateRequest
        <$> (user .:  "default_project_id")
        <*> (user .:  "description")
        <*> (user .:? "domain_id")
        <*> (user .:  "email")
        <*> (user .:  "enabled")
        <*> (user .:  "name")
        <*> (user .:? "password")
  parseJSON _ = mzero
