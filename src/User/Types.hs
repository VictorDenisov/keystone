{-# Language DeriveDataTypeable #-}

module User.Types
where

import Data.Data (Typeable)

import qualified Model.User as MU

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
