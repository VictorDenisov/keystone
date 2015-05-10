{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module User
( module User
, module User.Types
) where

import Common (underscoreOptions)
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.TH (mkParseJSON, defaultOptions)

import User.Types (UserCreateRequest(..))

instance FromJSON UserCreateRequest where
  parseJSON (Object v) = do
    user <- v .: "user"
    parseUcr user
  parseJSON _ = mzero

parseUcr = $(mkParseJSON underscoreOptions ''UserCreateRequest)

