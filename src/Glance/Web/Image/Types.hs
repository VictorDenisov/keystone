{-# LANGUAGE DeriveDataTypeable #-}
module Glance.Web.Image.Types
where

import Data.Data (Typeable)
import Glance.Model.Image (Visibility)

data ImageCreateRequest = ImageCreateRequest
                        { name            :: String
                        , visibility      :: Maybe Visibility
                        , tags            :: Maybe [String]
                        , containerFormat :: String
                        , diskFormat      :: String
                        , minDisk         :: Maybe Int
                        , minRam          :: Maybe Int
                        , protected       :: Maybe Bool
                        } deriving (Show, Read, Eq, Ord, Typeable)
