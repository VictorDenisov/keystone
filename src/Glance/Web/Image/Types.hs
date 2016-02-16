{-# LANGUAGE DeriveDataTypeable #-}
module Glance.Web.Image.Types
where

import Data.Data (Typeable)

data ImageCreateRequest = ImageCreateRequest
                        { name            :: String
                        , visibility      :: Maybe Bool
                        , tags            :: Maybe [String]
                        , containerFormat :: String
                        , diskFormat      :: String
                        , minDisk         :: Maybe Int
                        , minRam          :: Maybe Int
                        , protected       :: Maybe Bool
                        } deriving (Show, Read, Eq, Ord, Typeable)
