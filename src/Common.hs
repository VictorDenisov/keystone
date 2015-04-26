{-# LANGUAGE OverloadedStrings #-}
module Common
where

import Data.Aeson.TH (defaultOptions, Options(..))
import Data.Bson (ObjectId(..))
import Data.Char (toUpper)
import Data.Maybe (maybe)

import Text.Read (readMaybe)

import Web.Scotty (Parsable(..))

import qualified Data.Text.Lazy as T
import qualified Error as E
import qualified Web.Scotty.Trans as S

databaseVersion :: String
databaseVersion = "0.0.1"

loggerName :: String
loggerName = "Main"

type ScottyM = S.ScottyT E.Error IO
type ActionM = S.ActionT E.Error IO

skipTickOptions = defaultOptions { fieldLabelModifier = filter (/= '\'') }

dropOptions size = defaultOptions { fieldLabelModifier = drop size }

capitalize :: String -> String
capitalize "" = ""
capitalize s = (toUpper $ head s) : tail s

instance Parsable ObjectId where
  parseParam t = maybe
                  (Left "Failed to parse object id from argument")
                  Right
                  $ readMaybe $ T.unpack t
