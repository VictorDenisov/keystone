{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Common
where

import Control.Monad.Catch (MonadThrow(..), Exception(..))
import Data.Aeson (Value(..), Object)
import Data.Aeson.TH (defaultOptions, Options(..))
import Data.Bson (ObjectId(..))
import Data.Char (toUpper, toLower, isUpper)
import Data.List (intercalate)
import Data.List.Split (split, whenElt, keepDelimsL)
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

instance MonadThrow m => MonadThrow (S.ActionT E.Error m) where
  throwM e = S.raise $ E.internalError $ show $ toException e

skipTickOptions = defaultOptions { fieldLabelModifier = filter (/= '\'') }

skipUnderscoreOptions = defaultOptions { fieldLabelModifier = filter (/= '_') }

dropOptions size = defaultOptions { fieldLabelModifier = drop size }

underscoreOptions = defaultOptions { fieldLabelModifier = camelToUnderscore}

(<.>) :: Options -> Options -> Options
xOptions <.> yOptions = yOptions { fieldLabelModifier = fieldLabelModifier xOptions . fieldLabelModifier yOptions }

camelToUnderscore "" = ""
camelToUnderscore s = map toLower $ (head s) : (intercalate "_" $ underscoreIt $ tail s)

underscoreIt = split $ keepDelimsL $ whenElt isUpper

capitalize :: String -> String
capitalize "" = ""
capitalize s = (toUpper $ head s) : tail s

fromObject :: Value -> Object
fromObject (Object o) = o

instance Parsable ObjectId where
  parseParam t = maybe
                  (Left "Failed to parse object id from argument")
                  Right
                  $ readMaybe $ T.unpack t
