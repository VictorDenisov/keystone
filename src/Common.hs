{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Common
where

import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson (Value(..), Object)
import Data.Aeson.TH (defaultOptions, Options(..))
import Data.Bson (ObjectId(..))
import Data.Char (toUpper, toLower, isUpper)
import Data.List (intercalate)
import Data.List.Split (split, whenElt, keepDelimsL)

import Text.Read (readMaybe)

import Web.Scotty (Parsable(..))

import qualified Data.Text.Lazy as T
import qualified Error as E
import qualified Web.Scotty.Trans as S
import Web.Scotty.Internal.Types (ActionT(..))

databaseVersion :: String
databaseVersion = "0.0.1"

loggerName :: String
loggerName = "Main"

type ScottyM = S.ScottyT E.Error IO
type ActionM = ActionT E.Error IO

instance MonadThrow m => MonadThrow (ActionT E.Error m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (ActionT E.Error m) where
  catch (ActionT m) c = ActionT $ catch m (runAM . c)

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
