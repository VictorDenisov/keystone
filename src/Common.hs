module Common
where

import Data.Aeson.TH (defaultOptions, Options(..))
import Data.Char (toUpper)

import qualified Error as E
import qualified Web.Scotty.Trans as S

databaseVersion :: String
databaseVersion = "0.0.1"

loggerName :: String
loggerName = "Main"

type ScottyM = S.ScottyT E.Error IO
type ActionM = S.ActionT E.Error IO

aesonOptions = defaultOptions { fieldLabelModifier = filter (/= '\'') }

capitalize :: String -> String
capitalize "" = ""
capitalize s = (toUpper $ head s) : tail s
