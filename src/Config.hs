{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Config
where

import Control.Exception (catch, SomeException)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Data.Default (Default(..))
import Data.Yaml ( decodeFileEither, YamlException(..), ParseException(..)
                 , YamlMark(..))
import Language.Haskell.TH.Syntax (nameBase)
import System.Log.Logger (Priority)
import Text.Read (readMaybe)

import qualified Data.Text as T

class BaseConfig a where
  getEndpoint :: a -> Maybe String
  getServerType :: a -> ServerType

data Database = Database
              { dbHost :: String
              , dbPort :: Int
              , dbName :: String
              }

data ServerType = Tls
                | Plain
                  deriving (Read, Show)

readConfig :: (Default a, FromJSON a) => String -> IO a
readConfig filename = do
  mConf <- catch (decodeFileEither filename)
               $ \(e::SomeException) -> do
                        putStrLn $ "WARNING!!! Failed to parse config file. Using default values."
                        putStrLn $ show e
                        return $ Right def
  case mConf of
    Right conf -> return conf
    Left (InvalidYaml Nothing) -> do
      fail $ "Failed to parse existing config file. Please verify the syntax and mandatory values."
    Left (InvalidYaml (Just (YamlException message))) -> do
      fail $ "Failed to parse existing config file: " ++ message
    Left (InvalidYaml (Just (YamlParseException problem context mark))) -> do
      fail $ "Failed to parse existing config file: "
                                                ++ problem ++ " "
                                                ++ context ++ " "
                                                ++ " - line "
                                                ++ (show $ yamlLine mark)
                                                ++ ", col "
                                                ++ (show $ yamlColumn mark)
    Left (AesonException message) -> do
      fail $ "Failed to parse existing config file: " ++ message
    Left e -> do
      fail $ "Failed to parse existing config file: " ++ (show e)

instance ToJSON ServerType where
  toJSON t = String $ T.pack $ show t

instance FromJSON ServerType where
  parseJSON (String v) = case readMaybe $ T.unpack v of
                           Just v  -> return v
                           Nothing -> fail "Failed to parse ServerType"
  parseJSON v = typeMismatch (nameBase ''ServerType) v

instance ToJSON Priority where
  toJSON p = String $ T.pack $ show p

instance FromJSON Priority where
  parseJSON (String v) = case readMaybe $ T.unpack v of
                           Just v  -> return v
                           Nothing -> fail "Failed to parse Priority"
  parseJSON v = typeMismatch (nameBase ''Priority) v

$(deriveJSON defaultOptions ''Database)
