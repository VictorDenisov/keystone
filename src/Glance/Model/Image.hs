{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Glance.Model.Image
where

import Common (skipUnderscoreOptions, capitalize, underscoreOptions, (<.>))
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Model.Mongo.Common () -- Import ObjectId fromJson instance

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Bson ((=:))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Char (toLower)
import Data.Data (Typeable)
import Data.String (IsString(fromString))
import Data.Text (pack, unpack)

import Language.Haskell.TH.Syntax (nameBase)
import Text.Read (readMaybe)

import qualified Error as E
import qualified Database.MongoDB as M

collectionName :: M.Collection
collectionName = "image"

data Image = Image
           { _id             :: M.ObjectId
           , name            :: String
           , visibility      :: Visibility
           , status          :: Status
           , tags            :: [String]
           , containerFormat :: String
           , diskFormat      :: String
           , minDisk         :: Int
           , minRam          :: Int
           , protected       :: Bool
           } deriving (Show, Read, Eq, Ord, Typeable)

data Visibility = Public
                | Private
                  deriving (Show, Read, Eq, Ord, Typeable)

data Status = Queued
            | Saving
            | Active
            | Deactivated
            | Killed
            | Deleted
            | PendingDelete
              deriving (Show, Read, Eq, Ord, Typeable)

newtype ImageId = ImageId M.ObjectId
                  deriving (Show, Typeable, Eq)

instance M.Val Status where
  val s = M.String $ fromString $ show s
  cast' (M.String s) = readMaybe $ unpack s
  cast' _ = Nothing

instance FromJSON Status where
  parseJSON (String s) =
    case readMaybe $ capitalize $ unpack s of
      Just v -> return v
      Nothing -> fail $ "Unknown Status " ++ (unpack s)
  parseJSON v = typeMismatch (nameBase ''Status) v

instance ToJSON Status where
  toJSON v = String $ pack $ map toLower $ show v

instance M.Val Visibility where
  val s = M.String $ fromString $ show s
  cast' (M.String s) = readMaybe $ unpack s
  cast' _ = Nothing

instance FromJSON Visibility where
  parseJSON (String s) =
    case readMaybe $ capitalize $ unpack s of
      Just v -> return v
      Nothing -> fail $ "Unknown Visibility " ++ (unpack s)
  parseJSON v = typeMismatch (nameBase ''Visibility) v

instance ToJSON Visibility where
  toJSON v = String $ pack $ map toLower $ show v

instance M.Val ImageId where
  val (ImageId uid) = M.ObjId uid
  cast' (M.ObjId uid) = return $ ImageId uid
  cast' _ = Nothing

$(deriveBson id ''Image)

$(deriveJSON (underscoreOptions <.> skipUnderscoreOptions) ''Image)

listImages :: (Maybe String) -> M.Action IO [Image]
listImages mName = do
  let nameFilter = case mName of
                      Nothing -> []
                      Just nm -> [(pack $ nameBase 'name) =: (M.String $ pack nm)]
  cur <- M.find $ M.select nameFilter collectionName
  docs <- M.rest cur
  mapM fromBson docs

createImage :: Image -> M.Action IO (Either E.Error M.ObjectId)
createImage i =
  (do
    M.ObjId pid <- M.insert collectionName $ toBson i
    return $ Right pid
  ) `catch` (\f -> do
    case f of
      M.WriteFailure duplicateE message ->
          return $ Left $ E.conflict $ "Insert of project with the duplicate " ++ (nameBase 'name) ++ " is not allowed."
      _ -> throwM f
  )
