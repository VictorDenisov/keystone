{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Glance.Model.Image
where

import Common (skipUnderscoreOptions)
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Model.Mongo.Common () -- Import ObjectId fromJson instance

import Data.Aeson.TH (deriveJSON)
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Data (Typeable)

import Language.Haskell.TH.Syntax (nameBase)

import qualified Error as E
import qualified Database.MongoDB as M

collectionName :: M.Collection
collectionName = "image"

data Image = Image
           { _id             :: M.ObjectId
           , name            :: String
           --, visibility      :: Visibility
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

instance M.Val ImageId where
  val (ImageId uid) = M.ObjId uid
  cast' (M.ObjId uid) = return $ ImageId uid
  cast' _ = Nothing

$(deriveBson id ''Image)

$(deriveJSON skipUnderscoreOptions ''Image)

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
