{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Keystone.Model.User
where

import Common (skipUnderscoreOptions)
import Model.Mongo.Common () -- Import ObjectId fromJson instance

import Data.Aeson.TH (deriveJSON)
import Data.Bson.Mapping (deriveBson)
import Data.Data (Typeable)

import qualified Database.MongoDB as M

data User = User { _id :: M.ObjectId
                 , description :: Maybe String
                 , email :: Maybe String
                 , enabled :: Bool
                 , name :: String
                 , password :: Maybe String -- hash of the password
                 } deriving (Show, Read, Eq, Ord, Typeable)

newtype UserId = UserId M.ObjectId
                 deriving (Show, Typeable, Eq)

instance M.Val UserId where
  val (UserId uid) = M.ObjId uid
  cast' (M.ObjId uid) = return $ UserId uid
  cast' _ = Nothing

$(deriveBson id ''User)

$(deriveJSON skipUnderscoreOptions ''User)
