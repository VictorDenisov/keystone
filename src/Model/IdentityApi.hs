{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Model.IdentityApi where

import Data.Bson (ObjectId)
import Model.Common (OpStatus(..))

import qualified Database.MongoDB as M
import qualified Error as E
import qualified Keystone.Model.User as MU

class IdentityApi b where
  type IdentityApiHandle b
  createUser :: MU.User -> b (Either E.Error ObjectId)
  findUserById :: ObjectId
               -> b (Maybe MU.User)
  listUsers :: (Maybe String) -> b [MU.User]
  updateUser :: ObjectId -> M.Document -> b (Maybe MU.User)
  deleteUser :: ObjectId -> b OpStatus
  withHandle :: (IdentityApiHandle b -> b a) -> b a
  checkUserPassword :: Maybe M.ObjectId -> Maybe String -> String -> b (Either String MU.User)
