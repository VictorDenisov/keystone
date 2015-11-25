{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Backend where

import Data.Bson (ObjectId)
import Model.Common (OpStatus(..))

import qualified Database.MongoDB as M
import qualified Error as E
import qualified Model.User as MU

class BackendApi b where
  type BackendHandle b
  createUser :: MU.User -> b (Either E.Error ObjectId)
  findUserById :: ObjectId
               -> b (Maybe MU.User)
  listUsers :: (Maybe String) -> b [MU.User]
  updateUser :: ObjectId -> M.Document -> b (Maybe MU.User)
  deleteUser :: ObjectId -> b OpStatus
  withHandle :: (BackendHandle b -> b a) -> b a
