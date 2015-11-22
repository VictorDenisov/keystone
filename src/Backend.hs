{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Backend where

import Data.Bson (ObjectId)

import qualified Auth.Types as A
import qualified Model.Token as MT
import qualified Model.User as MU

class BackendApi b where
  type BackendHandle b
  authenticate :: (Maybe A.AuthScope)
               -> A.AuthMethod
               -> b (Either String (String, MT.Token))
  findUserById :: ObjectId
               -> b (Maybe MU.User)
  withHandle :: (BackendHandle b -> b a) -> b a
