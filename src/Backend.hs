{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Backend where

import qualified Auth.Types as A
import qualified Model.Token as MT

class BackendApi b where
  type BackendHandle b
  connect :: b (BackendHandle b)
  authenticate :: BackendHandle b
               -> (Maybe A.AuthScope)
               -> A.AuthMethod
               -> b (Either String (String, MT.Token))
  close :: BackendHandle b -> b ()
