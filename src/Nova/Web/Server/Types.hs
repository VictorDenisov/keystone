{-# LANGUAGE DeriveDataTypeable #-}
module Nova.Web.Server.Types
where

import Data.Data (Typeable)

data ServerCreateRequest = ServerCreateRequest
                         { name      :: String
                         , imageRef  :: String
                         , flavorRef :: String
                         } deriving (Show, Read, Eq, Ord, Typeable)

