{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
module Model.Service.Types
where

import Common.Database ( (+++) )
import Data.Data (Typeable)
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Data.Text as T

data Service = Service
             { _id         :: M.ObjectId
             , description :: Maybe String
             , enabled     :: Bool
             , name        :: Maybe String
             , type'       :: ServiceType
             , endpoints   :: [Endpoint]
             } deriving (Show, Read, Eq, Ord, Typeable)

endpointsF = T.pack $ nameBase 'endpoints
endpointsA = (M.String $ "$" +++ endpointsF) -- endpoints command for aggregation

data ServiceType = Identity
                 | Compute
                 | Image
                 | Volume
                 | Network
                   deriving (Show, Read, Eq, Ord, Typeable)

data Endpoint = Endpoint
              { einterface :: Interface
              , eurl       :: String
              , eenabled   :: Bool
              , eid        :: M.ObjectId
              } deriving (Show, Read, Eq, Ord, Typeable)

endpointFieldMod = drop 1

eidF = T.pack $ endpointFieldMod $ nameBase 'eid

data Interface = Admin
               | Internal
               | Public
                 deriving (Show, Read, Eq, Ord, Typeable)
