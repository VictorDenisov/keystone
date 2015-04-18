{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TemplateHaskell #-}
module Model.Service
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types (object, (.=), Value(..), typeMismatch)
import Data.Bson (Val(..))
import Data.Bson.Mapping (Bson(..), deriveBson)
import Data.Char (toLower)
import Data.Data (Typeable)
import Text.Read (readMaybe)

import qualified Database.MongoDB as M
import qualified Data.Text as T

collectionName :: M.Collection
collectionName = "service"

data Service = Service
             { description :: Maybe String
             , enabled     :: Bool
             , name        :: Maybe String
             , stype       :: ServiceType
             } deriving (Show, Read, Eq, Ord, Typeable)

data ServiceType = Identity
                 | Compute
                 | Image
                 | Volume
                 | Network
                   deriving (Show, Read, Eq, Ord, Typeable)

instance FromJSON ServiceType where
  parseJSON (String s) = case s of
    "identity" -> return Identity
    "compute"  -> return Compute
    "image"    -> return Image
    "volume"   -> return Volume
    "network"  -> return Network
    _          -> fail $ "Unknown ServiceType " ++ (T.unpack s)
  parseJSON v = typeMismatch "ServiceType" v

instance ToJSON ServiceType where
  toJSON v = String $ T.pack $ map toLower $ show v

instance Val ServiceType where
  val st = M.String $ T.pack $ show st
  cast' (M.String s) = readMaybe $ T.unpack s
  cast' _ = Nothing

$(deriveBson ''Service)

produceServiceReply :: Service -> M.ObjectId -> String -> Value
produceServiceReply (Service{..}) oid baseUrl
      = object [ "service"
                    .= (object [ "enabled" .= enabled
                               , "type"    .= stype
                               , "id"      .= (show oid)
                               , "links"   .= (object [ "self" .= (baseUrl ++ "/v3/services/" ++ (show oid)) ])]) ]

createService :: MonadIO m => Service -> M.Action m M.ObjectId
createService s = do
  M.ObjId oid <- M.insert collectionName $ toBson s
  return oid
