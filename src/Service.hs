{-# Language DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Service
( module Service
, module Service.Types
) where

import Common (skipTickOptions, dropOptions)
import Service.Types (ServiceCreateRequest(..), ServiceUpdateRequest(..))
import Control.Applicative ((<*>), (<$>))
import Data.Aeson ( FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.TH (mkParseJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Data.Data (Typeable)
import Data.Text (pack, unpack)
import Language.Haskell.TH.Syntax (nameBase)

import qualified Database.MongoDB as M
import qualified Model.Service as MS

newRequestToService :: ServiceCreateRequest -> MS.Service
newRequestToService ServiceCreateRequest{..} =
  MS.Service description (maybe True id enabled) name type'

updateRequestToDocument :: ServiceUpdateRequest -> M.Document
updateRequestToDocument ServiceUpdateRequest{..} = concat
  [ (pack $ nameBase 'MS.description) M.=? udescription
  , (pack $ nameBase 'MS.enabled)     M.=? uenabled
  , (pack $ nameBase 'MS.name)        M.=? uname
  , (pack $ nameBase 'MS.type')       M.=? utype
  ]

instance FromJSON ServiceCreateRequest where
  parseJSON (Object v) = do
    service <- v .: "service"
    parseScr service
  parseJSON v = typeMismatch (nameBase ''ServiceCreateRequest) v

instance FromJSON ServiceUpdateRequest where
  parseJSON (Object v) = do
    service <- v .: "service"
    parseSur service
  parseJSON v = typeMismatch (nameBase ''ServiceUpdateRequest) v

parseScr = $(mkParseJSON skipTickOptions ''ServiceCreateRequest)

parseSur = $(mkParseJSON (dropOptions 1) ''ServiceUpdateRequest)
