{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module User
( module User
, module User.Types
) where

import Common (underscoreOptions, dropOptions, (<.>))
import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.TH (mkParseJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Data.Text (pack)
import Language.Haskell.TH.Syntax (nameBase)

import User.Types (UserCreateRequest(..), UserUpdateRequest(..))

import qualified Database.MongoDB as M
import qualified Model.User as MU

updateRequestToDocument :: UserUpdateRequest -> M.Document
updateRequestToDocument UserUpdateRequest{..} = concat
  [ (pack $ nameBase 'MU.description) M.=? udescription
  , (pack $ nameBase 'MU.email)       M.=? uemail
  , (pack $ nameBase 'MU.name)        M.=? uname
  , (pack $ nameBase 'MU.enabled)     M.=? uenabled
  , (pack $ nameBase 'MU.password)    M.=? upassword
  ]

instance FromJSON UserCreateRequest where
  parseJSON (Object v) = do
    user <- v .: "user"
    parseUcr user
  parseJSON v = typeMismatch (nameBase ''UserCreateRequest) v

instance FromJSON UserUpdateRequest where
  parseJSON (Object v) = do
    user <- v .: "user"
    parseUur user
  parseJSON v = typeMismatch (nameBase ''UserUpdateRequest) v

parseUcr = $(mkParseJSON underscoreOptions ''UserCreateRequest)

parseUur = $(mkParseJSON (dropOptions 1 <.> underscoreOptions) ''UserUpdateRequest)
