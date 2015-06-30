{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module User
( module User
, module User.Types
) where

import Common (underscoreOptions, dropOptions, (<.>))
import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.PasswordStore (makePassword)
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.TH (mkParseJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Data.Text (pack)
import Language.Haskell.TH.Syntax (nameBase)

import User.Types (UserCreateRequest(..), UserUpdateRequest(..))

import qualified Data.ByteString.Char8 as BS8
import qualified Database.MongoDB as M
import qualified Model.User as MU

newRequestToUser :: UserCreateRequest -> IO MU.User
newRequestToUser UserCreateRequest{..} = do
    cryptedPassword <- runMaybeT $ do
      p <- MaybeT $ return $ password
      p1 <- liftIO $ makePassword (BS8.pack p) 17
      return $ BS8.unpack p1
    userId <- M.genObjectId
    return $ MU.User
                  userId
                  description
                  email
                  enabled
                  name
                  cryptedPassword

updateRequestToDocument :: UserUpdateRequest -> IO M.Document
updateRequestToDocument UserUpdateRequest{..} = do
  cryptedPassword <- runMaybeT $ do
    p <- MaybeT $ return upassword
    p1 <- liftIO $ makePassword (BS8.pack p) 17
    return $ BS8.unpack p1
  return $ concat
    [ (pack $ nameBase 'MU.description) M.=? udescription
    , (pack $ nameBase 'MU.email)       M.=? uemail
    , (pack $ nameBase 'MU.name)        M.=? uname
    , (pack $ nameBase 'MU.enabled)     M.=? uenabled
    , (pack $ nameBase 'MU.password)    M.=? cryptedPassword
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
