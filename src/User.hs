{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module User
( module User
, module User.Types
) where

import Common (fromObject, underscoreOptions, dropOptions, (<.>), UrlBasedValue, UrlInfo(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.PasswordStore (makePassword)
import Data.Aeson (FromJSON(..), (.:), Value(..), ToJSON(..))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (typeMismatch, (.=), object)
import Data.HashMap.Strict (insert)
import Data.Text (pack)
import Data.Vector (fromList)
import Language.Haskell.TH.Syntax (nameBase)

import User.Types ( ChangePasswordRequest(..)
                  , UserCreateRequest(..)
                  , UserUpdateRequest(..))

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

changePasswordRequestToDocument :: ChangePasswordRequest -> IO M.Document
changePasswordRequestToDocument ChangePasswordRequest{..} = do
  cryptedPassword <- runMaybeT $ do
    p <- return ppassword
    p1 <- liftIO $ makePassword (BS8.pack p) 17
    return $ BS8.unpack p1
  return $ concat [ (pack $ nameBase 'MU.password) M.=? cryptedPassword ]

produceUserJson :: MU.User -> String -> Value
produceUserJson (u@MU.User{..}) baseUrl
  = Object
    $ insert "links" (object [ "self" .= (baseUrl ++ "/v3/users/" ++ (show _id)) ])
    $ fromObject $ toJSON u

produceUserReply :: MU.User -> UrlBasedValue
produceUserReply (user@MU.User{..}) (UrlInfo {baseUrl})
  = object [ "user" .= produceUserJson user baseUrl ]

produceUsersReply :: [MU.User] -> UrlBasedValue
produceUsersReply users (UrlInfo {baseUrl, path, query})
  = object [ "links" .= (object [ "next"     .= Null
                                , "previous" .= Null
                                , "self"     .= (baseUrl ++ path ++ query)
                                ]
                        )
           , "users" .= usersEntry
           ]
  where
    usersEntry = Array $ fromList $ map (\f -> f baseUrl) $ map produceUserJson users

instance FromJSON ChangePasswordRequest where
  parseJSON (Object v) = do
    user <- v .: "user"
    parseCpr user
  parseJSON v = typeMismatch (nameBase ''ChangePasswordRequest) v

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

parseCpr = $(mkParseJSON (dropOptions 1 <.> underscoreOptions) ''ChangePasswordRequest)
