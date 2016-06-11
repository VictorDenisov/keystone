{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Keystone.Web.User
( module Keystone.Web.User
, module Keystone.Web.User.Types
) where

import Common (fromObject, underscoreOptions, dropOptions, (<.>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Crypto.PasswordStore (makePassword)
import Data.Aeson (FromJSON(..), (.:), Value(..), ToJSON(..))
import Data.Aeson.TH (mkParseJSON)
import Data.Aeson.Types (typeMismatch, (.=), object)
import Data.HashMap.Strict (insert)
import Data.Text (pack)
import Data.Vector (fromList)
import Keystone.Config (KeystoneConfig)
import Language.Haskell.TH.Syntax (nameBase)
import Model.Common (OpStatus(Success, NotFound))
import Keystone.Model.IdentityApi (IdentityApi(..))
import Network.HTTP.Types.Status (status200, status201, status204, status404)
import Web.Common ( UrlBasedValue, UrlInfo(..), withHostUrl, parseRequest
                  , parseId, parseMaybeString, ActionM)

import Keystone.Web.User.Types ( ChangePasswordRequest(..)
                               , UserCreateRequest(..)
                               , UserUpdateRequest(..)
                               )

import qualified Data.ByteString.Char8 as BS8
import qualified Database.MongoDB as M
import qualified Error as E
import qualified Keystone.Model.User as MU
import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S

createUserH :: (Functor m, MonadIO m, IdentityApi m)
           => AT.Policy -> KeystoneConfig -> ActionM m ()
createUserH policy config = A.requireToken config $ \token -> do
    (d :: UserCreateRequest) <- parseRequest
    user <- liftIO $ newRequestToUser d
    A.authorize policy AT.AddUser token AT.EmptyResource $ do
      mUid <- lift $ createUser user
      case mUid of
        Left err -> do
          S.json err
          S.status $ E.code err
        Right rid -> do
          S.status status201
          withHostUrl config $ produceUserReply user

listUsersH :: (Functor m, MonadIO m, IdentityApi m)
           => AT.Policy -> KeystoneConfig -> ActionM m ()
listUsersH policy config = A.requireToken config $ \token -> do
    userName <- parseMaybeString "name"
    A.authorize policy AT.ListUsers token AT.EmptyResource $ do
      users <- lift $ listUsers userName
      S.status status200
      withHostUrl config $ produceUsersReply users

userDetailsH :: (Functor m, MonadIO m, IdentityApi m)
             => AT.Policy -> KeystoneConfig -> ActionM m ()
userDetailsH policy config = A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.ShowUserDetails token (AT.UserId uid) $ do
      mUser <- lift $ findUserById uid
      case mUser of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "User not found"
        Just user -> do
          S.status status200
          withHostUrl config $ produceUserReply user

updateUserH :: (Functor m, MonadIO m, IdentityApi m)
            => AT.Policy -> KeystoneConfig -> ActionM m ()
updateUserH policy config = A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    (uur :: UserUpdateRequest) <- parseRequest
    A.authorize policy AT.UpdateUser token AT.EmptyResource $ do
      updateDocument <- liftIO $ updateRequestToDocument uur
      mUser <- lift $ updateUser uid updateDocument
      case mUser of
        Nothing -> do
          S.status status404
          S.json $ E.notFound "User not found"
        Just user -> do
          S.status status200
          withHostUrl config $ produceUserReply user

deleteUserH :: (Functor m, MonadIO m, IdentityApi m)
            => AT.Policy -> KeystoneConfig -> ActionM m ()
deleteUserH policy config = A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    A.authorize policy AT.DeleteUser token AT.EmptyResource $ do
      st <- lift $ deleteUser uid
      case st of
        Success  -> S.status status204
        NotFound -> do
          S.json $ E.notFound $ "Could not find user, " ++ (show uid) ++ "."
          S.status status404

updateUserPasswordH :: (Functor m, MonadIO m, IdentityApi m)
                    => AT.Policy -> KeystoneConfig -> ActionM m ()
updateUserPasswordH policy config = A.requireToken config $ \token -> do
    (uid :: M.ObjectId) <- parseId "uid"
    (cpr :: ChangePasswordRequest) <- parseRequest
    A.authorize policy AT.ChangePassword token (AT.UserId uid) $ do
      res <- lift $ checkUserPassword (Just uid) Nothing (poriginalPassword cpr)
      case res of
        Left errorMessage -> do
          S.status status404
          S.json $ E.notFound "User not found"
        Right _ -> do
          updateDocument <- liftIO $ changePasswordRequestToDocument cpr
          mModifiedUser <- lift $ updateUser uid updateDocument
          case mModifiedUser of
            Nothing -> do
              S.status status404
              S.json $ E.notFound "User not found"
            Just modifiedUser -> do
              S.status status200
              withHostUrl config $ produceUserReply modifiedUser

passwordStrength :: Int
passwordStrength = 10

newRequestToUser :: UserCreateRequest -> IO MU.User
newRequestToUser UserCreateRequest{..} = do
    cryptedPassword <- runMaybeT $ do
      p <- MaybeT $ return $ password
      p1 <- liftIO $ makePassword (BS8.pack p) passwordStrength
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
    p1 <- liftIO $ makePassword (BS8.pack p) passwordStrength
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
    p1 <- liftIO $ makePassword (BS8.pack p) passwordStrength
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
