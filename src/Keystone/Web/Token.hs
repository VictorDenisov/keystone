{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Keystone.Web.Token
where

import Control.Monad (when, MonadPlus(mzero))
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Except (runExceptT, MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Resource (runResourceT, allocate, release)

import Data.Maybe (isNothing, fromJust)
import Data.Pool (Pool, withResource)
import Data.Time.Clock (getCurrentTime)

import Keystone.Config (KeystoneConfig(database))
import Keystone.Model.IdentityApi (IdentityApi)

import Network.HTTP.Types.Status (status200, status204, status401, status404)

import Text.Read (readMaybe)

import Web.Common (parseRequest, getBaseUrl, ActionM)

import qualified Data.Text.Lazy as TL
import qualified Error as E
import qualified Model.Mongo.Common as CD
import qualified Keystone.Model.Token as MT
import qualified Keystone.Web.Auth as A
import qualified Keystone.Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S

issueTokenH :: ( Functor m
               , MonadIO m
               , IdentityApi m
               , MonadThrow m
               , MonadBase IO m
               , MonadBaseControl IO m)
            => Pool CD.Connection -> KeystoneConfig -> ActionM m ()
issueTokenH mongoPool config = do
    (au :: AT.AuthRequest) <- parseRequest
    baseUrl <- getBaseUrl config
    res <- withResource mongoPool $ \connection ->
              lift $ mapM (A.authenticate connection (AT.scope au)) (AT.methods au)
    case head res of
      Right (tokenId, t) -> do
        let resp = A.produceTokenResponse t baseUrl
        S.json resp
        S.addHeader "X-Subject-Token" (TL.pack tokenId)
        S.status status200
      Left errorMessage -> do
        S.json $ E.unauthorized errorMessage
        S.status status401

receiveExistingTokenH :: (Functor m, MonadIO m, IdentityApi m)
              => AT.Policy -> KeystoneConfig -> ActionM m ()
receiveExistingTokenH policy config = A.requireToken config $ \token -> do
    mSubjectToken <- S.header hXSubjectToken
    baseUrl <- getBaseUrl config
    res <- runExceptT $ do
      when (isNothing mSubjectToken) $ throwError "Could not find token, ."
      let mst = readMaybe $ TL.unpack $ fromJust mSubjectToken

      when (isNothing mst) $ throwError "Token is not an object id"
      let st = fromJust mst
      mToken <- liftIO $ CD.withDB (database config) $ MT.findTokenById st

      when (isNothing mToken) $ throwError $ "Could not find token, " ++ (show st) ++ "."
      let token = fromJust mToken
      currentTime <- liftIO getCurrentTime

      when (currentTime > (MT.expiresAt token)) $ throwError $ "Could not find token, " ++ (show st) ++ "."
      return token

    case res of
      Left errorMessage -> do
        S.status status404
        S.json $ E.notFound errorMessage
      Right tokenToVerify -> A.authorize policy AT.ValidateToken token (AT.Token tokenToVerify) $ do
        S.status status200
        S.json $ A.produceTokenResponse tokenToVerify baseUrl

checkTokenH :: (Functor m, MonadIO m, IdentityApi m)
            => AT.Policy -> KeystoneConfig -> ActionM m ()
checkTokenH policy config = A.requireToken config $ \token -> do
    mSubjectToken <- S.header hXSubjectToken
    res <- runMaybeT $ do
      subjectToken <- MaybeT $ return mSubjectToken
      st <- MaybeT $ return $ readMaybe $ TL.unpack subjectToken
      token <- MaybeT $ liftIO $ CD.withDB (database config) $ MT.findTokenById st
      currentTime <- liftIO getCurrentTime
      when (currentTime > (MT.expiresAt token)) mzero
      return token

    case res of
      Nothing -> do
        S.status status404
      Just tokenToVerify -> do
        A.authorize policy AT.CheckToken token (AT.Token tokenToVerify) $ S.status status204

hXSubjectToken :: TL.Text
hXSubjectToken = "X-Subject-Token"
