{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Common
where

import Data.Aeson (Value(..))

import Config (KeystoneConfig(..), ServerType(..))

import Control.Applicative ((<$>))
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Web.Scotty.Internal.Types (ActionT(..))
import Network.Wai (rawPathInfo, rawQueryString)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL

import qualified Error as E

import qualified Web.Scotty.Trans as S

type ScottyM m = S.ScottyT E.Error m
type ActionM m = ActionT E.Error m

instance MonadThrow m => MonadThrow (ActionT E.Error m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (ActionT E.Error m) where
  catch (ActionT m) c = ActionT $ catch m (runAM . c)

data UrlInfo = UrlInfo
             { baseUrl :: String
             , path    :: String
             , query   :: String
             }

type UrlBasedValue = UrlInfo -> Value

host_url :: MonadIO b => ServerType -> ActionM b (Maybe String)
host_url st = do
  mh <- S.header "host"
  let protocol =
          case st of
            Plain -> "http"
            Tls   -> "https"
  return $ fmap (\h -> protocol ++ "://" ++ (TL.unpack h)) mh

getBaseUrl :: MonadIO b => KeystoneConfig -> ActionM b String
getBaseUrl config = do
  case endpoint config of
    Just e -> return e
    Nothing -> do
      mh <- host_url $ serverType config
      case mh of
        Just h -> return h
        Nothing -> S.raise $ E.badRequest "Host header is required or endpoint should be set"

with_host_url :: ( Functor b
                 , MonadIO b)
                 => KeystoneConfig -> UrlBasedValue -> ActionM b ()
with_host_url config v = do
  pathString <- BS.unpack <$> rawPathInfo <$> S.request
  queryString <- BS.unpack <$> rawQueryString <$> S.request
  url <- getBaseUrl config
  S.json $ v (UrlInfo url pathString queryString)
