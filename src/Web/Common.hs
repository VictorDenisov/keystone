{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Common
where

import Common (loggerName)

import Config (ServerType(..), BaseConfig(..))

import Control.Applicative ((<$>))
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson (Value(..))
import Data.Aeson.Types (FromJSON)
import Network.Wai (rawPathInfo, rawQueryString)
import System.Log.Logger (debugM)
import Text.Read (readMaybe)
import Web.Scotty.Internal.Types (ActionT(..))

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

hostUrl :: MonadIO b => ServerType -> ActionM b (Maybe String)
hostUrl st = do
  mh <- S.header "host"
  let protocol =
          case st of
            Plain -> "http"
            Tls   -> "https"
  return $ fmap (\h -> protocol ++ "://" ++ (TL.unpack h)) mh

getBaseUrl :: (MonadIO b, BaseConfig bc) => bc -> ActionM b String
getBaseUrl config = do
  case getEndpoint config of
    Just e -> return e
    Nothing -> do
      mh <- hostUrl $ getServerType config
      case mh of
        Just h -> return h
        Nothing -> S.raise $ E.badRequest "Host header is required or endpoint should be set"

withHostUrl :: ( Functor b
               , MonadIO b
               , BaseConfig bc)
               => bc -> UrlBasedValue -> ActionM b ()
withHostUrl config v = do
  pathString <- BS.unpack <$> rawPathInfo <$> S.request
  queryString <- BS.unpack <$> rawQueryString <$> S.request
  url <- getBaseUrl config
  S.json $ v (UrlInfo url pathString queryString)

parseId :: (MonadIO m, Read a) => TL.Text -> ActionM m a
parseId paramName = do
  s <- S.param paramName
  case readMaybe s of
    Nothing -> S.raise $ E.badRequest $ "Failed to parse ObjectId from " ++ (TL.unpack paramName)
    Just v  -> return v

parseMaybeString :: (MonadIO m) => TL.Text -> ActionM m (Maybe String)
parseMaybeString paramName =
  (flip S.rescue) (\msg -> return Nothing) $ do
    (value :: String) <- S.param paramName
    return $ Just value

parseRequest :: ( Show a
                , FromJSON a
                , MonadIO b)
                => ActionM b a
parseRequest = do
  request <- S.rescue S.jsonData $ \e ->
    S.raise $ E.badRequest $ E.message e
  liftIO $ debugM loggerName $ "Parsed request body: " ++ (show request)
  return request
