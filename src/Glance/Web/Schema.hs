{-# LANGUAGE ScopedTypeVariables #-}

module Glance.Web.Schema
where

import Common (loggerName)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value, decode)
import Data.Maybe (fromJust)
import Glance.Config (confFileName, GlanceConfig(..))
import Network.HTTP.Types.Status (status500)
import System.Log.Logger (errorM)
import Web.Common (UrlBasedValue, UrlInfo(..), withHostUrl, ActionM)

import qualified Data.ByteString.Lazy as LBS
import qualified Web.Scotty.Trans as S

imagesSchemaH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
imagesSchemaH c = do
  images_schema <- liftIO $ LBS.readFile "images_schema.json"
  case (decode images_schema :: Maybe Value) of
    Nothing ->
      throwError $ S.stringError "Failed  to parse images_schema.json file"
    Just v ->
      S.json v

imageSchemaH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
imageSchemaH c = do
  image_schema <- liftIO $ LBS.readFile "image_schema.json"
  case (decode image_schema :: Maybe Value) of
    Nothing ->
      throwError $ S.stringError "Failed  to parse image_schema.json file"
    Just v ->
      S.json v
