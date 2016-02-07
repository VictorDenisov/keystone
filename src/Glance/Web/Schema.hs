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
  handleSchema "images_schema.json"

imageSchemaH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
imageSchemaH c = do
  handleSchema "image_schema.json"

membersSchemaH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
membersSchemaH c = do
  handleSchema "members_schema.json"

memberSchemaH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
memberSchemaH c = do
  handleSchema "member_schema.json"

metadefsNamespaceSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsNamespaceSchemaH c = do
  handleSchema "metadefs_namespace_schema.json"

metadefsNamespacesSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsNamespacesSchemaH c = do
  handleSchema "metadefs_namespaces_schema.json"

metadefsObjectSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsObjectSchemaH c = do
  handleSchema "metadefs_object_schema.json"

metadefsObjectsSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsObjectsSchemaH c = do
  handleSchema "metadefs_objects_schema.json"

metadefsPropertySchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsPropertySchemaH c = do
  handleSchema "metadefs_property_schema.json"

metadefsPropertiesSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsPropertiesSchemaH c = do
  handleSchema "metadefs_properties_schema.json"

metadefsTagSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsTagSchemaH c = do
  handleSchema "metadefs_tag_schema.json"

metadefsTagsSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsTagsSchemaH c = do
  handleSchema "metadefs_tags_schema.json"

metadefsResourceTypeSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsResourceTypeSchemaH c = do
  handleSchema "metadefs_resource_type_schema.json"

metadefsResourceTypesSchemaH :: (Functor m, MonadIO m)
                         => GlanceConfig -> ActionM m ()
metadefsResourceTypesSchemaH c = do
  handleSchema "metadefs_resource_types_schema.json"

handleSchema :: (Functor m, MonadIO m) => FilePath -> ActionM m ()
handleSchema filename = do
  schema <- liftIO $ LBS.readFile filename
  case (decode schema :: Maybe Value) of
    Nothing ->
      throwError $ S.stringError $ "Failed  to parse " ++ filename ++ " file"
    Just v ->
      S.json v
