{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Glance.Web.Image
where

import Common (fromObject, underscoreOptions)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson ( object, (.=), FromJSON(..), Value(..), ToJSON(..))
import Data.Aeson.TH (mkParseJSON)
import Data.HashMap.Strict (insert)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Glance.Config (GlanceConfig(database))
--import Glance.Model.Image (createImage)
import Glance.Web.Image.Types (ImageCreateRequest(..))
import Network.HTTP.Types.Status (status200)
import Web.Common (ActionM, parseRequest, parseId, parseMaybeString)

import qualified Data.ByteString.Lazy as LB
import qualified Database.MongoDB as M
import qualified Glance.Model.Image as MI
import qualified Model.Mongo.Common as CD
import qualified Web.Scotty.Trans as S

listImagesH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
listImagesH config = do
  imageName <- parseMaybeString "name"
  images <- liftIO $ CD.withDB (database config) $ MI.listImages imageName
  S.status status200
  S.json $ object [ "images" .= (map (fromObject . toJSON) images)
                  , "schema" .= ("/v2/schemas/images" :: String)
                  , "first" .= ("/v2/images" :: String)]

createImageH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
createImageH config = do
  (d :: ImageCreateRequest) <- parseRequest
  image <- liftIO $ newRequestToImage d
  liftIO $ putStrLn $ show image
  liftIO $ CD.withDB (database config) $ MI.createImage image
  S.status status200
  S.json $ produceImageJson image

uploadImageH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
uploadImageH config = do
    (iid :: M.ObjectId) <- parseId "iid"
    s <- S.body
    liftIO $ LB.writeFile (show iid) s

newRequestToImage :: ImageCreateRequest -> IO MI.Image
newRequestToImage ImageCreateRequest{..} = do
  imageId <- M.genObjectId
  return $ MI.Image
                imageId
                name
                (fromMaybe MI.Private visibility)
                MI.Queued
                (fromMaybe [] tags)
                containerFormat
                diskFormat
                (fromMaybe 0 minDisk)
                (fromMaybe 0 minRam)
                (fromMaybe False protected)

instance FromJSON ImageCreateRequest where
  parseJSON v = parseIcr v

parseIcr = $(mkParseJSON underscoreOptions ''ImageCreateRequest)

produceImageJson :: MI.Image -> Value
produceImageJson (image@MI.Image{..})
  = Object
        $ insert "self" (String $ fromString $ "/v2/images/" ++ (show _id))
        $ insert "schema" (String $ "/v2/schemas/image")
        $ fromObject $ toJSON image
