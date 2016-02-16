{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Glance.Web.Image
where

import Common (fromObject, underscoreOptions)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson ( ToJSON(..), object, (.=), Object(..), FromJSON(..), (.:)
                  , Value(..))
import Data.Aeson.TH (mkParseJSON, mkToJSON)
import Data.Aeson.Types (typeMismatch)
import Data.HashMap.Strict (insert)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Glance.Config (GlanceConfig(database))
--import Glance.Model.Image (createImage)
import Glance.Web.Image.Types (ImageCreateRequest(..))
import Glance.Web.Image.Types
import Language.Haskell.TH.Syntax (nameBase)
import Web.Common (ActionM, parseRequest)

import qualified Database.MongoDB as M
import qualified Glance.Model.Image as MI
import qualified Model.Mongo.Common as CD
import qualified Web.Scotty.Trans as S

listImagesH :: (Functor m, MonadIO m) => ActionM m ()
listImagesH = do
  S.json $ object [ "images" .= ([] :: [Object])
                  , "schema" .= ("/v2/schemas/images" :: String)
                  , "first" .= ("/v2/images" :: String)]

createImageH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
createImageH config = do
  (d :: ImageCreateRequest) <- parseRequest
  image <- liftIO $ newRequestToImage d
  liftIO $ putStrLn $ show image
  liftIO $ CD.withDB (database config) $ MI.createImage image
  S.json $ produceImageJson image

newRequestToImage :: ImageCreateRequest -> IO MI.Image
newRequestToImage ImageCreateRequest{..} = do
  imageId <- M.genObjectId
  return $ MI.Image
                imageId
                name
                --(fromMaybe True visibility)
                (fromMaybe [] tags)
                containerFormat
                diskFormat
                (fromMaybe 0 minDisk)
                (fromMaybe 0 minRam)
                (fromMaybe False protected)

instance FromJSON ImageCreateRequest where
  parseJSON v = do
    parseIcr v
  parseJSON v = typeMismatch (nameBase ''ImageCreateRequest) v

parseIcr = $(mkParseJSON underscoreOptions ''ImageCreateRequest)

encodeImageReply = $(mkToJSON underscoreOptions ''MI.Image)

produceImageJson :: MI.Image -> Value
produceImageJson (image@MI.Image{..})
  = Object
        $ insert "self" (String $ fromString $ "/v2/images/" ++ (show _id))
        $ insert "schema" (String $ "/v2/schemas/image")
        $ fromObject $ encodeImageReply image
