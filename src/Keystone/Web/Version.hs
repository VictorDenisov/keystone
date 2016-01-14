{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Keystone.Web.Version
(listVersionsH, v3detailsH)
where

import Config (KeystoneConfig)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson.Types (object, (.=), Value)
import Web.Common (UrlBasedValue, UrlInfo(..), withHostUrl, ActionM)

listVersionsH :: (Functor m, MonadIO m) => KeystoneConfig -> ActionM m ()
listVersionsH config = withHostUrl config apiVersions

v3detailsH :: (Functor m, MonadIO m) => KeystoneConfig -> ActionM m ()
v3detailsH config = withHostUrl config apiV3Reply


jsonMediaTypeV3 = object [ "base" .= ("application/json" :: String)
                         , "type" .= ("application/vnd.openstack.identity-v3+json" :: String)
                         ]

apiV3Reply :: UrlBasedValue
apiV3Reply (UrlInfo {baseUrl}) = object ["version" .= apiV3Json baseUrl]

apiV3Json :: String -> Value
apiV3Json baseUrl = object [ "status"  .= ("stable" :: String)
                           , "updated" .= ("2013-03-06T00:00:00Z" :: String)
                           , "id"      .= ("v3.0" :: String)
                           , "links"   .= [(object [ "rel" .= ("self" :: String)
                                                   , "href" .= (baseUrl ++ "/v3")
                                                   ])
                                          ]
                           , "media-types" .= [jsonMediaTypeV3]
                           ]

apiVersions :: UrlBasedValue
apiVersions (UrlInfo {baseUrl}) = object [ "versions"
                                  .= ( object
                                          [ "values" .= [ apiV3Json baseUrl ] ] ) ]
