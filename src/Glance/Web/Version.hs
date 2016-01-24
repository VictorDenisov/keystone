{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Glance.Web.Version
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson.Types (object, (.=), Value)

import Glance.Config (GlanceConfig)

import Web.Common (UrlBasedValue, UrlInfo(..), withHostUrl, ActionM)

listVersionsH :: (Functor m, MonadIO m) => GlanceConfig -> ActionM m ()
listVersionsH config = withHostUrl config apiVersions

apiVersions :: UrlBasedValue
apiVersions (UrlInfo {baseUrl}) = object [ "versions" .= [ apiV23Json baseUrl ] ]

apiV23Reply :: UrlBasedValue
apiV23Reply (UrlInfo {baseUrl}) = object ["version" .= apiV23Json baseUrl]

apiV23Json :: String -> Value
apiV23Json baseUrl = object [ "status"  .= ("CURRENT" :: String)
                            , "id"      .= ("v2.3" :: String)
                            , "links"   .= [(object [ "rel" .= ("self" :: String)
                                                    , "href" .= (baseUrl ++ "/v2")
                                                    ])
                                           ]
                            ]
