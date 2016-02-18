{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Nova.Web.Version
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson.Types (object, (.=), Value)

import Nova.Config (NovaConfig)

import Web.Common (UrlBasedValue, UrlInfo(..), withHostUrl, ActionM)

listVersionsH :: (Functor m, MonadIO m) => NovaConfig -> ActionM m ()
listVersionsH config = withHostUrl config apiVersions

apiVersions :: UrlBasedValue
apiVersions (UrlInfo {baseUrl}) = object [ "versions" .= [ apiV21Json baseUrl ] ]

apiV21Json :: String -> Value
apiV21Json baseUrl = object [ "status"  .= ("CURRENT" :: String)
                            , "id"      .= ("v2.1" :: String)
                            , "links"   .= [(object [ "rel" .= ("self" :: String)
                                                    , "href" .= (baseUrl ++ "/v2.1/")
                                                    ])
                                           ]
                            , "updated" .= ("2013-07-23T11:33:21Z" :: String)
                            ]
