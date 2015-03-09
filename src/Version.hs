{-# LANGUAGE OverloadedStrings #-}
module Version
( apiV3
, apiVersions
)
where

import Data.Aeson.Types (object, (.=), Value)

jsonMediaTypeV3 = object [ "base" .= ("application/json" :: String)
                         , "type" .= ("application/vnd.openstack.identity-v3+json" :: String)
                         ]

apiV3 :: String -> Value
apiV3 baseUrl = object [ "status"  .= ("stable" :: String)
                       , "updated" .= ("2013-03-06T00:00:00Z" :: String)
                       , "id"      .= ("v3.0" :: String)
                       , "links"   .= (object [ "rel" .= ("self" :: String)
                                              , "links" .= (baseUrl ++ "/v3")
                                              ]
                                      )
                       , "media-types" .= [jsonMediaTypeV3]
                       ]

apiVersions :: String -> Value
apiVersions baseUrl = object [ "versions" .= ( object [ "values" .= [ apiV3 baseUrl ] ] ) ]
