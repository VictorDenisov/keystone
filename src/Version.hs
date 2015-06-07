{-# LANGUAGE OverloadedStrings #-}
module Version
( apiV2Reply
, apiV3Reply
, apiVersions
)
where

import Data.Aeson.Types (object, (.=), Value)

jsonMediaTypeV3 = object [ "base" .= ("application/json" :: String)
                         , "type" .= ("application/vnd.openstack.identity-v3+json" :: String)
                         ]

jsonMediaTypeV2 = object [ "base" .= ("application/json" :: String)
                         , "type" .= ("application/vnd.openstack.identity-v2.0+json" :: String)
                         ]

apiV3Reply :: String -> Value
apiV3Reply baseUrl = object ["version" .= apiV3Json baseUrl]

apiV2Reply :: String -> Value
apiV2Reply baseUrl = object ["version" .= apiV2Json baseUrl]

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

apiV2Json :: String -> Value
apiV2Json baseUrl = object [ "status"  .= ("stable" :: String)
                           , "updated" .= ("2014-04-17T00:00:00Z" :: String)
                           , "id"      .= ("v2.0" :: String)
                           , "links"   .= [(object [ "rel" .= ("self" :: String)
                                                   , "href" .= (baseUrl ++ "/v2.0")
                                                   ])
                                          ]
                           , "media-types" .= [jsonMediaTypeV2]
                           ]

apiVersions :: String -> Value
apiVersions baseUrl = object [ "versions"
                                  .= ( object
                                          [ "values" .= [ apiV3Json baseUrl
                                                        , apiV2Json baseUrl
                                                        ] ] ) ]
