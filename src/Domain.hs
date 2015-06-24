{-# LANGUAGE OverloadedStrings #-}
module Domain
where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.Types (object, (.=), Value(..))
import Data.Vector (fromList)

data Domain = Domain

defaultDomainId :: String
defaultDomainId = "55897e27b74cea2c9383d14c"

produceDomainJson :: Domain -> String -> Value
produceDomainJson domain baseUrl
  = object [ "description" .= ("Fake default domain" :: String)
           , "enabled"     .= True
           , "id"          .= defaultDomainId
           , "links"       .=
                ( object
                [ "self" .= (baseUrl ++ "/v3/domains/" ++ defaultDomainId)
                ] )
           , "name"        .= ("Default" :: String)
           ]

produceDomainReply :: Domain -> String -> Value
produceDomainReply domain baseUrl
      = object [ "domain" .= produceDomainJson domain baseUrl ]

produceDomainsReply :: [Domain] -> String -> Value
produceDomainsReply domains baseUrl
  = object [ "links" .= (object [ "next"     .= Null
                                , "previous" .= Null
                                , "self"     .= (baseUrl ++ "/v3/domains")
                                ]
                        )
           , "domains" .= (Array $ fromList [produceDomainJson Domain baseUrl])
           ]
