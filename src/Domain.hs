{-# LANGUAGE OverloadedStrings #-}
module Domain
where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object)
import Data.Aeson.Types (object, (.=), Value(..))
import Data.Vector (fromList)

data Domain = Domain

domainJson :: String -> Value
domainJson baseUrl
  = object [ "description" .= ("Fake default domain" :: String)
           , "enabled"     .= True
           , "id"          .= ("default" :: String)
           , "links"       .=
                ( object
                [ "self" .= (baseUrl ++ "/v3/domains/default")
                ] )
           , "name"        .= ("Default" :: String)
           ]
            

produceDomainsReply :: [Domain] -> String -> Value
produceDomainsReply domains baseUrl
  = object [ "links" .= (object [ "next"     .= Null
                                , "previous" .= Null
                                , "self"     .= (baseUrl ++ "/v3/domains")
                                ]
                        )
           , "domains" .= (Array $ fromList [domainJson baseUrl])
           ]
