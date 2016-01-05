{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.Domain
where

import Config (KeystoneConfig(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value(..))
import Data.Aeson.Types (object, (.=))
import Data.Vector (fromList)
import Model.IdentityApi (IdentityApi)
import Network.HTTP.Types.Status (status200)

import Web.Common (UrlBasedValue, UrlInfo(..), ActionM, withHostUrl, parseId)

import qualified Database.MongoDB as M
import qualified Keystone.Model.Domain as MD
import qualified Web.Auth as A
import qualified Web.Auth.Types as AT
import qualified Web.Scotty.Trans as S

listDomainsH :: (Functor m, MonadIO m, IdentityApi m)
            => AT.Policy -> KeystoneConfig -> ActionM m ()
listDomainsH policy config = A.requireToken config $ \token -> do
    A.authorize policy AT.ListDomains token AT.EmptyResource $ do
      S.status status200
      withHostUrl config $ produceDomainsReply []

domainDetailsH :: (Functor m, MonadIO m, IdentityApi m)
              => AT.Policy -> KeystoneConfig -> ActionM m ()
domainDetailsH policy config = A.requireToken config $ \token -> do
    (did :: M.ObjectId) <- parseId "did"
    A.authorize policy AT.ShowDomainDetails token AT.EmptyResource $ do
      S.status status200
      withHostUrl config $ produceDomainReply MD.Domain

produceDomainJson :: MD.Domain -> String -> Value
produceDomainJson domain baseUrl
  = object [ "description" .= ("Fake default domain" :: String)
           , "enabled"     .= True
           , "id"          .= MD.defaultDomainId
           , "links"       .=
                ( object
                [ "self" .= (baseUrl ++ "/v3/domains/" ++ MD.defaultDomainId)
                ] )
           , "name"        .= MD.defaultDomainName
           ]

produceDomainReply :: MD.Domain -> UrlBasedValue
produceDomainReply domain (UrlInfo {baseUrl})
      = object [ "domain" .= produceDomainJson domain baseUrl ]

produceDomainsReply :: [MD.Domain] -> UrlBasedValue
produceDomainsReply domains (UrlInfo {baseUrl})
  = object [ "links" .= (object [ "next"     .= Null
                                , "previous" .= Null
                                , "self"     .= (baseUrl ++ "/v3/domains")
                                ]
                        )
           , "domains" .= (Array $ fromList [produceDomainJson MD.Domain baseUrl])
           ]
