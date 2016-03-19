{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Nova.Web.Server
where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.Aeson.TH (mkParseJSON, defaultOptions)
import Data.Aeson.Types (typeMismatch)
import Language.Haskell.TH.Syntax (nameBase)
import Nova.Config (NovaConfig)
import Nova.Compute (AgentList, ComputeAgent)
import Nova.Web.Server.Types

import Web.Common ( UrlBasedValue, UrlInfo(..), withHostUrl, ActionM
                  , parseRequest)

createServerH :: (Functor m, MonadIO m) => NovaConfig -> AgentList -> ActionM m ()
createServerH config agentList = do
  (scr :: ServerCreateRequest) <- parseRequest
  liftIO $ putStrLn $ show scr

instance FromJSON ServerCreateRequest where
  parseJSON (Object v) = do
    server <- v .: "server"
    parseScr server
  parseJSON v = typeMismatch (nameBase ''ServerCreateRequest) v

parseScr = $(mkParseJSON defaultOptions ''ServerCreateRequest)

