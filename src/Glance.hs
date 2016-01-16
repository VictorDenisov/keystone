{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Config (readConfig)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Glance.Config (confFileName, GlanceConfig)

main = do
  (config :: GlanceConfig) <- readConfig confFileName
  putStrLn $ unpack $ encode config
