{-# LANGUAGE ScopedTypeVariables #-}
module Main
where

import Config (readConfig)
import Nova.Config (confFileName, NovaConfig(..))

main = do
  (config :: NovaConfig) <- readConfig confFileName
  return ()
