{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import

run :: RIO App ()
run = do
  command <- asks $ optionsCommand . appOptions
  configFile <- readFileUtf8 "FilePath"
  case command of
    On -> logInfo "Turning on"
    Off -> logInfo "Turning off"
    Info -> logInfo "Getting Info"
    Bright n -> logInfo $ "Setting brightness to " <> display n
    Temp n -> logInfo $ "Setting Color Temperature to " <> display n
    List n -> logInfo $ "Listing " <> display n

  logDebug "this only happens if it's verbose"
  logInfo "We're inside the application!"
