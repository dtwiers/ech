{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import RIO.Directory (doesFileExist)

getConfigFile :: FilePath -> RIO App (Maybe MyConfig)
getConfigFile filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then return $ Just <$> (readFileUtf8 "FilePath")
    else return Nothing

run :: RIO App ()
run = do
  command <- asks $ optionsCommand . appOptions
  configFile <- getConfigFile $ asks $ optionsConfigPath . optionsGlobalOptions . appOptions

  case command of
    On -> logInfo "Turning on"
    Off -> logInfo "Turning off"
    Info -> logInfo "Getting Info"
    Bright n -> logInfo $ "Setting brightness to " <> display n
    Temp n -> logInfo $ "Setting Color Temperature to " <> display n
    List n -> logInfo $ "Listing " <> display n

  logDebug "this only happens if it's verbose"
  logInfo "We're inside the application!"
