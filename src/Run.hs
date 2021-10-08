{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import RIO.Directory (doesFileExist)

getConfigFile :: FilePath -> RIO App (Maybe Text)
getConfigFile filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then Just <$> readFileUtf8 "FilePath"
    else return Nothing

run :: RIO App ()
run = do
  command <- asks $ optionsCommand . appOptions
  configFile <- asks (optionsConfigPath . optionsGlobalOptions . appOptions)
  configContents <- getConfigFile configFile
  case command of
    On _ -> logInfo "Turning on"
    Off _ -> logInfo "Turning off"
    Info _ -> logInfo "Getting Info"
    Bright n _ -> logInfo $ "Setting brightness to " <> display n
    Temp n _ -> logInfo $ "Setting Color Temperature to " <> display n
    List n -> logInfo $ "Listing " <> display n

  logDebug "this only happens if it's verbose"
  logInfo "We're inside the application!"
