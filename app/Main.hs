{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative
import RIO.Process
import Run
import System.Directory
import System.FilePath (FilePath, (</>))

parseGlobalOptions :: FilePath -> Parser GlobalOptions
parseGlobalOptions defaultConfigPath = GlobalOptions <$> (verbose <*> configPath)
  where
    verbose = switch (long "verbose" <> short 'v' <> help "Verbose Output")

    configPath =
      option
        auto
        ( long "config"
            <> short 'c'
            <> help "Config File Path"
            <> metavar "PATH"
            <> value defaultConfigPath
        )

parseCommand :: Parser Command
parseCommand path = hsubparser $ onP <> offP <> getInfoP <> brightP <> tempP <> listP
  where
    onP = command "on" (info (pure On) (progDesc "Turn the default lights on."))
    offP = command "off" (info (pure Off) (progDesc "Turn the default lights off."))
    getInfoP = command "info" (info (pure Info) (progDesc "Get info about lights"))
    brightP = command "bright" (info (helper <*> brightOptions) (progDesc "Set brightness of lights"))
    tempP = command "temp" (info (helper <*> tempOptions) (progDesc "Set Color Temp of lights"))
    listP = command "list" (info (helper <*> listOptions) (progDesc "List config items"))
    brightOptions :: Parser Command
    brightOptions = Bright <$> argument auto (metavar "BRIGHTNESS_IN_%")
    tempOptions :: Parser Command
    tempOptions = Temp <$> argument auto (metavar "TEMPERATURE_IN_K")
    listOptions :: Parser Command
    listOptions = List <$> argument parseListable (metavar "ITEM")
    parseListable :: ReadM Listable
    parseListable = eitherReader $ \s -> if s == "devices" then Right Devices else Left "Invalid Listable Item"

parseOptions :: FilePath -> Parser Options
parseOptions defaultFile = Options <$> parseGlobalOptions <*> parseCommand

getVerbose :: Options -> Bool
getVerbose (Options o _) = optionsVerbose o

main :: IO ()
main = do
  defaultDirectory <- getXdgDirectory XdgConfig "ech" </> "ech.yml"
  options <- execParser (info (helper <*> parseOptions defaultDirectory) $ progDesc "ech: Elgato Control: Haskell")
  lo <- logOptionsHandle stderr $ getVerbose options
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
