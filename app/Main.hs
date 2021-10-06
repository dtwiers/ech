{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative
import qualified Paths_ech
import RIO.Process
import Run
import Types

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = GlobalOptions <$> verbose
  where
    verbose = switch (long "verbose" <> short 'v' <> help "Verbose Output")

parseCommand :: Parser Command
parseCommand = subparser $ on <> off <> getInfo <> brightP <> tempP
  where
    on = command "on" (info (pure On) (progDesc "Turn the default lights on."))
    off = command "off" (info (pure Off) (progDesc "Turn the default lights off."))
    getInfo = command "info" (info (pure Info) (progDesc "Get info about lights"))
    brightP = command "bright" (info (helper <*> brightOptions) (progDesc "Set brightness of lights"))
    tempP = command "temp" (info (helper <*> tempOptions) (progDesc "Set Color Temp of lights"))
    brightOptions :: Parser Command
    brightOptions = Bright <$> argument auto (metavar "BRIGHTNESS_IN_%")
    tempOptions :: Parser Command
    tempOptions = Temp <$> argument auto (metavar "TEMPERATURE_IN_K")

parseOptions :: Parser Options
parseOptions = Options <$> parseGlobalOptions <*> parseCommand

main :: IO ()
main = do
  options <- execParser (info (helper <*> parseOptions) $ progDesc "ech: Elgato Control: Haskell")
  lo <- logOptionsHandle stderr (optionsVerbose $ fst options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
