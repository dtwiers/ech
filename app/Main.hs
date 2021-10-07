{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative
import RIO.Process
import Run

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = GlobalOptions <$> verbose
  where
    verbose = switch (long "verbose" <> short 'v' <> help "Verbose Output")

parseCommand :: Parser Command
parseCommand = subparser $ onP <> offP <> getInfoP <> brightP <> tempP
  where
    onP = command "on" (info (pure On) (progDesc "Turn the default lights on."))
    offP = command "off" (info (pure Off) (progDesc "Turn the default lights off."))
    getInfoP = command "info" (info (pure Info) (progDesc "Get info about lights"))
    brightP = command "bright" (info (helper <*> brightOptions) (progDesc "Set brightness of lights"))
    tempP = command "temp" (info (helper <*> tempOptions) (progDesc "Set Color Temp of lights"))
    brightOptions :: Parser Command
    brightOptions = Bright <$> argument auto (metavar "BRIGHTNESS_IN_%")
    tempOptions :: Parser Command
    tempOptions = Temp <$> argument auto (metavar "TEMPERATURE_IN_K")

parseOptions :: Parser Options
parseOptions = Options <$> parseGlobalOptions <*> parseCommand

getVerbose :: Options -> Bool
getVerbose (Options o _) = optionsVerbose o

main :: IO ()
main = do
  options <- execParser (info (helper <*> parseOptions) $ progDesc "ech: Elgato Control: Haskell")
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
