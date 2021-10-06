{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import RIO
import RIO.Process
import GHC.Base (Nat)

-- | Command line arguments

newtype GlobalOptions = GlobalOptions
  { optionsVerbose :: Bool
  }

type Brightness = Int
type Temperature = Int

data Command
  = On
  | Off
  | Info
  | Bright Brightness
  | Temp Temperature

data Options = Options GlobalOptions Command

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
