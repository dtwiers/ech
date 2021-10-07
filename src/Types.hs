{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import qualified Data.Aeson as Json
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:))
import RIO
import RIO.Process

-- | Command line arguments
newtype GlobalOptions = GlobalOptions
  { optionsVerbose :: Bool
  }

instance Display GlobalOptions where
  display = \n ->
    if optionsVerbose n then "On" else "Off"

type Brightness = Int

type Temperature = Int

data Listable
  = Devices
  deriving (Show)

instance Display Listable where
  display = \Devices -> "Devices"

data Command
  = On
  | Off
  | Info
  | Bright Brightness
  | Temp Temperature
  | List Listable
  deriving (Show)

data Options = Options
  { optionsGlobalOptions :: GlobalOptions,
    optionsCommand :: Command
  }

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

data MyConfig = MyConfig
  { defaultDevices :: [String],
    devices :: [Device]
  }
  deriving (Eq, Show)

data Device = Device
  { name :: String,
    ipAddress :: String,
    port :: Int
  }
  deriving (Eq, Show)

instance Yaml.FromJSON Device where
  parseJSON (Yaml.Object v) =
    Device <$>
    v .: "name" <*>
    v .: "ipAddress" <*>
    v .: "port"
  parseJSON _ = fail "Yaml Parse Failure"

instance Yaml.FromJSON MyConfig where
  parseJSON (Yaml.Object v) =
    MyConfig <$>
    v .: "defaultDevices" <*>
    v .: "devices"
  parseJSON _ = fail "Yaml Parse Failure"