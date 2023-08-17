{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Text
import Data.Time (Day)
import GHC.Generics

type Address = Text

data When = Now | On Day
  deriving (Show)

data GeoCoords = GeoCoords
  { lat :: Text,
    lon :: Text
  }
  deriving (Show, Generic, FromJSON)

data SunTimes dt = SunTimes
  { sunrise :: dt,
    sunset :: dt
  }
  deriving (Show, Generic, FromJSON)

data WebAPIAuth = WebAPIAuth
  { email :: Text,
    agent :: Text,
    timeZoneDBKey :: Text
  }
  deriving (Show, Generic, FromJSON)
