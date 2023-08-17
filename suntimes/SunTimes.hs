{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module SunTimes (getSunTimes) where

import App
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Network.HTTP.Req
import STExcept
import Types

newtype SunTimesWrapper dt = SunTimesWrapper {results :: SunTimes dt}
  deriving (Show, Generic, FromJSON)

getSunTimesUTC :: GeoCoords -> When -> MyApp (SunTimes UTCTime)
getSunTimesUTC GeoCoords {..} w = handle rethrowReqException $ liftIO $ runReq defaultHttpConfig $ do
  r <- req GET ep NoReqBody jsonResponse reqParams
  pure (results $ responseBody r)
  where
    ep = https "api.sunrise-sunset.org" /: "json"
    reqParams = mconcat $ ["lat" =: lat, "lon" =: lon, "formatted" =: (0 :: Int)] ++ whenToOptions w
    whenToOptions Now = []
    whenToOptions (On day) = ["date" =: formatTime defaultTimeLocale "%Y-%m-%d" day]

getSunTimes :: GeoCoords -> When -> MyApp (SunTimes ZonedTime)
getSunTimes gc d = do
  SunTimes {..} <- getSunTimesUTC gc d `catch` noTimeHandler
  ltz <- lookupTimeZone gc sunrise `catchAll` (const $ pure utc)
  return $ SunTimes (utcToZonedTime ltz sunrise) (utcToZonedTime ltz sunset)
  where
    noTimeHandler :: MonadThrow m => SunInfoException -> m a
    noTimeHandler (ServiceAPIError _) = throwM (UnknownTime gc)
    noTimeHandler e = throwM e

data TimeZoneInfo = TimeZoneInfo
  { gmtOffset :: Int,
    abbreviation :: String,
    dst :: String
  }
  deriving (Show, Generic, FromJSON)

lookupTimeZone :: GeoCoords -> UTCTime -> MyApp TimeZone
lookupTimeZone GeoCoords {..} t = do
  key <- asks timeZoneDBKey
  let ep = http "api.timezonedb.com" /: "v2.1" /: "get-time-zone"
      reqParams =
        mconcat
          [ "key" =: key,
            "lat" =: lat,
            "lng" =: lon,
            "time" =: formatTime defaultTimeLocale "%s" t,
            "format" =: ("json" :: T.Text),
            "fields" =: ("gmtOffset,abbreviation,dst" :: T.Text),
            "by" =: ("position" :: T.Text)
          ]
  r <-
    liftIO $
      runReq defaultHttpConfig $
        req GET ep NoReqBody jsonResponse reqParams
  pure (timeZoneInfo2TimeZone $ responseBody r)
  where
    secondsToTimeZone s = minutesToTimeZone (s `div` 60)
    timeZoneInfo2TimeZone TimeZoneInfo {..} =
      (secondsToTimeZone gmtOffset)
        { timeZoneName = abbreviation,
          timeZoneSummerOnly = dst == "1"
        }
