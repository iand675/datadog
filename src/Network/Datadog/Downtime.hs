{-# LANGUAGE OverloadedStrings #-}

{-|
Downtime prevents all alerting related to specific Datadog scopes.
-}
module Network.Datadog.Downtime
( DowntimeSpec(..)
, Downtime
, dId
, dSpec
, DowntimeId
, minimalDowntimeSpec
, scheduleDowntime
, updateDowntime
, cancelDowntime
, loadDowntime
, loadDowntimes
) where


import Control.Monad (void)

import Data.Aeson
import Data.Aeson.Types (modifyFailure, typeMismatch)
import qualified Data.HashMap.Strict as Data.HashMap (insert)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector ((!?))

import Network.HTTP.Types

import Network.Datadog
import Network.Datadog.Internal


-- | A description of when downtime should occur.
data DowntimeSpec = DowntimeSpec { dsScope :: Tag
                                   -- ^ The scope to apply downtime to (if applying downtime to a
                                   -- host, use a tag of the form "host:hostname", NOT just
                                   -- "hostname")
                                 , dsStart :: Maybe UTCTime
                                   -- ^ When to start the downtime (or immediately)
                                 , dsEnd :: Maybe UTCTime
                                   -- ^ When to stop the downtime (or indefinitely)
                                 , dsMessage :: Maybe Text
                                   -- ^ A message to include with notifications for this downtime
                                 } deriving (Eq)

instance ToJSON DowntimeSpec where
  toJSON ds = object $
              prependMaybe (\a -> "start" .= (ceiling (utcTimeToPOSIXSeconds a) :: Integer)) (dsStart ds) $
              prependMaybe (\a -> "end" .= (floor (utcTimeToPOSIXSeconds a) :: Integer)) (dsEnd ds)
              ["scope" .= dsScope ds]

instance FromJSON DowntimeSpec where
  parseJSON (Object v) = modifyFailure ("DowntimeSpec: " ++) $
                         DowntimeSpec <$>
                         (withArray "Text" (\t -> maybe (fail "\"scope\" Array is too short") parseJSON (t !? 0)) =<< v .: "scope") <*>
                         (maybe (return Nothing) (withScientific "Integer" (\t -> return (Just (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))))) =<< (v .:? "start")) <*>
                         (maybe (return Nothing) (withScientific "Integer" (\t -> return (Just (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))))) =<< (v .:? "end")) <*>
                         v .:? "message" .!= Nothing
  parseJSON a = modifyFailure ("DowntimeSpec: " ++) $ typeMismatch "Object" a


-- | Creates the most basic possible downtime specification, which just
-- contains the scope to which the downtime applies.
minimalDowntimeSpec :: Tag -> DowntimeSpec
minimalDowntimeSpec scope = DowntimeSpec { dsScope = scope
                                         , dsStart = Nothing
                                         , dsEnd = Nothing
                                         , dsMessage = Nothing
                                         }


-- | Datadog's internal reference to a specific donwtime instance.
type DowntimeId = Int

-- | A scheduled donwtime stored in Datadog.
data Downtime = Downtime { dId :: DowntimeId
                           -- ^ Datadog's unique reference to the scheduled downtime
                         , dSpec :: DowntimeSpec
                           -- ^ Context on the downtime schedule
                         } deriving (Eq)

instance ToJSON Downtime where
  toJSON downtime = Object $ Data.HashMap.insert "id" (toJSON (dId downtime)) basemap
    where (Object basemap) = toJSON (dSpec downtime)

instance FromJSON Downtime where
  parseJSON (Object v) = modifyFailure ("Downtime: " ++) $
                         Downtime <$> v .: "id" <*> parseJSON (Object v)
  parseJSON a = modifyFailure ("Downtime: " ++) $ typeMismatch "Object" a


-- | Schedule a new downtime in Datadog.
scheduleDowntime :: Environment -> DowntimeSpec -> IO Downtime
scheduleDowntime env downtimeSpec =
  let path = "downtime"
  in datadogHttp env path [] POST (Just $ encode downtimeSpec) >>=
     decodeDatadog "scheduleDowntime"


-- | Update the specification of a downtime in Datadog.
updateDowntime :: Environment -> DowntimeId -> DowntimeSpec -> IO Downtime
updateDowntime env did dspec =
  let path = "downtime/" ++ show did
  in datadogHttp env path [] PUT (Just $ encode dspec) >>=
     decodeDatadog "updateDowntime"


-- | Cancel scheduled downtime in Datadog.
cancelDowntime :: Environment -> DowntimeId -> IO ()
cancelDowntime env downtimeId =
  let path = "downtime/" ++ show downtimeId
  in void $ datadogHttp env path [] DELETE Nothing


-- | Load a scheduled downtime from Datadog by its ID.
loadDowntime :: Environment -> DowntimeId -> IO Downtime
loadDowntime env downtimeId =
  let path = "downtime/" ++ show downtimeId
  in datadogHttp env path [] GET Nothing >>=
     decodeDatadog "loadDowntime"


-- | Load all scheduled downtimes, optionally filtering for only downtimes that
-- are currently active.
loadDowntimes :: Environment -> Bool -> IO [Downtime]
loadDowntimes env active =
  let path = "downtime"
      query = [("current_only", "true") | active]
  in datadogHttp env path query GET Nothing >>=
     decodeDatadog "loadDowntimes"
