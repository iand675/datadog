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
import qualified Data.HashMap.Strict as Data.HashMap (union)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Vector (head)

import Network.HTTP.Conduit

import Network.Datadog
import Network.Datadog.Internal


-- | A description of when downtime should occur.
data DowntimeSpec = DowntimeSpec { dsScope :: Text
                                   -- ^ The scope/tag(s) to apply downtime to
                                 , dsStart :: Maybe UTCTime
                                   -- ^ When to start the downtime (or immediately)
                                 , dsEnd :: Maybe UTCTime
                                   -- ^ When to stop the downtime (or indefinitely)
                                 , dsMessage :: Maybe Text
                                   -- ^ A message to include with notifications for this downtime
                                 } deriving (Eq)

instance ToJSON DowntimeSpec where
  toJSON ds = object (["scope" .= dsScope ds]
                      ++ maybe [] (\a -> ["start" .= (ceiling (utcTimeToPOSIXSeconds a) :: Integer)]) (dsStart ds)
                      ++ maybe [] (\a -> ["end" .= (floor (utcTimeToPOSIXSeconds a) :: Integer)]) (dsEnd ds)
                      ++ maybe [] (\a -> ["message" .= a]) (dsMessage ds)
                     )

instance FromJSON DowntimeSpec where
  parseJSON (Object v) = modifyFailure ("DowntimeSpec: " ++) $
                         DowntimeSpec <$>
                         (withArray "Text" (parseJSON . Data.Vector.head) =<< v .: "scope") <*>
                         (maybe (return Nothing) (withScientific "Integer" (\t -> return (Just (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))))) =<< (v .:? "start")) <*>
                         (maybe (return Nothing) (withScientific "Integer" (\t -> return (Just (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))))) =<< (v .:? "end")) <*>
                         v .:? "message" .!= Nothing
  parseJSON a = modifyFailure ("DowntimeSpec: " ++) $ typeMismatch "Object" a


-- | Creates the most basic possible downtime specification, which just
-- contains the scope to which the downtime applies.
minimalDowntimeSpec :: Text -> DowntimeSpec
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
  toJSON downtime = Object $ Data.HashMap.union basemap newmap
    where (Object basemap) = toJSON (dSpec downtime)
          (Object newmap) = object ["id" .= dId downtime]

instance FromJSON Downtime where
  parseJSON (Object v) = modifyFailure ("Downtime: " ++) $
                         Downtime <$> v .: "id" <*> parseJSON (Object v)
  parseJSON a = modifyFailure ("Downtime: " ++) $ typeMismatch "Object" a


-- | Schedule a new downtime in Datadog.
scheduleDowntime :: Environment -> DowntimeSpec -> IO Downtime
scheduleDowntime (Environment keys manager) downtimeSpec = do
  initReq <- parseUrl $ "https://app.datadoghq.com/api/v1/downtime?api_key=" ++ apiKey keys
             ++ "&application_key=" ++ appKey keys
  let request = initReq { method = "POST"
                        , requestHeaders = [("Content-type","application/json")]
                        , requestBody = RequestBodyLBS (encode downtimeSpec)
                        }
  resp <- httpLbs request manager
  decodeDatadog "scheduleDowntime" $ responseBody resp


-- | Update the specification of a downtime in Datadog.
updateDowntime :: Environment -> DowntimeId -> DowntimeSpec -> IO Downtime
updateDowntime (Environment keys manager) did dspec = do
  initReq <- parseUrl $ "https://app.datadoghq.com/api/v1/downtime/" ++ show did
             ++ "?api_key=" ++ apiKey keys ++ "&application_key=" ++ appKey keys
  let request = initReq { method = "PUT"
                        , requestHeaders = [("Content-type","application/json")]
                        , requestBody = RequestBodyLBS (encode dspec)
                        }
  resp <- httpLbs request manager
  decodeDatadog "updateDowntime" $ responseBody resp


-- | Cancel scheduled downtime in Datadog.
cancelDowntime :: Environment -> DowntimeId -> IO ()
cancelDowntime (Environment keys manager) downtimeId = do
  initReq <- parseUrl $ "https://app.datadoghq.com/api/v1/downtime/" ++ show downtimeId
             ++ "?api_key=" ++ apiKey keys ++ "&application_key=" ++ appKey keys
  let request = initReq { method = "DELETE" }
  void $ httpLbs request manager


-- | Load a scheduled downtime from Datadog by its ID.
loadDowntime :: Environment -> DowntimeId -> IO Downtime
loadDowntime (Environment keys manager) downtimeId = do
  request <- parseUrl $ "https://app.datadoghq.com/api/v1/downtime/" ++ show downtimeId
             ++ "?api_key=" ++ apiKey keys ++ "&application_key=" ++ appKey keys
  resp <- httpLbs request manager
  decodeDatadog "loadDowntime" $ responseBody resp


-- | Load all scheduled downtimes, optionally filtering for only downtimes that
-- are currently active.
loadDowntimes :: Environment -> Bool -> IO [Downtime]
loadDowntimes (Environment keys manager) active = do
  request <- parseUrl $ "https://app.datadoghq.com/api/v1/downtime?api_key=" ++ apiKey keys
             ++ "&application_key=" ++ appKey keys
             ++ if active then "&current_only=true" else []
  resp <- httpLbs request manager
  decodeDatadog "loadDowntimes" $ responseBody resp
