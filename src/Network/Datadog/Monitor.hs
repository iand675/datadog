{-# LANGUAGE OverloadedStrings #-}

{-|
Monitoring all of your infrastructure in one place wouldn’t be complete without
the ability to know when critical changes are occurring. Datadog gives you the
ability to create monitors that will actively check metrics, integration
availability, network endpoints and more. Once a monitor is created, you will
be notified when its conditions are met.

A simple way to get started with monitors:

> main = do
>   env <- createEnvironment =<< loadKeysFromEnv
>   -- Check if the average bytes received in the last five minutes is >100 on host0
>   let query = "avg(last_5m):sum:system.net.bytes_rcvd{host:host0} > 100"
>   let mspec = minimalMonitorSpec MetricAlert query
>   monitor <- createMonitor env mspec
>   print $ mId monitor
-}
module Network.Datadog.Monitor
( MonitorSpec(..)
, MonitorType(..)
, MonitorOptions
, getSilencedMonitorScopes
, silenceMonitorScope
, unsilenceMonitorScope
, unsilenceAllMonitorScope
, doesNotifyOnNoMonitorData
, notifyOnNoMonitorData
, noNotifyOnNoMonitorData
, getMonitorTimeout
, setMonitorTimeout
, clearMonitorTimeout
, doesRenotifyMonitor
, renotifyMonitor
, noRenotifyMonitor
, doesNotifyOnAudit
, notifyOnAudit
, noNotifyOnAudit
, Monitor
, mId
, mSpec
, MonitorId
, minimalMonitorSpec
, createMonitor
, updateMonitor
, deleteMonitor
, loadMonitor
, loadMonitors
, muteAllMonitors
, unmuteAllMonitors
) where


import Control.Arrow
import Control.Exception
import Control.Monad (void)

import Data.Aeson
import Data.Aeson.Types (modifyFailure, typeMismatch)
import qualified Data.HashMap.Strict as Data.HashMap
import Data.List (intercalate)
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Network.Datadog
import Network.Datadog.Internal


-- | Each monitor is of a specific type, which determines what sort of check
-- the monitor performs.
data MonitorType = MetricAlert
                   -- ^ Watches a (combination of) metric(s), alerting when it
                   -- crosses some threshold.
                 | ServiceCheck
                   -- ^ Watches a service and alerts when the service enters a
                   -- failing state.
                 deriving (Eq)

instance Show MonitorType where
  show MetricAlert = "metric alert"
  show ServiceCheck = "service check"

instance ToJSON MonitorType where
  toJSON MetricAlert = Data.Aeson.String "metric alert"
  toJSON ServiceCheck = Data.Aeson.String "service check"

instance FromJSON MonitorType where
  parseJSON (Data.Aeson.String "metric alert") = return MetricAlert
  -- TODO figure out what "query alert" actually is
  parseJSON (Data.Aeson.String "query alert") = return MetricAlert
  parseJSON (Data.Aeson.String "service check") = return ServiceCheck
  parseJSON (Data.Aeson.String s) = fail $ "MonitorType: String \"" ++ unpack s ++ "\" is not a valid MonitorType"
  parseJSON a = modifyFailure ("MonitorType: " ++) $ typeMismatch "String" a


-- | Advanced configuration parameters for a monitor.
data MonitorOptions = MonitorOptions { moSilenced :: Data.HashMap.HashMap Text (Maybe Integer)
                                     , moNotifyNoData :: Bool
                                     , moNoDataTimeframe :: Maybe Integer
                                     , moTimeoutH :: Maybe Integer
                                     , moRenotifyInterval :: Maybe Integer
                                     , moEscalationMessage :: Text
                                     , moNotifyAudit :: Bool
                                     } deriving (Eq)

instance ToJSON MonitorOptions where
  toJSON options = Object $ Data.HashMap.fromList [("notify_no_data", Bool (moNotifyNoData options))
                                                  ,("no_data_timeframe", maybe Null (Number . fromIntegral) (moNoDataTimeframe options))
                                                  ,("timeout_h", maybe Null (Number . fromIntegral) (moTimeoutH options))
                                                  ,("renotify_interval", maybe Null (Number . fromIntegral) (moRenotifyInterval options))
                                                  ,("escalation_message", Data.Aeson.String (moEscalationMessage options))
                                                  ,("notify_audit", Bool (moNotifyAudit options))]

instance FromJSON MonitorOptions where
  parseJSON (Object v) = modifyFailure ("MonitorOptions: " ++) $
                         MonitorOptions <$>
                         v .:? "silenced" .!= Data.HashMap.empty <*>
                         v .:? "notify_no_data" .!= False <*>
                         v .:? "no_data_timeframe" .!= Nothing <*>
                         v .:? "timeout_h" .!= Nothing <*>
                         v .:? "renotify_interval" .!= Nothing <*>
                         v .:? "escalation_message" .!= "" <*>
                         v .:? "notify_audit" .!= False
  parseJSON a = modifyFailure ("MonitorOptions: " ++) $ typeMismatch "Object" a

-- | Creates the most basic specification required by a monitor, containing the
-- type of monitor and the query string used to detect the monitor's state.
--
-- Generates a set of "default" Monitor options, which specify as little
-- optional configuration as possible.  This includes:
--
--   * No silencing of any part of the monitor
--   * No notification when data related to the monitor is missing
--   * No alert timeout after the monitor is triggeredn
--   * No renotification when the monitor is triggered
--   * No notification when the monitor is modified
--
-- In production situations, it is /not safe/ to rely on this documented
-- default behaviour for critical setitngs; use the helper functions to
-- introspect the MonitorOptions instance provided by this function. This also
-- protects against future modifications to this API.
defaultMonitorOptions :: MonitorOptions
defaultMonitorOptions = MonitorOptions { moSilenced = Data.HashMap.empty
                                       , moNotifyNoData = False
                                       , moNoDataTimeframe = Nothing
                                       , moTimeoutH = Nothing
                                       , moRenotifyInterval = Nothing
                                       , moEscalationMessage = ""
                                       , moNotifyAudit = False
                                       }

-- | Provide a list of the silenced scopes for this monitor and the time at
-- which the silencer will expire (may be indefinite). The monitor @ "*" @
-- refers to the monitor at large (un-scoped).
getSilencedMonitorScopes :: MonitorOptions -> [(Text, Maybe UTCTime)]
getSilencedMonitorScopes options = map (second (fmap (posixSecondsToUTCTime . fromIntegral))) $ Data.HashMap.toList $ moSilenced options

-- | Silence a given scope until some time (or indefinitely), replacing the
-- current silencer on the given scope if one already exists.
silenceMonitorScope :: Text -> Maybe UTCTime -> MonitorOptions -> MonitorOptions
silenceMonitorScope scope mtime old = old { moSilenced = silenced }
  where silenced = Data.HashMap.insert scope (fmap (floor . utcTimeToPOSIXSeconds) mtime) $ moSilenced old

-- | Remove the silencer from a given scope, if the scope is currently
-- silenced.
unsilenceMonitorScope :: Text -> MonitorOptions -> MonitorOptions
unsilenceMonitorScope scope old = old { moSilenced = silenced }
  where silenced = Data.HashMap.delete scope $ moSilenced old

-- | Unsilence every scope in the monitor, including the global scope.
unsilenceAllMonitorScope :: MonitorOptions -> MonitorOptions
unsilenceAllMonitorScope old = old { moSilenced = Data.HashMap.empty }

-- | Determine how long without data a monitor will go before notifying to
-- such, providing Nothing if the monitor will never notify on lack of data.
doesNotifyOnNoMonitorData :: MonitorOptions -> Maybe NominalDiffTime
doesNotifyOnNoMonitorData options = if moNotifyNoData options
                                    then Just (maybe (throw (AssertionFailed "Datadog Library internal error")) (fromIntegral . (*60)) $ moNoDataTimeframe options)
                                    else Nothing

-- | Have the monitor notify when it does not receive data for some given
-- amount of time (rounded down to the nearest minute).
notifyOnNoMonitorData :: NominalDiffTime -> MonitorOptions -> MonitorOptions
notifyOnNoMonitorData difftime old = old { moNotifyNoData = True, moNoDataTimeframe = Just stamp }
  where stamp = floor (difftime / 60)

-- | Prevent the monitor from notifying when it is missing data.
noNotifyOnNoMonitorData :: MonitorOptions -> MonitorOptions
noNotifyOnNoMonitorData old = old { moNotifyNoData = False, moNoDataTimeframe = Nothing }

-- | Determine after how long the monitor will stop alerting after it is
-- triggered, providing Nothing if the monitor will never stop alerting.
getMonitorTimeout :: MonitorOptions -> Maybe NominalDiffTime
getMonitorTimeout options = (fromIntegral . (*3600)) <$> moTimeoutH options

-- | Have the monitor stop alerting some time after it is triggered (rounded up
-- to the nearest hour).
setMonitorTimeout :: NominalDiffTime -> MonitorOptions -> MonitorOptions
setMonitorTimeout difftime old = old { moTimeoutH = Just stamp }
  where stamp = ceiling (difftime / 3600)

-- | Prevent the monitor from timing out after it is triggered.
clearMonitorTimeout :: MonitorOptions -> MonitorOptions
clearMonitorTimeout old = old { moTimeoutH = Nothing }

-- | Determine after how long after being triggered the monitor will re-notify,
-- and what message it will include in the re-notification (if any), providing
-- Nothing if the monitor will never re-notify.
doesRenotifyMonitor :: MonitorOptions -> Maybe (NominalDiffTime,Maybe Text)
doesRenotifyMonitor options = (\i -> (fromIntegral (i * 60), result)) <$> moRenotifyInterval options
  where message = moEscalationMessage options
        result = if message == "" then Nothing else Just message

-- | Have the monitor re-notify some amount of time after the most recent
-- notification (rounded down to the nearest minute) and optionally what text
-- it will include in the re-notification.
renotifyMonitor :: NominalDiffTime -> Maybe Text -> MonitorOptions -> MonitorOptions
renotifyMonitor difftime mmessage old = old { moRenotifyInterval = Just stamp, moEscalationMessage = message }
  where stamp = floor (difftime / 60)
        message = fromMaybe "" mmessage

-- | Prevent the monitor from re-notifying after it triggers an un-resolved
-- notification.
noRenotifyMonitor :: MonitorOptions -> MonitorOptions
noRenotifyMonitor old = old { moRenotifyInterval = Nothing, moEscalationMessage = "" }

-- | Determine if the monitor triggers a notification when it is modified.
doesNotifyOnAudit :: MonitorOptions -> Bool
doesNotifyOnAudit = moNotifyAudit

-- | Have the monitor trigger a notification when it is modified.
notifyOnAudit :: MonitorOptions -> MonitorOptions
notifyOnAudit old = old { moNotifyAudit = True }

-- | Prevent the monitor from triggering a notification when it is modified.
noNotifyOnAudit :: MonitorOptions -> MonitorOptions
noNotifyOnAudit old = old { moNotifyAudit = False }


-- | A representation of a monitor's configuration, from which a monitor could
-- be rebuilt.
data MonitorSpec = MonitorSpec { msType :: MonitorType
                               , msQuery :: Text
                                 -- ^ The query string the monitor uses to
                                 -- determine its state.
                               , msName :: Maybe Text
                                 -- ^ The human-readable name of the monitor.
                               , msMessage :: Maybe Text
                                 -- ^ The message sent with the notification
                                 -- when the monitor is triggered.
                               , msOptions :: MonitorOptions
                                 -- ^ Optional configuration parameters
                                 -- specifying advanced monitor beahviour.
                               } deriving (Eq)

instance ToJSON MonitorSpec where
  toJSON ms = Object $ Data.HashMap.insert "options" (toJSON (msOptions ms)) hmap
    where (Object hmap) = object $
                          prependMaybe ("name" .=) (msName ms) $
                          prependMaybe ("message" .=) (msMessage ms)
                          ["type" .= pack (show (msType ms))
                          ,"query" .= msQuery ms
                          ]

instance FromJSON MonitorSpec where
  parseJSON (Object v) = modifyFailure ("MonitorSpec: " ++) $
                         MonitorSpec <$>
                         v .: "type" <*>
                         v .: "query" <*>
                         v .:? "name" .!= Nothing <*>
                         v .:? "message" .!= Nothing <*>
                         v .:? "options" .!= defaultMonitorOptions
  parseJSON a = modifyFailure ("MonitorSpec: " ++) $ typeMismatch "Object" a


-- | Creates the most basic specification required by a monitor, containing the
-- type of monitor and the query string used to detect the monitor's state.
--
-- This uses 'defaultMonitorOptions' to set the options (see that function for
-- disclaimer(s)).
minimalMonitorSpec :: MonitorType -> Text -> MonitorSpec
minimalMonitorSpec cmtype cmquery = MonitorSpec { msType = cmtype
                                                , msQuery = cmquery
                                                , msName = Nothing
                                                , msMessage = Nothing
                                                , msOptions = defaultMonitorOptions
                                                }


-- | Datadog's internal reference to a specific monitor.
type MonitorId = Int

-- | A Datadog monitor. These monitors actively check multiple different types
-- of data within Datadog against user-provided conditions, triggering
-- notifications when condition(s) are met.
data Monitor = Monitor { mId :: MonitorId
                         -- ^ Datadog's internal reference to this specific
                         -- monitor.
                       , mSpec :: MonitorSpec
                         -- ^ The specification from which this monitor can be
                         -- re-created.
                       } deriving (Eq)

instance ToJSON Monitor where
  toJSON monitor = Object $ Data.HashMap.insert "id" (toJSON (mId monitor)) basemap
    where (Object basemap) = toJSON (mSpec monitor)

instance FromJSON Monitor where
  parseJSON (Object v) = modifyFailure ("Monitor: " ++ ) $
                         Monitor <$> v .: "id" <*> parseJSON (Object v)
  parseJSON a = modifyFailure ("Monitor: " ++) $ typeMismatch "Object" a


-- | Create a new monitor in Datadog matching a specification.
createMonitor :: Environment -> MonitorSpec -> IO Monitor
createMonitor env monitorspec =
  let path = "monitor"
  in datadogHttp env path [] "POST" (Just $ encode monitorspec) >>=
     decodeDatadog "createMonitor"


-- | Load a monitor from Datadog by its ID.
loadMonitor :: Environment -> MonitorId -> IO Monitor
loadMonitor env monitorId =
  let path = "monitor/" ++ show monitorId
  in datadogHttp env path [] "GET" Nothing >>=
     decodeDatadog "loadMonitor"


-- | Sync a monitor with Datadog.
--
-- This must be called on any active monitors to apply the changes with Datadog
-- itself; merely modifying a monitor locally is not enough to store the
-- changes.
--
-- /Beware/: If a monitor has changed on the Datadog remote endpoint between
-- the time it was copied locally and when this function is called, those
-- changes already made remotely will be overwritten by this change.
updateMonitor :: Environment -> MonitorId -> MonitorSpec -> IO Monitor
updateMonitor env monitorId mspec =
  let path = "monitor/" ++ show monitorId
  in datadogHttp env path [] "PUT" (Just $ encode mspec) >>=
     decodeDatadog "updateMonitor"


-- | Delete a monitor from Datadog.
--
-- Note that once a monitor is deleted, it cannot be used locally anymore,
-- however you can always create a new monitor using the deleted monitor's
-- specification.
deleteMonitor :: Environment -> MonitorId -> IO ()
deleteMonitor env monitorId =
  let path = "monitor/" ++ show monitorId
  in void $ datadogHttp env path [] "DELETE" Nothing


-- | Load monitors from Datadog.
--
-- This function takes a filter list argument, which should contain any tags
-- the user wishes to filter on.  If the filter list is left empty, no filters
-- will be applied.  The list of tags is ANDed together; if you specify more
-- than one filter tag, only metrics which match all filter tags will be
-- provided.
loadMonitors :: Environment
             -> [Tag]
             -- ^ Tags on which to filter the results
             -> IO [Monitor]
loadMonitors env tags =
  let path = "monitor"
      query = [("tags", intercalate "," (map show tags)) | not (null tags)]
  in datadogHttp env path query "GET" Nothing >>=
     decodeDatadog "loadMonitors"


-- | Prevent all monitors from notifying indefinitely.
muteAllMonitors :: Environment -> IO ()
muteAllMonitors env =
  let path = "monitor/mute_all"
  in void $ datadogHttp env path [] "POST" Nothing


-- | Allow all monitors to notify.
unmuteAllMonitors :: Environment -> IO ()
unmuteAllMonitors env =
  let path = "monitor/unmute_all"
  in void $ datadogHttp env path [] "POST" Nothing
