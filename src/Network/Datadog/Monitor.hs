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
, MonitorOptions(..)
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
, Monitor(..)
, MonitorId
, minimalMonitorSpec
, createMonitor
, updateMonitor
, deleteMonitor
, loadMonitor
, loadMonitors
, muteAllMonitors
, unmuteAllMonitors
, HasId'(..)
, HasSpec(..)
, HasType'(..)
, HasQuery(..)
, HasOptions(..)
, HasMessage(..)
, HasName(..)
, HasNotifyNoData(..)
, HasTimeoutH(..)
, HasRenotifyInterval(..)
, HasNoDataTimeframe(..)
, HasSilenced(..)
, AsMonitorType(..)
) where

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad (void)

import Data.Aeson
import qualified Data.HashMap.Strict as Data.HashMap
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Text as T (Text, null)
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Network.HTTP.Types

import Network.Datadog.Internal

-- | Provide a list of the silenced scopes for this monitor and the time at
-- which the silencer will expire (may be indefinite). The monitor @ "*" @
-- refers to the monitor at large (un-scoped).
getSilencedMonitorScopes :: MonitorOptions -> [(T.Text, Maybe UTCTime)]
getSilencedMonitorScopes opts = map (second (fmap (posixSecondsToUTCTime . fromIntegral))) $ Data.HashMap.toList $ opts ^. silenced

-- | Silence a given scope until some time (or indefinitely), replacing the
-- current silencer on the given scope if one already exists.
silenceMonitorScope :: T.Text -> Maybe UTCTime -> MonitorOptions -> MonitorOptions
silenceMonitorScope scopeName mtime old = old & silenced %~ silenceF
  where silenceF = Data.HashMap.insert scopeName (fmap (floor . utcTimeToPOSIXSeconds) mtime)

-- | Remove the silencer from a given scope, if the scope is currently
-- silenced.
unsilenceMonitorScope :: T.Text -> MonitorOptions -> MonitorOptions
unsilenceMonitorScope scopeName old = old & silenced %~ (Data.HashMap.delete scopeName)

-- | Unsilence every scope in the monitor, including the global scope.
unsilenceAllMonitorScope :: MonitorOptions -> MonitorOptions
unsilenceAllMonitorScope = set silenced Data.HashMap.empty

-- | Determine how long without data a monitor will go before notifying to
-- such, providing Nothing if the monitor will never notify on lack of data.
doesNotifyOnNoMonitorData :: MonitorOptions -> Maybe NominalDiffTime
doesNotifyOnNoMonitorData opts = if opts ^. notifyNoData
                                   then Just $ maybe (throw (AssertionFailed "Datadog Library internal error")) (fromIntegral . (*60)) $ opts ^. noDataTimeframe
                                   else Nothing

-- | Have the monitor notify when it does not receive data for some given
-- amount of time (rounded down to the nearest minute).
notifyOnNoMonitorData :: NominalDiffTime -> MonitorOptions -> MonitorOptions
notifyOnNoMonitorData difftime old = old & notifyNoData .~ True & noDataTimeframe ?~ stamp
  where stamp = floor (difftime / 60)

-- | Prevent the monitor from notifying when it is missing data.
noNotifyOnNoMonitorData :: MonitorOptions -> MonitorOptions
noNotifyOnNoMonitorData old = old & notifyNoData .~ False & noDataTimeframe .~ Nothing

-- | Determine after how long the monitor will stop alerting after it is
-- triggered, providing Nothing if the monitor will never stop alerting.
getMonitorTimeout :: MonitorOptions -> Maybe NominalDiffTime
getMonitorTimeout opts = (fromIntegral . (*3600)) <$> (opts ^. timeoutH)

-- | Have the monitor stop alerting some time after it is triggered (rounded up
-- to the nearest hour).
setMonitorTimeout :: NominalDiffTime -> MonitorOptions -> MonitorOptions
setMonitorTimeout difftime old = old & timeoutH ?~ stamp
  where stamp = ceiling (difftime / 3600)

-- | Prevent the monitor from timing out after it is triggered.
clearMonitorTimeout :: MonitorOptions -> MonitorOptions
clearMonitorTimeout = set timeoutH Nothing

-- | Determine after how long after being triggered the monitor will re-notify,
-- and what message it will include in the re-notification (if any), providing
-- Nothing if the monitor will never re-notify.
doesRenotifyMonitor :: MonitorOptions -> Maybe (NominalDiffTime,Maybe T.Text)
doesRenotifyMonitor opts = (\i -> (fromIntegral (i * 60), result)) <$> (opts ^. renotifyInterval)
  where msg = opts ^. escalationMessage
        result = if T.null msg then Nothing else Just msg

-- | Have the monitor re-notify some amount of time after the most recent
-- notification (rounded down to the nearest minute) and optionally what text
-- it will include in the re-notification.
renotifyMonitor :: NominalDiffTime -> Maybe T.Text -> MonitorOptions -> MonitorOptions
renotifyMonitor difftime mMessage old = old & renotifyInterval ?~ stamp & escalationMessage .~ msg
  where stamp = floor (difftime / 60)
        msg = fromMaybe "" mMessage

-- | Prevent the monitor from re-notifying after it triggers an un-resolved
-- notification.
noRenotifyMonitor :: MonitorOptions -> MonitorOptions
noRenotifyMonitor old = old & renotifyInterval .~ Nothing & escalationMessage .~ ""

-- | Determine if the monitor triggers a notification when it is modified.
doesNotifyOnAudit :: MonitorOptions -> Bool
doesNotifyOnAudit = view notifyAudit

-- | Have the monitor trigger a notification when it is modified.
notifyOnAudit :: MonitorOptions -> MonitorOptions
notifyOnAudit = set notifyAudit True

-- | Prevent the monitor from triggering a notification when it is modified.
noNotifyOnAudit :: MonitorOptions -> MonitorOptions
noNotifyOnAudit = set notifyAudit False


-- | Creates the most basic specification required by a monitor, containing the
-- type of monitor and the query string used to detect the monitor's state.
--
-- This uses 'defaultMonitorOptions' to set the options (see that function for
-- disclaimer(s)).
minimalMonitorSpec :: MonitorType -> T.Text -> MonitorSpec
minimalMonitorSpec cmtype cmquery = MonitorSpec { monitorSpecType' = cmtype
                                                , monitorSpecQuery = cmquery
                                                , monitorSpecName = Nothing
                                                , monitorSpecMessage = Nothing
                                                , monitorSpecOptions = defaultMonitorOptions
                                                }

-- | Create a new monitor in Datadog matching a specification.
createMonitor :: Environment -> MonitorSpec -> IO Monitor
createMonitor env monitorspec =
  let path = "monitor"
  in datadogHttp env path [] POST (Just $ encode monitorspec) >>=
     decodeDatadog "createMonitor"


-- | Load a monitor from Datadog by its ID.
loadMonitor :: Environment -> MonitorId -> IO Monitor
loadMonitor env monitorId =
  let path = "monitor/" ++ show monitorId
  in datadogHttp env path [] GET Nothing >>=
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
  in datadogHttp env path [] PUT (Just $ encode mspec) >>=
     decodeDatadog "updateMonitor"


-- | Delete a monitor from Datadog.
--
-- Note that once a monitor is deleted, it cannot be used locally anymore,
-- however you can always create a new monitor using the deleted monitor's
-- specification.
deleteMonitor :: Environment -> MonitorId -> IO ()
deleteMonitor env monitorId =
  let path = "monitor/" ++ show monitorId
  in void $ datadogHttp env path [] DELETE Nothing


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
loadMonitors env filterTags =
  let path = "monitor"
      q = [("tags", intercalate "," (map show filterTags)) | not (null filterTags)]
  in datadogHttp env path q GET Nothing >>=
     decodeDatadog "loadMonitors"


-- | Prevent all monitors from notifying indefinitely.
muteAllMonitors :: Environment -> IO ()
muteAllMonitors env =
  let path = "monitor/mute_all"
  in void $ datadogHttp env path [] POST Nothing


-- | Allow all monitors to notify.
unmuteAllMonitors :: Environment -> IO ()
unmuteAllMonitors env =
  let path = "monitor/unmute_all"
  in void $ datadogHttp env path [] POST Nothing

