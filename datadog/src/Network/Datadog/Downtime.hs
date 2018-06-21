{-# LANGUAGE OverloadedStrings #-}

{-|
Downtime prevents all alerting related to specific Datadog scopes.
-}
module Network.Datadog.Downtime
( DowntimeSpec(..)
, Downtime
, minimalDowntimeSpec
, scheduleDowntime
, updateDowntime
, cancelDowntime
, loadDowntime
, loadDowntimes
, HasScope(..)
, HasSpec(..)
, HasMessage(..)
, HasStart(..)
, HasEnd(..)
, HasId'(..)
, DowntimeId
) where


import Control.Monad (void)
import Data.Aeson
import Network.HTTP.Types
import Network.Datadog.Internal

-- | Creates the most basic possible downtime specification, which just
-- contains the scope to which the downtime applies.
minimalDowntimeSpec :: Tag -> DowntimeSpec
minimalDowntimeSpec = DowntimeSpec Nothing Nothing Nothing

-- | Schedule a new downtime in Datadog.
scheduleDowntime :: Environment -> DowntimeSpec -> IO Downtime
scheduleDowntime env dspec =
  let path = "downtime"
  in datadogHttp env path [] POST (Just $ encode dspec) >>=
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
      q = [("current_only", "true") | active]
  in datadogHttp env path q GET Nothing >>=
     decodeDatadog "loadDowntimes"
