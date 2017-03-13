{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Datadog
    ( datadogMiddleware
    , rtsStatReporter
    , statsTagsKey
    , statsClientKey
    ) where
import Control.Concurrent
import Control.Lens ((&), (.~))
import Control.Monad
import Data.Int
import Data.IORef
import Data.Text (Text)
import Data.Vault.Lazy
import GHC.Stats
import Network.StatsD.Datadog
import Network.Wai
import Network.Wai.Internal
import System.Clock
import System.IO.Unsafe

statsTagsKey :: Key (IORef [Tag])
statsTagsKey = unsafePerformIO newKey
{-# NOINLINE statsTagsKey #-}

statsClientKey :: Key StatsClient
statsClientKey = unsafePerformIO newKey
{-# NOINLINE statsClientKey #-}

responseTime :: MetricName
responseTime = MetricName "warp.requests.duration"

-- warp.net.writing
-- warp.net.waiting
-- warp.net.reading
-- warp.net.connections
-- warp.net.request_per_s
-- warp.net.conn_opened_per_s
-- warp.net.conn_dropped_per_s
-- warp.connections.accepted
-- warp.connections.active
-- warp.connections.dropped
-- warp.connections.idle
-- warp.requests.current
-- warp.requests.total
-- warp.requests.duration
-- warp.responses.1xx
-- warp.responses.2xx
-- warp.responses.3xx
-- warp.responses.4xx
-- warp.responses.5xx
-- warp.responses.total
-- warp.sent
-- warp.ssl.handshakes
-- warp.ssl.handshakes_failed
-- warp.ssl.session_reuses

rtsStatReporter :: StatsClient -> [Tag] -> IO ()
rtsStatReporter client ts = do
  enabled <- getGCStatsEnabled
  when enabled $ void $ forkIO $ do
    stats <- getGCStats
    go stats
  where
  scg :: ToMetricValue a => Text -> MetricType -> a -> IO ()
  scg n t v = send client (metric (MetricName n) t v)
  toInt :: Int64 -> Int
  toInt = fromIntegral
  go _ = do
    newStats@GCStats{..} <- getGCStats
    scg "rts.gc.bytes_allocated" Gauge $ toInt bytesAllocated
    scg "rts.gc.num_gcs" Gauge $ toInt numGcs
    scg "rts.gc.num_bytes_usage_samples" Gauge $ toInt numByteUsageSamples
    scg "rts.gc.cumulative_bytes_used" Gauge $ toInt cumulativeBytesUsed
    scg "rts.gc.max_bytes_used" Gauge $ toInt maxBytesUsed
    scg "rts.gc.bytes_copied" Gauge $ toInt bytesCopied
    scg "rts.gc.mutator_cpu_ms" Gauge mutatorCpuSeconds
    scg "rts.gc.mutator_wall_ms" Gauge mutatorWallSeconds
    scg "rts.gc.gc_cpu_ms" Gauge gcCpuSeconds
    scg "rts.gc.gc_wall_ms" Gauge gcWallSeconds
    scg "rts.gc.cpu_ms" Gauge cpuSeconds
    scg "rts.gc.wall_ms" Gauge wallSeconds
    scg "rts.gc.current_bytes_used" Gauge $ toInt currentBytesUsed
    scg "rts.gc.current_bytes_slop" Gauge $ toInt currentBytesSlop

datadogMiddleware :: Clock -> [Tag] -> StatsClient -> Middleware
datadogMiddleware clock defaultTags client app req responder = do
  tsRef <- newIORef defaultTags
  let vault' = insert statsTagsKey tsRef $ insert statsClientKey client $ vault req
      req' = req { vault = vault' }
  startTime <- getTime clock
  app req' $ \resp -> do
    r <- responder resp
    case resp of
      ResponseRaw{} -> return ()
      _ -> do
        endTime <- getTime clock
        let millis = case (endTime - startTime) of
              TimeSpec{..} -> fromIntegral ((sec * 1000) + (nsec `div` 1000000)) :: Int
        ts <- readIORef tsRef
        send client (metric responseTime Timer millis & tags .~ ts)
    return r
