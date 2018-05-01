{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Wai.Middleware.Datadog
    ( datadogMiddleware
    -- , rtsStatReporter
    , statsTagsKey
    , statsClientKey
    , apmMiddleware
    , traceGenerator
    , getTraceState
    ) where
import Control.Concurrent
import Control.Lens ((&), (.~))
import Control.Monad
import Data.Int
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import Data.IP
import Data.Vault.Lazy
import GHC.Stats
import Network.Datadog.APM
import Network.HTTP.Types.Status
import Network.Socket (SockAddr(..), inet_ntoa)
import Network.StatsD.Datadog
import Network.Wai
import Network.Wai.Internal
import System.Clock
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Types (ProcessID)
import System.Random.MWC

traceGenerator :: GenIO
traceGenerator = unsafePerformIO createSystemRandom
{-# NOINLINE traceGenerator #-}

traceStateKey :: Key TraceState
traceStateKey = unsafePerformIO newKey
{-# NOINLINE traceStateKey #-}

statsTagsKey :: Key (IORef [Tag])
statsTagsKey = unsafePerformIO newKey
{-# NOINLINE statsTagsKey #-}

statsClientKey :: Key StatsClient
statsClientKey = unsafePerformIO newKey
{-# NOINLINE statsClientKey #-}

unsafeProcessId :: ProcessID
unsafeProcessId = unsafePerformIO getProcessID
{-# NOINLINE unsafeProcessId #-}

responseTime :: MetricName
responseTime = MetricName "warp.requests.duration"

getTraceState :: Request -> Maybe TraceState
getTraceState = Data.Vault.Lazy.lookup traceStateKey . vault

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

{-
rtsStatReporter :: StatsClient -> [Tag] -> IO ()
rtsStatReporter client ts = do
  enabled <- getRTSStatsEnabled
  when enabled $ void $ forkIO $ do
    stats <- getRTSStats
    go stats
  where
  scg :: ToMetricValue a => Text -> MetricType -> a -> IO ()
  scg n t v = send client (metric (MetricName n) t v)
  toInt :: Int64 -> Int
  toInt = fromIntegral
  go _ = do
    newStats@RTSStats{..} <- getRTSStats
    let GCDetails{..} = gc
    scg "rts.gc.bytes_allocated" Gauge gcdetails_allocated_bytes
    scg "rts.gc.num_gcs" Gauge gcs
    scg "rts.gc.num_bytes_usage_samples" Gauge numByteUsageSamples
    scg "rts.gc.cumulative_bytes_used" Gauge cumulativeBytesUsed
    scg "rts.gc.max_bytes_used" Gauge maxBytesUsed
    scg "rts.gc.bytes_copied" Gauge bytesCopied
    scg "rts.gc.mutator_cpu_ms" Gauge mutatorCpuSeconds
    scg "rts.gc.mutator_wall_ms" Gauge mutatorWallSeconds
    scg "rts.gc.gc_cpu_ms" Gauge gcCpuSeconds
    scg "rts.gc.gc_wall_ms" Gauge gcWallSeconds
    scg "rts.gc.cpu_ms" Gauge cpuSeconds
    scg "rts.gc.wall_ms" Gauge wallSeconds
    scg "rts.gc.current_bytes_used" Gauge currentBytesUsed
    scg "rts.gc.current_bytes_slop" Gauge currentBytesSlop
-}

apmMiddleware :: APMClient -> Context -> Middleware
apmMiddleware client ctxt app req respond = do
  threadId <- myThreadId
  tid <- TraceId <$> uniform traceGenerator
  mt <- createMutableTrace tid traceGenerator >>= \st -> createMutableTraceSpan st ctxt
  let st = case mt of
             Network.Datadog.APM.Dummy -> error "Should be impossible"
             (MTrace st) -> st
  let req' = req { vault = insert traceStateKey st $ vault req }
  app req' $ \resp -> do
    let traceFinisher status respHeaders = do
          let code = statusCode status
              otherMeta = HM.fromList
                [ ("system.pid", T.pack $ show unsafeProcessId)
                , ("thread.id", T.pack $ show threadId)
                ]
              netMeta = HM.fromList $ case remoteHost req of
                SockAddrInet p addr ->
                  [ ("out.host", T.pack $ show $ fromHostAddress addr)
                  , ("out.port", T.pack $ show p)
                  ]
                SockAddrInet6 p _ addr _ ->
                  [ ("out.host", T.pack $ show $ fromHostAddress6 addr)
                  , ("out.port", T.pack $ show p)
                  ]
                SockAddrUnix str ->
                  [ ("out.socket", T.pack str)
                  ]
                SockAddrCan p ->
                  [ ("out.can", T.pack $ show p)
                  ]
              httpMeta = HM.fromList
                [ ("http.method", T.decodeUtf8 $ requestMethod req)
                , ("http.status_code", T.pack $ show $ statusCode status)
                , ("http.url", T.decodeUtf8 $ rawPathInfo req)
                ]

          when (code >= 500 && code < 599) $ markTraceAsError mt
          tagTrace mt (httpMeta `HM.union` netMeta `HM.union` otherMeta)
          mt' <- completeMutableTraceSpan mt
          t <- completeMutableTrace mt'
          sendTrace client t
    r <- respond resp
    case resp of
      ResponseRaw{} -> do
        mt' <- completeMutableTraceSpan mt
        t <- completeMutableTrace mt'
        sendTrace client t
      ResponseFile status respHeaders _ _ -> traceFinisher status respHeaders
      ResponseBuilder status respHeaders _ -> traceFinisher status respHeaders
      ResponseStream status respHeaders _ -> traceFinisher status respHeaders
    return r

datadogMiddleware :: [Tag] -> StatsClient -> Middleware
datadogMiddleware defaultTags client app req responder = do
  tsRef <- newIORef defaultTags
  let vault' = insert statsTagsKey tsRef $ insert statsClientKey client $ vault req
      req' = req { vault = vault' }
  startTime <- getTime Monotonic
  app req' $ \resp -> do
    r <- responder resp
    let traceFinisher = do
          endTime <- getTime Monotonic
          let millis = case (endTime - startTime) of
                TimeSpec{..} -> fromIntegral ((sec * 1000) + round (fromIntegral nsec / 1000000)) :: Int
          ts <- readIORef tsRef
          send client (metric responseTime Timer millis & tags .~ ts)
    case resp of
      ResponseRaw{} -> return ()
      _ -> traceFinisher
    return r
