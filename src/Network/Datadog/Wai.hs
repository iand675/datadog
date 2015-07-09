{-# LANGUAGE OverloadedStrings #-}
module Datadog.Wai where
import Control.Concurrent
import Control.Lens
import Control.Monad (forever, void)
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Datadog.StatsD
import Network.Wai
import qualified Data.Vault.Lazy as V
import System.CPUTime
import System.IO.Unsafe

{-
datadogSeriesKey :: V.Key Series
datadogSeriesKey = unsafePerformIO V.newKey
{-# NOINLINE datadogSeriesKey #-}

datadogBatchStorage :: IORef Series
datadogBatchStorage = unsafePerformIO $ newIORef $ Series mempty
{-# NOINLINE datadogBatchStorage #-}

batchReporter :: DatadogCredentials k => DatadogClient k -> Int -> IO ThreadId
batchReporter c t = forkIO $ forever $ do
  s <- atomicModifyIORef datadogBatchStorage $ (,) mempty
  -- TODO: exception handling
  sendMetrics c s
  threadDelay (1000000 * t)

batchReporterMiddleware :: Middleware
batchReporterMiddleware app req respond = app req' $ \res -> do
  r <- respond res
  void $ forM (V.lookup datadogSeriesKey $ vault req') $ \s' ->
    atomicModifyIORef' datadogBatchStorage $ \s -> (s <> s', ())
  return r
  where
    req' = req { vault = V.insert datadogSeriesKey mempty $ vault req }

postRequestReporter :: DatadogCredentials k => DatadogClient k -> Middleware
postRequestReporter c app req respond = app req' $ \res -> do
  r <- respond res
  -- TODO: exception handling
  void $ forM (V.lookup datadogSeriesKey $ vault req') $ \s -> sendMetrics c s
  return r
  where
    req' = req { vault = V.insert datadogSeriesKey mempty $ vault req }
-}

dogStatsKey :: V.Key StatsClient
dogStatsKey = unsafePerformIO V.newKey
{-# NOINLINE dogStatsKey #-}

provideStatsClient :: StatsClient -> Middleware
provideStatsClient c app req = app req'
  where
    req' = req { vault = V.insert dogStatsKey c $ vault req }
{-# INLINEABLE provideStatsClient #-}

-- | Returns the stats client if request is wrapped by 'statsMiddleware'.
-- otherwise returns dummy client that ignores stat logging.
requestStatsClient :: Request -> StatsClient
requestStatsClient r = fromMaybe Dummy $ V.lookup dogStatsKey (vault r)

requestDuration :: MetricName
requestDuration = toMetricName ("request.duration" :: Text)

methodTag = tag "method" . T.decodeUtf8 . requestMethod
pathTag = tag "path" . T.decodeUtf8 . rawPathInfo
statusCodeTag = tag "status" . T.pack . show

instrumentRequests :: Middleware
instrumentRequests app req respond = do
  tStart <- getCPUTime
  app req $ \res -> do
    r <- respond res
    let client = requestStatsClient req
    tEnd <- getCPUTime
    case client of
      Dummy -> putStrLn "Dummy!"
      _ -> return ()
    let duration :: Word64
        duration = fromIntegral $ (tEnd - tStart) `div` 1000000000
        requestTags = [methodTag req, pathTag req, statusCodeTag req]
        m = metric requestDuration Timer duration & tags .~ requestTags
    send client m
    return r
