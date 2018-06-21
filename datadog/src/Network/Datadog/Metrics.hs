{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Datadog.Metrics
  ( Series (..)
  , Metric (..)
  , MetricPoints (..)
  , sendMetrics
  , series
  , HasName(..)
  , HasPoints(..)
  , HasTags(..)
  , HasHost(..)
  , AsMetricPoints(..)
  ) where
import Control.Monad (void)
import Data.Aeson hiding (Series)
import Data.DList
import qualified Network.HTTP.Types  as HTTP
import qualified Network.HTTP.Client as C
import Network.Datadog.Internal

series :: [Metric] -> Series
series = Series . fromList

metricRequest :: C.Request
metricRequest = baseRequest { C.method = HTTP.methodPost
                            , C.path = "/api/v1/series"
                            , C.requestHeaders = [("Content-Type", "application/json")]
                            }

sendMetrics :: DatadogCredentials k => DatadogClient k -> Series -> IO ()
sendMetrics m s = void $ C.httpNoBody req $ datadogClientManager m
  where
    req = (signRequest (datadogClientKeys m) metricRequest) { C.requestBody = C.RequestBodyLBS $ encode s }

-- | Wall clock time
{-
withTimingMetric :: DatadogCredentials k => DatadogClient k
                 -> (Metric -> IO ())
                 -> Text        -- ^ metric
                 -> Maybe Text  -- ^ hostname
                 -> [Text]      -- ^ tags
                 -> IO a
                 -> IO a
withTimingMetric mAction m h ts action = do
  t <- getCurrentTime
  r <- action
  t' <- getCurrentTime
  mAction $ Metric m [Counter diff]diffUTCTime t' t
-}
