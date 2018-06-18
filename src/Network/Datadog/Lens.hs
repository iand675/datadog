{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE FunctionalDependencies #-}
module Network.Datadog.Lens where

import Control.Lens.TH (makeClassyPrisms, makeFields)

import Network.Datadog.Types

makeFields ''CheckResult
makeFields ''DowntimeSpec
makeFields ''Downtime
makeFields ''EventSpec
makeFields ''Event
makeFields ''Metric
makeFields ''MonitorOptions
makeFields ''MonitorSpec
makeFields ''Monitor
makeFields ''NewUser
makeFields ''DatadogUser
makeClassyPrisms ''Tag
makeClassyPrisms ''CheckStatus
makeClassyPrisms ''EventPriority
makeClassyPrisms ''AlertType
makeClassyPrisms ''SourceType
makeClassyPrisms ''MetricPoints
makeClassyPrisms ''MonitorType
