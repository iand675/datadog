module Test.Network.Datadog.Monitor (spec) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Control.Monad (when)
import Test.Hspec (Spec, describe, expectationFailure, it)

import Network.Datadog (Environment, createEnvironment, loadKeysFromEnv)
import Network.Datadog.Monitor
  ( MonitorType (MetricAlert)
  , createMonitor
  , deleteMonitor
  , id'
  , loadMonitor
  , loadMonitors
  , minimalMonitorSpec
  , monitorSpecName
  , updateMonitor
  )

spec :: Spec
spec = describe "Monitor spec" $ do
  it "Does monitor CRUD operations" $ do
    let environment :: IO Environment
        environment = createEnvironment =<< loadKeysFromEnv
        monitorDetails = minimalMonitorSpec
          MetricAlert
          "avg(last_5m):sum:system.net.bytes_rcvd{host:host0} > 100"
        monitorUpdatedDetails = monitorDetails { monitorSpecName = Just "Haskell Datadog test monitor" }
    env <- environment
    threadDelay 500000
    monitor1 <- createMonitor env monitorDetails
    threadDelay 500000
    monitor2 <- updateMonitor env (monitor1 ^. id') monitorUpdatedDetails
    threadDelay 500000
    monitor3 <- loadMonitor env (monitor1 ^. id')
    threadDelay 500000
    monitors <- loadMonitors env []
    threadDelay 500000
    deleteMonitor env (monitor1 ^. id')
    when (monitor2 /= monitor3) $
      expectationFailure "Updated and fetched monitors are not identical"
    when (monitor2 `notElem` monitors) $
      expectationFailure "Created monitor not updated (in group load)"
