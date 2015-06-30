{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.Monitor (tests) where


import Control.Concurrent (threadDelay)
import Control.Exception

import Distribution.TestSuite

import Network.Datadog
import Network.Datadog.Monitor


tests :: IO [Test]
tests = return [Test TestInstance { run = testMonitorCycle
                                  , name = "Test monitor CRUD"
                                  , tags = ["Monitor"]
                                  , options = []
                                  , setOption = \_ _ -> Left ""
                                  }
               ]


environment :: IO Environment
environment = createEnvironment =<< loadKeysFromEnv


testMonitorCycle :: IO Progress
testMonitorCycle = do
  env <- environment
  let monitorDetails = minimalMonitorSpec
                       MetricAlert
                       "avg(last_5m):sum:system.net.bytes_rcvd{host:host0} > 100"
  let monitorUpdatedDetails = monitorDetails { msName = Just "Haskell Datadog test monitor" }
  let computation = do
        threadDelay 500000
        monitor1 <- createMonitor env monitorDetails
        threadDelay 500000
        monitor2 <- updateMonitor env (mId monitor1) monitorUpdatedDetails
        threadDelay 500000
        monitor3 <- loadMonitor env (mId monitor1)
        threadDelay 500000
        monitors <- loadMonitors env []
        threadDelay 500000
        deleteMonitor env (mId monitor1)
        return (if monitor2 /= monitor3
                then Finished (Fail "Updated and fetched monitors are not identical")
                else if monitor2 `notElem` monitors
                     then Finished (Fail (if monitor1 `elem` monitors
                                          then "Created monitor not updated (in group load)"
                                          else "Updated monitor not fetched from group load"))
                     else Finished Pass
               )
  catch computation (\e -> return $ Finished $ Fail $ show (e :: SomeException))
