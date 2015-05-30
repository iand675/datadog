{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.Downtime (tests) where


import Control.Concurrent (threadDelay)
import Control.Exception

import Distribution.TestSuite

import Data.Time.Clock

import Network.Datadog
import Network.Datadog.Downtime


tests :: IO [Test]
tests = return [Test TestInstance { run = testDowntimeCycle
                                  , name = "Test downtime CRUD"
                                  , tags = ["Downtime"]
                                  , options = []
                                  , setOption = \_ _ -> Left ""
                                  }
               ]


environment :: IO Environment
environment = createEnvironment =<< loadKeysFromEnv


testDowntimeCycle :: IO Progress
testDowntimeCycle = do
  env <- environment
  time <- getCurrentTime
  let downtimeDetails = minimalDowntimeSpec
                        "haskell-datadog-test-scope"
  let downtimeUpdatedDetails = downtimeDetails { dsEnd = Just (addUTCTime 300 time) }
  let computation = do
        threadDelay 500000
        downtime1 <- scheduleDowntime env downtimeDetails
        threadDelay 500000
        downtime2 <- updateDowntime env (dId downtime1) downtimeUpdatedDetails
        threadDelay 500000
        downtime3 <- loadDowntime env (dId downtime1)
        threadDelay 500000
        downtimes <- loadDowntimes env False
        threadDelay 500000
        cancelDowntime env (dId downtime1)
        return (if downtime2 /= downtime3
                then Finished (Fail "Updated and fetched downtimes are not identical")
                else if downtime2 `notElem` downtimes
                     then Finished (Fail (if downtime1 `elem` downtimes
                                          then "Created downtime not updated (in group load)"
                                          else "Updated downtime not fetched from group load"))
                     else Finished Pass
               )
  catch computation (\e -> return $ Finished $ Fail $ show (e :: SomeException))
