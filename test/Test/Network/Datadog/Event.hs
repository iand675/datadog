{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.Event (tests) where


import Control.Concurrent (threadDelay)
import Control.Exception

import Distribution.TestSuite

import Data.Time.Clock

import Network.Datadog
import Network.Datadog.Event


tests :: IO [Test]
tests = return [Test TestInstance { run = testEventCycle
                                  , name = "Test event creation and loading methods"
                                  , tags = ["Event"]
                                  , options = []
                                  , setOption = \_ _ -> Left ""
                                  }
               ]


environment :: IO Environment
environment = createEnvironment =<< loadKeysFromEnv


testEventCycle :: IO Progress
testEventCycle = do
  env <- environment
  time <- getCurrentTime
  let eventDetails = minimalEventSpec
                     "Datadog Test Event"
                     "This is a test for the Haskell Datadog API."
                     time
                     NormalPriority
  let computation = do
        threadDelay 500000
        event1 <- createEvent env eventDetails
        threadDelay 10000000
        event2 <- loadEvent env (eId event1)
        threadDelay 500000
        events <- loadEvents env (addUTCTime (-60) time, addUTCTime 60 time) Nothing []
        return (if event1 /= event2
                then Finished (Fail "Created and fetched events are not identical")
                else if event1 `notElem` events
                     then Finished (Fail "Created event not fetched from group load")
                     else Finished Pass
               )
  catch computation (\e -> return $ Finished $ Fail $ show (e :: SomeException))
