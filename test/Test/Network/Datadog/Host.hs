{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.Host (tests) where


import Control.Concurrent (threadDelay)
import Control.Exception

import Distribution.TestSuite

import Data.Time.Clock

import Network.Datadog
import Network.Datadog.Host


tests :: IO [Test]
tests = return [Test TestInstance { run = testHostMutes
                                  , name = "Test host muting and unmuting"
                                  , tags = ["Host"]
                                  , options = []
                                  , setOption = \_ _ -> Left ""
                                  }
               ]


environment :: IO Environment
environment = createEnvironment =<< loadKeysFromEnv


performMute :: Maybe UTCTime -> Bool -> IO ()
performMute time override = do
  env <- environment
  threadDelay 1000000
  muteHost env "haskell-datadog-test-host" time override


performUnmute :: IO ()
performUnmute = do
  env <- environment
  threadDelay 1000000
  unmuteHost env "haskell-datadog-test-host"


testHostMutes :: IO Progress
testHostMutes = do
  time <- getCurrentTime
  let computation = do
        -- Ensure overriding works
        performMute Nothing True
        performMute (Just (addUTCTime 30 time)) True
        -- Unmute the host before we test no override option
        performUnmute
        performMute Nothing False
        -- Unmute for "cleanup"
        performUnmute
        return $ Finished Pass
  catch computation (\e -> return $ Finished $ Fail $ show (e :: SomeException))
