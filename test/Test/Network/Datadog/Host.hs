{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.Host (tests) where


import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Time.Clock
import Distribution.TestSuite
import System.Random (randomIO)

import Network.Datadog (Environment, createEnvironment, loadKeysFromEnv)
import Network.Datadog.Host


tests :: IO [Test]
tests = return
  [ Test TestInstance { run = testHostMutes
                      , name = "Test host muting and unmuting"
                      , tags = ["Host"]
                      , options = []
                      , setOption = \_ _ -> Left ""
                      }
  ]


environment :: IO Environment
environment = createEnvironment =<< loadKeysFromEnv

-- FIXME host should be randomized
performMute :: Text -> Maybe UTCTime -> Bool -> IO ()
performMute host time override = do
  env <- environment
  threadDelay 1000000
  muteHost env host time override


performUnmute :: Text -> IO ()
performUnmute host = do
  env <- environment
  threadDelay 1000000
  unmuteHost env host

testHostMutes :: IO Progress
testHostMutes = do
  time <- getCurrentTime
  let computation = do
        host <- ("haskell-datadog-test-host-" <>) . pack . show <$> (randomIO :: IO Word)
        -- Ensure overriding works
        performMute host Nothing True
        performMute host (Just (addUTCTime 30 time)) True
        -- Unmute the host before we test no override option
        performUnmute host
        performMute host Nothing False
        -- Unmute for "cleanup"
        performUnmute host
        return $ Finished Pass
  catch computation (\e -> return $ Finished $ Fail $ show (e :: SomeException))
