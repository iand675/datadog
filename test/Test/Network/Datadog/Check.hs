{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.Check (tests) where


import Control.Concurrent (threadDelay)
import Control.Exception

import Distribution.TestSuite

import Network.Datadog (Environment, loadKeysFromEnv, createEnvironment)
import Network.Datadog.Check hiding (tags)

tests :: IO [Test]
tests = return
  [ Test TestInstance { run = testCheckRecord
                      , name = "Test recording of a status check"
                      , tags = ["Check"]
                      , options = []
                      , setOption = \_ _ -> Left ""
                      }
  ]


environment :: IO Environment
environment = createEnvironment =<< loadKeysFromEnv


testCheckRecord :: IO Progress
testCheckRecord = do
  env <- environment
  let check = CheckResult { checkResultCheck = "Datadog Test Check"
                          , checkResultHostName = "development"
                          , checkResultStatus = CheckOk
                          , checkResultTimestamp = Nothing
                          , checkResultMessage = Nothing
                          , checkResultTags = []
                          }
  let computation = const (Finished Pass) <$> const (recordCheck env check) <$> threadDelay 500000
  catch computation (\e -> return $ Finished $ Fail $ show (e :: SomeException))
