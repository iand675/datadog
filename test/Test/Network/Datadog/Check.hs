module Test.Network.Datadog.Check (spec) where

import Test.Hspec (Spec, describe, it)

import Network.Datadog (Environment, createEnvironment, loadKeysFromEnv)
import Network.Datadog.Check
  ( CheckResult (CheckResult)
  , CheckStatus (CheckOk)
  , checkResultCheck
  , checkResultHostName
  , checkResultMessage
  , checkResultStatus
  , checkResultTags
  , checkResultTimestamp
  , recordCheck
  )

spec :: Spec
spec = describe "Check spec" $ do
  it "Records a status check" $ do
    let environment :: IO Environment
        environment = createEnvironment =<< loadKeysFromEnv
        check = CheckResult
          { checkResultCheck = "Datadog Test Check"
          , checkResultHostName = "development"
          , checkResultStatus = CheckOk
          , checkResultTimestamp = Nothing
          , checkResultMessage = Nothing
          , checkResultTags = []
          }
    env <- environment
    recordCheck env check
