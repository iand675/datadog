module Test.Network.Datadog.Event (spec) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Test.Hspec (Spec, describe, expectationFailure, it)

import Network.Datadog (Environment, createEnvironment, loadKeysFromEnv)
import Network.Datadog.Event
  (EventPriority (NormalPriority), createEvent, id', loadEvent, loadEvents, minimalEventSpec)

spec :: Spec
spec = describe "Event spec" $ do
  it "Does event creation and loading methods" $ do
    let environment :: IO Environment
        environment = createEnvironment =<< loadKeysFromEnv
    env <- environment
    time <- getCurrentTime
    let testDetails = minimalEventSpec
          "Datadog Test Event"
          "This is a test for the Haskell Datadog API."
          time
          NormalPriority
    threadDelay 500000
    event1 <- createEvent env testDetails
    threadDelay 20000000
    event2 <- loadEvent env (event1 ^. id')
    threadDelay 500000
    events <- loadEvents env (addUTCTime (-60) time, addUTCTime 60 time) Nothing []
    when (event1 /= event2) $
      expectationFailure "Created and fetched events are not identical"
    when (event1 `notElem` events) $
      expectationFailure "Created event not fetched from group load"
