module Test.Network.Datadog (spec) where

import Test.Hspec (Spec, describe)

import qualified Test.Network.Datadog.Check as Check (spec)
import qualified Test.Network.Datadog.Downtime as Downtime (spec)
import qualified Test.Network.Datadog.Event as Event (spec)
import qualified Test.Network.Datadog.Host as Host (spec)
import qualified Test.Network.Datadog.Monitor as Monitor (spec)
import qualified Test.Network.Datadog.StatsD as StatsD (spec)

spec :: Spec
spec = describe "Datadog spec" $ do
  describe "StatsD spec" StatsD.spec
  describe "Check spec" Check.spec
  describe "Downtime spec" Downtime.spec
  describe "Event spec" Event.spec
  describe "Host spec" Host.spec
  describe "Monitor spec" Monitor.spec
