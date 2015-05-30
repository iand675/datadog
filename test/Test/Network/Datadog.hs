module Test.Network.Datadog (tests) where


import Distribution.TestSuite

import qualified Test.Network.Datadog.StatsD as StatsD (tests)
import qualified Test.Network.Datadog.Check as Check (tests)
import qualified Test.Network.Datadog.Downtime as Downtime (tests)
import qualified Test.Network.Datadog.Event as Event (tests)
import qualified Test.Network.Datadog.Monitor as Monitor (tests)
import qualified Test.Network.Datadog.Host as Host (tests)


tests :: IO [Test]
tests = map (\(s,t) -> Group s False t)
        <$> mapM (\(s,tm) -> fmap (\t -> (s,t)) tm)
        [("StatsD", StatsD.tests)]
        -- ,("Check", Check.tests)
        -- ,("Downtime", Downtime.tests)
        -- ,("Event", Event.tests)
        -- ,("Host", Host.tests)
        -- ,("Monitor", Monitor.tests)
        -- ]
