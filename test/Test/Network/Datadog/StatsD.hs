{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.StatsD (tests) where
import           Control.Exception
import           Data.Maybe
import           Distribution.TestSuite
import           Network.StatsD.Datadog hiding (name, tags)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString hiding (send)
import           System.Timeout

tests :: IO [Test]
tests = return
  [ Test TestInstance { run = testSend
                      , name = "Test sending DogStatsD data to a local server"
                      , tags = ["StatsD"]
                      , options = []
                      , setOption = \_ _ -> Left ""
                      }
  ]

makeServer :: IO Socket
makeServer = do
  (serverAddr:_) <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                                Nothing
                                (Just "8125")
  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
  bindSocket sock (addrAddress serverAddr)
  return sock

testSend :: IO Progress
testSend = bracket makeServer sClose $ \conn -> do
  withDogStatsD defaultSettings $ \stats -> do
    send stats $ event "foo" "bar"
  val <- timeout 10000000 $ recvFrom conn 2048
  return $ Finished $ if isJust val
                      then Pass
                      else Fail "Did not receive DogStatsD event"

