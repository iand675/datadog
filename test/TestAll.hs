module Main where
import           Control.Exception
import           Data.Maybe
import           Network.Datadog.StatsD
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString hiding (send)
import           System.Timeout
import           Test.Tasty
import           Test.Tasty.HUnit

main = defaultMain $ testGroup "Tests"
  [ testCase "send" testSend
  ]

makeServer :: IO Socket
makeServer = do
  (serverAddr:_) <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                                Nothing
                                (Just "8125")
  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
  bindSocket sock (addrAddress serverAddr)
  return sock

testSend = bracket makeServer sClose $ \conn -> do
  withDogStatsD defaultSettings $ \stats -> do
    send stats $ event "foo" "bar"
  val <- timeout 10000000 $ recvFrom conn 2048
  assertBool "Received event" $ isJust val

