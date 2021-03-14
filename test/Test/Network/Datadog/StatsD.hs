module Test.Network.Datadog.StatsD (spec) where

import Control.Monad.Catch (SomeException, bracket, try, displayException)
import Control.Concurrent (forkFinally, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Network.Socket
  ( AddrInfoFlag (AI_PASSIVE)
  , Socket
  , SocketType (Datagram)
  , addrAddress
  , addrFamily
  , addrFlags
  , bind
  , close
  , defaultHints
  , defaultProtocol
  , getAddrInfo
  , socket
  )
import Network.Socket.ByteString (recvFrom)
import Control.Monad
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it)
import qualified Data.Text as Text

import Network.StatsD.Datadog (MetricLargerThanBufferSizeException, defaultSettings, event, send, withDogStatsD, dogStatsSettingsMaxDelay)

spec :: Spec
spec = describe "StatsD spec" $ do
  it "Sends DogStatsD data to a local server" $ do
    let makeServer :: IO Socket
        makeServer = do
          (serverAddr:_) <- getAddrInfo (Just defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just "8125")
          sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
          bind sock (addrAddress serverAddr)
          return sock
    bracket makeServer close $ \conn -> do
      withDogStatsD defaultSettings $ \stats -> do
        send stats $ event "foo" "bar"
      val <- timeout 10000000 $ recvFrom conn 2048
      case val of
        Just _  -> pure ()
        Nothing -> expectationFailure "Did not receive DogStatsD event"
  it "Can handle more metrics than fit in a single UDP packet without erroring" $ do
    let makeServer :: IO Socket
        makeServer = do
          (serverAddr:_) <- getAddrInfo (Just defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just "8125")
          sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
          bind sock (addrAddress serverAddr)
          return sock
    bracket makeServer close $ \conn -> do
      withDogStatsD defaultSettings $ \stats -> do
        replicateM 500000 $ send stats $ event "foo" "bar"
      val <- timeout 50000000 $ recvFrom conn 2048
      case val of
        Just _  -> pure ()
        Nothing -> expectationFailure "Did not receive DogStatsD event"
  it "does not go into an infinite loop when trying sending a metric larger than a UPD packet" $ do
    let longText = Text.replicate 65507 "x"
        sendLargeMetric =
          withDogStatsD defaultSettings $ \stats -> do
            try $ send stats $ event "foo" longText

    threadFinishedVar <- newEmptyMVar

    -- run the `withDogStatsD` computation on another thread so we can successfully time out the
    -- test. The infinite loop caused by large packets was not interruptible by asynchronous
    -- exceptions. Most likely did not allocate any memory, so there was no safe spot to raise
    -- the async exception.
    threadId <- forkFinally sendLargeMetric (putMVar threadFinishedVar)
    threadResult <- timeout 10000000 $ takeMVar threadFinishedVar

    -- timeout the killThread call because it will end up waiting indefinitely trying to deliver
    -- the asynchronous exception if the infinite loop is triggered.
    _ <- timeout 100000 $ killThread threadId

    case threadResult of
      -- The thread finished and try caught the expected exception.
      Just (Right (Left (_ :: MetricLargerThanBufferSizeException))) ->
        pure ()

      -- The thread finished with *no* exception, which is unexpected.
      Just (Right (Right _)) ->
        expectationFailure "Expected a MetricLargerThanBufferSizeException, but no exception was thrown."

      -- The thread finished with an exception that was not caught by the `try` above.
      Just (Left err) ->
        expectationFailure $ "Expected a MetricLargerThanBufferSizeException to be thrown by send, but got: " ++ show err

      -- The takeMVar above timed out, indicating the thread didn't finish.
      Nothing ->
        expectationFailure "Sending thread did not finish before timeout."

