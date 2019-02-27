module Test.Network.Datadog.StatsD (spec) where

import Control.Monad.Catch (bracket)
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
  , recvFrom
  , socket
  )
import Control.Monad
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it)

import Network.StatsD.Datadog (defaultSettings, event, send, withDogStatsD)

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
