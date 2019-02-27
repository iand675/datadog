### :zap: WAI middleware for tracking metrics in Datadog :dog2: :zap:

dd-middleware provides a simple middleware for shoving metrics from WAI
requests to Datadog. Out of the box, it times all request/response
lifecycles that don't throw exceptions (excluding raw responses used in
Websocket upgrades and the like).

The default tracked metric is `warp.requests.duration`

Additional metrics can be sent as part of the request lifecycle by
grabbing the datadog client from the WAI vault:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Foldable
import qualified Data.Vault.Lazy as V
import Network.StatsD.Datadog

anApp :: Application
anApp req responder = do
  forM_ (V.lookup statsClientKey $ vault req) $ \client ->
    send client $ metric (MetricName "aardvark") Counter (1 :: Int)
  responder $ responseLBS "aardvark"
```




