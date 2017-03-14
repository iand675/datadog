{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Datadog
    ( ddMiddleware
    ) where

import Control.Monad
import Data.IORef
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vault.Lazy as L
import Network.StatsD.Datadog
import Network.Wai
import Network.Wai.Middleware.Datadog
import Yesod.Core

ddMiddleware :: (Show (Route site), ToTypedContent res) => HandlerT site IO res -> HandlerT site IO res
ddMiddleware m = do
  mRoute <- getCurrentRoute
  req <- waiRequest
  liftIO $ case L.lookup statsTagsKey $ vault req of
    Just ref -> case mRoute of
      Nothing -> modifyIORef ref (tag "route_name" "none" :)
      Just r -> do
        let rn = pack $ takeWhile (/= ' ') $ show r
        modifyIORef ref (tag "route_name" rn :)
    Nothing -> return ()
  m
