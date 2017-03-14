{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Lib
    ( ddMiddleware
    ) where

import Control.Monad
import Data.IORef
import Data.Text (Text, pack)
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
      Just r -> modifyIORef ref (tag "route_name" (pack $ show r) :)
  m
