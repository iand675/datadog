{-# LANGUAGE OverloadedStrings #-}

module Network.Datadog.Internal
( decodeDatadog
) where


import Control.Exception

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)


decodeDatadog :: FromJSON a => String -> ByteString -> IO a
decodeDatadog funcname body = either (throwIO . AssertionFailed . failstring) return $
                              eitherDecode body
  where failstring e = "Datadog Library decoding failure in \"" ++ funcname ++
                       "\": " ++ e ++ ": " ++ unpack (decodeUtf8 body)
