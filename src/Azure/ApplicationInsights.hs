{-# LANGUAGE LambdaCase #-}

module Azure.ApplicationInsights where

import Data.Text (Text)

trackEvent :: Text -> EventPayload -> IO ()
trackEvent eventName payload = do
    undefined
