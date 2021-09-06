{-# LANGUAGE LambdaCase #-}

module Azure.ApplicationInsights where

import Data.Text (Text)

data TelemetryType
    = EventData

telemetryTypeToText :: TelemetryType -> Text
telemetryTypeToText = \case
    EventData -> "EventData"

trackEvent :: Text -> EventPayload -> IO ()
trackEvent eventName payload = do
    undefined
