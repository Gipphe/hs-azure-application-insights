module Azure.ApplicationInsights.TelemetryType
    ( TelemetryType(..)
    , telemetryTypeToBaseType
    ) where

import Data.Aeson (ToJSON(..))

data TelemetryType = TTEvent

instance ToJSON TelemetryType where
    toJSON = toJSON . telemetryTypeToBaseType

telemetryTypeToBaseType :: TelemetryType -> Text
telemetryTypeToBaseType = \case
    TTEvent -> "EventData"

telemetryTypeToEnvelopeNameFragment :: TelemetryType -> Text
telemetryTypeToEnvelopeNameFragment = \case
    TTEvent -> "Event"
