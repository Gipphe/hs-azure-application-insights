module Azure.ApplicationInsights.Telemetry
    ( Telemetry(..)
    ) where

import Azure.ApplicationInsights.EventData (EventData)

data Telemetry = Event EventData
