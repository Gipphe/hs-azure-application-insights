module Azure.ApplicationInsights.BaseTelemetry (BaseTelemetry(..)) where

import Data.Time (UTCTime)

data BaseTelemetry = BaseTelemetry
    { baseTelemetryTime :: Maybe UTCTime
    , baseTelemetryProperties :: Map Text Text
    }
