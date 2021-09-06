module Azure.ApplicationInsights.Event
    ( EventData(..)
    , mkEventData
    ) where

import Data.Aeson ((.=), ToJSON(..), object)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Azure.ApplicationInsights.BaseTelemetry (BaseTelemetry(..))
import Azure.ApplicationInsights.Data (Data(..))
import Azure.ApplicationInsights.TelemetryType (TelemetryType(..))

data EventTelemetry = EventTelemetry
    { eventTelemetryName          :: Text
    , eventTelemetryMeasurements  :: Map Text Int
    , eventTelemetryBaseTelemetry :: BaseTelemetry
    }

data EventData = EventData
    { eventDataVer          :: Int
    , eventDataName         :: Text
    , eventDataProperties   :: Map Text Text
    , eventDataMeasurements :: Map Text Int
    }
    deriving (Eq, Show)

instance ToJSON EventData where
    toJSON et = object
        [ "ver" .= eventDataVer et
        , "name" .= eventDataName et
        , "properties" .= eventDataProperties et
        , "measurements" .= eventDataMeasurements et
        ]

instance Semigroup EventData where
    EventData _ _ p1 m1 <> EventData v n p2 m2 =
        EventData v n (p1 <> p2) (m1 <> m2)

instance Monoid EventData where
    mempty = EventData 2 "" mempty mempty

mkEventData :: EventTelemetry -> Data EventData
mkEventData et = Data TTEvent $ EventData
    { eventDataVer          = 2
    , eventDataName         = eventTelemetryName et
    , eventDataProperties   = baseTelemetryProperties
        $ eventTelemetryBaseTelemetry et
    , eventDataMeasurements = eventTelemetryMeasurements et
    }
