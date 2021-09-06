module Azure.ApplicationInsights.Envelope
    ( Envelope(..)
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson ((.=), object)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Azure.ApplicationInsights.Data (Data(..))
import Azure.ApplicationInsights.InstrumentationKey
    (InstrumentationKey, instrumentationKeyToName)
import Azure.ApplicationInsights.Telemetry (Telemetry(..))
import Azure.ApplicationInsights.TelemetryType
    (TelemetryType, telemetryTypeToEnvelopeNameFragment)

data Envelope = Envelope
    { envelopeVer        :: Int
    , envelopeName       :: Text
    , envelopeTime       :: UTCTime
    -- ^ Must be formatted to ISO8601 when transmitted.
    , envelopeSampleRate :: Word
    , envelopeSeq        :: Maybe Text
    , envelopeIKey       :: InstrumentationKey
    , envelopeTags       :: Map Text Text
    , envelopeData       :: Data
    }
    deriving (Eq, Show)

instance ToJSON Envelope where
    toJSON e =
        object
            $  [ "ver" .= envelopeVer e
               , "name" .= envelopeName e
               , "time" .= iso8601Show (envelopeTime e)
               , "sampleRate" .= envelopeSampleRate e
               , "iKey" .= envelopeIKey e
               , "tags" .= envelopeTags e
               , "data" .= envelopeData e
               ]
            <> maybe mempty pure (envelopeSeq e)

mkEnvelope :: MonadIO m => InstrumentationKey -> Telemetry -> m Envelope
mkEnvelope iKey = \case
    Event et -> do
        now <- liftIO getCurrentTime
        pure $ Envelope
            { envelopeVer  = 1
            , envelopeSeq  = ""
            , envelopeIKey = iKey
            , envelopeTags = mempty
            , envelopeName = mkEnvelopeName iKey
            , envelopeData = mkEventData et
            , envelopeTime = now
            }

mkEnvelopeName :: InstrumentationKey -> TelemetryType -> Text
mkEnvelopeName iKey tt = mconcat
    [ "Microsoft.ApplicationInsights."
    , instrumentationKeyToName iKey
    , "."
    , telemetryTypeToEnvelopeNameFragment tt
    ]
