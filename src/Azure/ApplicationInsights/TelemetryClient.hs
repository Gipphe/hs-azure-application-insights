module Azure.ApplicationInsights.TelemetryClient
    ( TelemetryClient
    , trackEvent
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
    ((/:), POST(..), ReqBodyLbs(..), header, https, jsonResponse, req)

import Azure.ApplicationInsights.Envelope (mkEnvelope)
import Azure.ApplicationInsights.InstrumentationKey
    (InstrumentationKey, instrumentationKeyFromEnv)

data TelemetryClient = TelemetryClient
    { telemetryClientInstrumentationKey :: InstrumentationKey
    }

data BreezeResponse = BreezeResponse
    { breezeResponseItemsReceived :: Int
    , breezeResponseItemsAccepted :: Int
    , breezeResponseErrors        :: [BreezeError]
    }

data BreezeError = BreezeError
    { breezeErrorIndex      :: Int
    , breezeErrorStatusCode :: Int
    , breezeErrorMessage    :: Text
    }

mkTelemetryClientIO :: IO TelemetryClient
mkTelemetryClientIO = do
    TelemetryClient
        <$> instrumentationKeyFromEnv "APPINSIGHTS_INSTRUMENTATIONKEY"

track :: (MonadIO m) => TelemetryClient -> Telemetry -> m ()
track client t = do
    envelope <- mkEnvelope t (telemetryClientInstrumentationKey client)

send :: MonadIO m => Envelope -> m ()
send e = do
    req POST endpoint body (Proxy :: Proxy AppInsightsResponse) mempty
  where
    endpoint = https "dc.services.visualstudio.com" :/ "v2.1" :/ "track"
    -- opts     = header "Content-Type" "application/x-json-stream"
    opts     = header "Accept" "application/json"
    body     = ReqBodyLbs $ encode e
