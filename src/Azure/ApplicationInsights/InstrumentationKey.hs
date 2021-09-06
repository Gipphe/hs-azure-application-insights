module Azure.ApplicationInsights.InstrumentationKey
    ( InstrumentationKey
    , instrumentationKeyToName
    , instrumentationKeyFromEnv
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnv)

newtype InstrumentationKey = InstrumentationKey
    { getInstrumentationKey :: Text
    }

instrumentationKeyToName :: InstrumentationKey -> Text
instrumentationKeyToName = T.filter (/= '-') . getInstrumentationKey

instrumentationKeyFromEnv :: MonadIO m => String -> m InstrumentationKey
instrumentationKeyFromEnv k = InstrumentationKey . T.pack <$> liftIO (getEnv k)
