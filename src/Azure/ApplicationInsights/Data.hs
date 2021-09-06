{-# LANGUAGE GADTs #-}

module Azure.ApplicationInsights.Data
    ( Data(..)
    ) where

import Data.Aeson ((.=), ToJSON(..), object)

import Azure.ApplicationInsights.TelemetryType (TelemetryType)

data Data where
    Data ::ToJSON a => TelemetryType -> a -> Data

instance ToJSON a => ToJSON (Data a) where
    toJSON (Data t d) = object ["baseType" .= t, "baseData" .= d]
