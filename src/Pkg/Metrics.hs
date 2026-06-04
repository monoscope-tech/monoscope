-- | Shared OpenTelemetry metric instruments.
--
-- Instruments are spec-safe to cache once and reuse globally, so we hold
-- them as top-level 'unsafePerformIO' thunks rather than threading a
-- 'Meter' through 'AuthContext'. The global 'MeterProvider' is initialised
-- in 'Start.startApp' before any of these are forced; before init they
-- silently no-op via the SDK's 'noopMeterProvider'.
module Pkg.Metrics (
  ingestDecodeHist,
  ingestWriteHist,
  recordMs,
  timed,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Exception (bracket)
import GHC.Clock (getMonotonicTime)
import OpenTelemetry.Attributes (Attribute, Attributes, emptyAttributes, unsafeAttributesFromListIgnoringLimits)
import OpenTelemetry.Metric.Core (
  Histogram (..),
  Meter (..),
  defaultAdvisoryParameters,
  getGlobalMeterProvider,
  getMeter,
 )
import Relude
import System.IO.Unsafe (unsafePerformIO)


monoscopeMeter :: Meter
monoscopeMeter = unsafePerformIO $ getGlobalMeterProvider >>= flip getMeter "monoscope"
{-# NOINLINE monoscopeMeter #-}


-- | Wall-time spent decoding/redacting/building span vectors in
-- 'ProcessMessage.processMessages', per batch.
ingestDecodeHist :: Histogram
ingestDecodeHist =
  unsafePerformIO
    $ meterCreateHistogram monoscopeMeter "monoscope.ingest.decode.duration" (Just "ms") Nothing defaultAdvisoryParameters
{-# NOINLINE ingestDecodeHist #-}


-- | Wall-time spent in the PG + TF dual write
-- ('Telemetry.insertAndHandOff'), per batch.
ingestWriteHist :: Histogram
ingestWriteHist =
  unsafePerformIO
    $ meterCreateHistogram monoscopeMeter "monoscope.ingest.write.duration" (Just "ms") Nothing defaultAdvisoryParameters
{-# NOINLINE ingestWriteHist #-}


recordMs :: MonadIO m => Histogram -> Double -> [(Text, Attribute)] -> m ()
recordMs h v attrs = liftIO $ histogramRecord h v (toAttrs attrs)


-- | Time an action and record its wall-time (ms) on @hist@. Uses
-- 'bracket' so the measurement fires on both success and exception —
-- useful when you want failures represented in the distribution (e.g.
-- DB write latency where a timeout is still latency). Pass @[]@ for
-- no attributes.
timed :: IOE :> es => Histogram -> [(Text, Attribute)] -> Eff es a -> Eff es a
timed hist attrs act =
  bracket
    (liftIO getMonotonicTime)
    (\t0 -> liftIO getMonotonicTime >>= \t1 -> recordMs hist ((t1 - t0) * 1000) attrs)
    (const act)


toAttrs :: [(Text, Attribute)] -> Attributes
toAttrs [] = emptyAttributes
toAttrs xs = unsafeAttributesFromListIgnoringLimits xs
{-# INLINE toAttrs #-}
