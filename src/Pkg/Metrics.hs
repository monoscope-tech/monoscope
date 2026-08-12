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
  gitApiErrors,
  gitWebhookRejections,
  gitSyncHist,
  bump,
  recordMs,
  timed,
) where

import Effectful (Eff, IOE, (:>))
import Effectful.Exception (bracket)
import GHC.Clock (getMonotonicTime)
import OpenTelemetry.Attributes (Attribute, Attributes, emptyAttributes, unsafeAttributesFromListIgnoringLimits)
import OpenTelemetry.Metric.Core (
  Counter (..),
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


-- | Failed calls to a git host's REST API, by @host@ and @operation@.
--
-- Both dimensions are closed sets (four hosts, six operations), which is the whole reason
-- they are metric labels: the thing you actually want when a sync stops working is "GitLab
-- tree reads started failing at 03:10", and that is an aggregate. The error *text* is
-- unbounded and stays on the log line, never here.
gitApiErrors :: Counter Int64
gitApiErrors =
  unsafePerformIO
    $ meterCreateCounterInt64 monoscopeMeter "monoscope.git.api.errors" Nothing (Just "Failed git host API calls") defaultAdvisoryParameters
{-# NOINLINE gitApiErrors #-}


-- | Webhook deliveries refused, by @host@ and @reason@.
--
-- A rising count here is either a misconfigured secret or someone probing the endpoint, and
-- the two are told apart by the reason label — which is a fixed vocabulary, not the message.
gitWebhookRejections :: Counter Int64
gitWebhookRejections =
  unsafePerformIO
    $ meterCreateCounterInt64 monoscopeMeter "monoscope.git.webhook.rejected" Nothing (Just "Webhook deliveries that failed verification") defaultAdvisoryParameters
{-# NOINLINE gitWebhookRejections #-}


-- | Wall-time of one dashboard sync pull, by @host@.
gitSyncHist :: Histogram
gitSyncHist =
  unsafePerformIO
    $ meterCreateHistogram monoscopeMeter "monoscope.git.sync.duration" (Just "ms") Nothing defaultAdvisoryParameters
{-# NOINLINE gitSyncHist #-}


-- | Add one to a counter. Attributes must be bounded-cardinality; see 'gitApiErrors'.
bump :: MonadIO m => Counter Int64 -> [(Text, Attribute)] -> m ()
bump c attrs = liftIO $ counterAdd c 1 (toAttrs attrs)


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
