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
  count,

  -- * Live Tail
  liveTailEvaluated,
  liveTailMatched,
  liveTailSkipped,
  liveTailFilterErrors,
  liveTailPublishFailed,
  liveTailOversized,
  liveTailRelayDropped,
  liveTailBrowserDropped,
  liveTailCacheLoaded,
  liveTailCacheRejected,
  liveTailDecodeFailed,

  -- * Ingest and extraction
  ingestBatchesProcessed,
  ingestSpansProcessed,
  extractionBatchesDropped,
  drainFlushesCompleted,
  drainSpansFlushed,
  drainPatternsPersisted,

  -- * Dashboards
  widgetSqlErrors,
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


-- | Declare a monotonic counter. Every instrument below pairs this with its own @NOINLINE@,
-- which is what makes the caching safe — the instrument must be created once, not once per
-- call site.
mkCounter :: Text -> Maybe Text -> Counter Int64
mkCounter name unit = unsafePerformIO $ meterCreateCounterInt64 monoscopeMeter name unit Nothing defaultAdvisoryParameters
{-# NOINLINE mkCounter #-}


mkHist :: Text -> Maybe Text -> Histogram
mkHist name unit = unsafePerformIO $ meterCreateHistogram monoscopeMeter name unit Nothing defaultAdvisoryParameters
{-# NOINLINE mkHist #-}


-- | Add to a counter, taking the increment as an 'Int' because that is what the call sites
-- already hold (lengths, 'Sum' totals).
--
-- Zero is a no-op on purpose. Most call sites fold a batch and add whatever came out, and a
-- great many batches produce nothing at all — recording those would create a time series for
-- every attribute combination that has never actually happened, which is cost without signal.
count :: MonadIO m => Counter Int64 -> Int -> [(Text, Attribute)] -> m ()
count c n attrs = when (n /= 0) $ liftIO $ counterAdd c (fromIntegral n) (toAttrs attrs)


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


-- ---------------------------------------------------------------------------------------
-- Live Tail
-- ---------------------------------------------------------------------------------------

-- $livetail
--
-- All of these carry at most a @transport@ attribute (@kafka@ or @relay@) — two values, and
-- constant for the lifetime of a process. Deliberately __no__ project, user, service, query or
-- subscription id: those are unbounded, and a metric labelled with one is a cardinality
-- incident rather than a diagnostic. That context belongs on spans and logs, which already
-- carry it.
--
-- The pairs are what make these readable. @evaluated@ without @matched@ cannot distinguish a
-- filter that matches nothing from a tail nobody opened; @matched@ without @publishFailed@ and
-- @oversized@ cannot tell "delivered" from "dropped on the way out". Each of the three drop
-- counters exists because that drop is otherwise indistinguishable, to the person watching an
-- empty tail, from "nothing is happening".


-- | Records offered to at least one subscription's filter.
liveTailEvaluated :: Counter Int64
liveTailEvaluated = mkCounter "monoscope.live_tail.records.evaluated" Nothing
{-# NOINLINE liveTailEvaluated #-}


-- | Record/subscription pairs that matched and were handed to the transport.
liveTailMatched :: Counter Int64
liveTailMatched = mkCounter "monoscope.live_tail.records.matched" Nothing
{-# NOINLINE liveTailMatched #-}


-- | Records a batch never evaluated because it hit the per-batch evaluation budget.
liveTailSkipped :: Counter Int64
liveTailSkipped = mkCounter "monoscope.live_tail.records.skipped" Nothing
{-# NOINLINE liveTailSkipped #-}


-- | Filters that could not be decided on a record. Treated as non-matches, so a rising line
-- here means users are quietly seeing less than they asked for.
liveTailFilterErrors :: Counter Int64
liveTailFilterErrors = mkCounter "monoscope.live_tail.filter.errors" Nothing
{-# NOINLINE liveTailFilterErrors #-}


-- | Matched rows the transport refused.
liveTailPublishFailed :: Counter Int64
liveTailPublishFailed = mkCounter "monoscope.live_tail.publish.failed" Nothing
{-# NOINLINE liveTailPublishFailed #-}


-- | Matched rows dropped for exceeding the envelope size cap.
liveTailOversized :: Counter Int64
liveTailOversized = mkCounter "monoscope.live_tail.rows.oversized" Nothing
{-# NOINLINE liveTailOversized #-}


-- | Rows the relay write buffer discarded under load.
liveTailRelayDropped :: Counter Int64
liveTailRelayDropped = mkCounter "monoscope.live_tail.relay.dropped" Nothing
{-# NOINLINE liveTailRelayDropped #-}


-- | Rows dropped because a browser could not keep up. The number the UI shows the user.
liveTailBrowserDropped :: Counter Int64
liveTailBrowserDropped = mkCounter "monoscope.live_tail.browser.dropped" Nothing
{-# NOINLINE liveTailBrowserDropped #-}


-- | Active subscriptions seen by one cache refresh. A histogram rather than a counter because
-- the question is "how many tails are open right now", and sitting at the cap is the signal.
liveTailCacheLoaded :: Histogram
liveTailCacheLoaded = mkHist "monoscope.live_tail.cache.loaded" Nothing
{-# NOINLINE liveTailCacheLoaded #-}


-- | Subscriptions dropped from a refresh because their stored filter no longer compiles.
liveTailCacheRejected :: Counter Int64
liveTailCacheRejected = mkCounter "monoscope.live_tail.cache.rejected" Nothing
{-# NOINLINE liveTailCacheRejected #-}


-- | Side-topic messages that did not decode, by @reason@ (@skew@ or @undecodable@) — the one
-- attribute here that is not @transport@, and bounded to those two values. A skew that never
-- settles is a rolling deploy that never finished.
liveTailDecodeFailed :: Counter Int64
liveTailDecodeFailed = mkCounter "monoscope.live_tail.consumer.decode_failed" Nothing
{-# NOINLINE liveTailDecodeFailed #-}


-- ---------------------------------------------------------------------------------------
-- Ingest and extraction
-- ---------------------------------------------------------------------------------------

-- | Eager-track batches completed by the extraction worker.
ingestBatchesProcessed :: Counter Int64
ingestBatchesProcessed = mkCounter "monoscope.extraction.batches.processed" Nothing
{-# NOINLINE ingestBatchesProcessed #-}


-- | Spans carried by those batches.
ingestSpansProcessed :: Counter Int64
ingestSpansProcessed = mkCounter "monoscope.extraction.spans.processed" Nothing
{-# NOINLINE ingestSpansProcessed #-}


-- | Batches the extraction worker refused because its queue was full. These spans are written
-- and queryable but never get patterns or enrichment, which is silent by nature.
extractionBatchesDropped :: Counter Int64
extractionBatchesDropped = mkCounter "monoscope.extraction.batches.dropped" Nothing
{-# NOINLINE extractionBatchesDropped #-}


drainFlushesCompleted :: Counter Int64
drainFlushesCompleted = mkCounter "monoscope.drain.flushes.completed" Nothing
{-# NOINLINE drainFlushesCompleted #-}


drainSpansFlushed :: Counter Int64
drainSpansFlushed = mkCounter "monoscope.drain.spans.flushed" Nothing
{-# NOINLINE drainSpansFlushed #-}


drainPatternsPersisted :: Counter Int64
drainPatternsPersisted = mkCounter "monoscope.drain.patterns.persisted" Nothing
{-# NOINLINE drainPatternsPersisted #-}


-- ---------------------------------------------------------------------------------------
-- Dashboards
-- ---------------------------------------------------------------------------------------

-- | Dashboard widget queries that failed and rendered an error overlay.
--
-- Unlabelled, which departs from the @TODO@ this replaces: it proposed
-- @widget_sql_error{project_id, error_class=userMsg}@, and both of those labels are unbounded
-- — @project_id@ by definition, and @userMsg@ because it is a sanitised database error
-- carrying column and table names. Either would multiply the series count by the number of
-- distinct failures. The rate is what alerts; the message is already on the span and the log
-- line, which is where high-cardinality context belongs.
widgetSqlErrors :: Counter Int64
widgetSqlErrors = mkCounter "monoscope.dashboard.widget.sql_errors" Nothing
{-# NOINLINE widgetSqlErrors #-}
