module System.Tracing (
  Tracing,
  runTracing,
  withSpan,
  withSpanKind,
  withSpan_,
  addEvent,
  addAttribute,
  setStatus,
  SpanStatus (..),
  SpanKind (..),

  -- * Cross-thread context propagation
  forkWithCtx,
  forkBackground,
  forkBackgroundFor,

  -- * Common attribute builders
  batchSpanAttrs,
) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Effectful
import Effectful.Concurrent (Concurrent, forkIO)
import Effectful.Dispatch.Dynamic
import Effectful.Ki qualified as Ki
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.TH
import OpenTelemetry.Attributes (Attribute)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Trace (
  Span,
  SpanKind (..),
  SpanStatus (..),
  TracerProvider,
  defaultSpanArguments,
 )
import OpenTelemetry.Trace qualified as Trace
import Pkg.ErrorFingerprint qualified as EF
import Relude hiding (span)
import UnliftIO qualified
import UnliftIO.Exception (bracket, finally, tryAny, withException)


data Tracing :: Effect where
  WithSpanKind :: SpanKind -> Text -> [(Text, Attribute)] -> (Span -> m a) -> Tracing m a
  AddEvent :: Span -> Text -> [(Text, Attribute)] -> Tracing m ()
  AddAttribute :: Span -> Text -> Attribute -> Tracing m ()
  SetStatus :: Span -> SpanStatus -> Tracing m ()


type instance DispatchOf Tracing = 'Dynamic


makeEffect ''Tracing


runTracing :: IOE :> es => TracerProvider -> Eff (Tracing ': es) a -> Eff es a
runTracing tp = interpret $ \env -> \case
  WithSpanKind kind name attrs f -> do
    let tracer = Trace.makeTracer tp "monoscope" Trace.tracerOptions
        attrMap = HM.fromList attrs
    -- Properly propagate context through the span lifecycle
    localSeqUnliftIO env $ \unlift -> liftIO $ do
      ctx <- Context.getContext
      sp <- Trace.createSpan tracer ctx name (defaultSpanArguments{Trace.kind = kind, Trace.attributes = attrMap})
      Context.adjustContext (Context.insertSpan sp)
      -- Mark the span Error on exception (unless it is expected control flow, which became
      -- ~870 runtime-exception issues), then always close it + restore context.
      withException (unlift (f sp)) (\(e :: SomeException) -> let rendered = toText (displayException e) in unless (EF.isExpectedControlFlow "" rendered) $ Trace.setStatus sp (Error rendered))
        `finally` do
          Context.adjustContext (const ctx)
          Trace.endSpan sp Nothing
  AddEvent span event attrs -> liftIO $ Trace.addEvent span $ Trace.NewEvent event (HM.fromList attrs) Nothing
  AddAttribute span k v -> liftIO $ Trace.addAttribute span k v
  SetStatus span status -> liftIO $ Trace.setStatus span status


withSpan :: Tracing :> es => Text -> [(Text, Attribute)] -> (Span -> Eff es a) -> Eff es a
withSpan = withSpanKind Server


withSpan_ :: Tracing :> es => Text -> [(Text, Attribute)] -> Eff es a -> Eff es a
withSpan_ name attrs action = withSpan name attrs $ const action


-- | Effectful 'Ki.fork' that copies the OTel thread-local context into the
-- child fiber. Without this, hasql/inSpan spans emitted from the child have
-- no parent because 'Context.ThreadLocal' is keyed by 'ThreadId'. The detach
-- token returned by 'attachContext' is restored on exit so the entry doesn't
-- linger if the runtime ever pools fork threads.
forkWithCtx
  :: (IOE :> es, Ki.StructuredConcurrency :> es)
  => Ki.Scope -> Eff es a -> Eff es (Ki.Thread a)
forkWithCtx scope action = do
  ctx <- liftIO Context.getContext
  Ki.fork scope
    $ bracket
      (liftIO $ Context.attachContext ctx)
      (liftIO . Context.detachContext)
      (const action)


-- | Fire-and-forget a task that must outlive the current request (e.g. Slack/Twilio
-- handlers that ACK fast then do the real work). Uses the app-lifetime ki scope when one
-- is present (prod) so the thread is tracked and reaped on shutdown; falls back to an
-- untracked fork only in non-server contexts (tests) where no scope exists. The action is
-- always guarded so a thrown exception is logged at attention, never silently dropped.
forkBackground
  :: (Concurrent :> es, IOE :> es, Ki.StructuredConcurrency :> es, Log :> es)
  => Maybe Ki.Scope -> Text -> Eff es () -> Eff es ()
forkBackground = forkBackgroundFor backgroundTaskTimeoutMicros


-- | 'forkBackground' with its own wall-clock cap, for work known to run longer.
forkBackgroundFor
  :: (Concurrent :> es, IOE :> es, Ki.StructuredConcurrency :> es, Log :> es)
  => Int -> Maybe Ki.Scope -> Text -> Eff es () -> Eff es ()
forkBackgroundFor capMicros scopeM label action =
  case scopeM of
    Just scope -> void $ forkWithCtx scope guardedAction
    Nothing -> void $ forkIO guardedAction
  where
    guardedAction =
      whenLeftM_ (tryAny runWithTimeout) \e ->
        Log.logAttention (label <> " background task failed") (AE.object ["error" AE..= show @Text e])
    -- Hard-cap every fire-and-forget task. A hung external call (Slack/Twilio HTTP
    -- with no response timeout) otherwise keeps its ki fiber alive, blocking the
    -- app-lifetime scope's reap on shutdown → zombie process (2026-07-06 incident).
    runWithTimeout =
      UnliftIO.timeout capMicros action >>= \case
        Just () -> pass
        Nothing -> Log.logAttention (label <> " background task timed out") (AE.object ["timeout_secs" AE..= (capMicros `div` 1_000_000 :: Int)])


-- | Wall-clock cap for a single 'forkBackground' task (Slack/Twilio/WhatsApp).
backgroundTaskTimeoutMicros :: Int
backgroundTaskTimeoutMicros = 30_000_000 -- 30s


-- | Standard attributes for batch-processing root spans (queue consumers,
-- gRPC handlers). Always emits @messaging.batch.message_count@; emits
-- @ce.type@ (OTel attribute name, dot) only when @"ce-type"@ (CloudEvents
-- header name, hyphen) is present in the headers map.
batchSpanAttrs :: Int -> HM.HashMap Text Text -> [(Text, Attribute)]
batchSpanAttrs n attrs =
  catMaybes
    [ Just ("messaging.batch.message_count", OA.toAttribute n)
    , ("ce.type",) . OA.toAttribute <$> HM.lookup "ce-type" attrs
    ]
