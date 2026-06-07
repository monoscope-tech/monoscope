module System.Tracing (
  Tracing,
  runTracing,
  withSpan,
  withSpan_,
  addEvent,
  addAttribute,
  setStatus,
  SpanStatus (..),

  -- * Cross-thread context propagation
  forkWithCtx,

  -- * Common attribute builders
  batchSpanAttrs,
) where

import Data.HashMap.Strict qualified as HM
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Ki qualified as Ki
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
import Relude hiding (span)
import UnliftIO.Exception (bracket, finally, withException)


data Tracing :: Effect where
  WithSpan :: Text -> [(Text, Attribute)] -> (Span -> m a) -> Tracing m a
  AddEvent :: Span -> Text -> [(Text, Attribute)] -> Tracing m ()
  AddAttribute :: Span -> Text -> Attribute -> Tracing m ()
  SetStatus :: Span -> SpanStatus -> Tracing m ()


type instance DispatchOf Tracing = 'Dynamic


makeEffect ''Tracing


runTracing :: IOE :> es => TracerProvider -> Eff (Tracing ': es) a -> Eff es a
runTracing tp = interpret $ \env -> \case
  WithSpan name attrs f -> do
    let tracer = Trace.makeTracer tp "monoscope" Trace.tracerOptions
        attrMap = HM.fromList attrs
    -- Properly propagate context through the span lifecycle
    localSeqUnliftIO env $ \unlift -> liftIO $ do
      ctx <- Context.getContext
      sp <- Trace.createSpan tracer ctx name (defaultSpanArguments{Trace.kind = Server, Trace.attributes = attrMap})
      Context.adjustContext (Context.insertSpan sp)
      -- Mark the span Error on exception, then always close it + restore
      -- context (otherwise failed operations show as green spans and we
      -- silently leak unclosed spans).
      withException (unlift (f sp)) (\(e :: SomeException) -> Trace.setStatus sp (Error (toText (displayException e))))
        `finally` do
          Context.adjustContext (const ctx)
          Trace.endSpan sp Nothing
  AddEvent span event attrs -> liftIO $ Trace.addEvent span $ Trace.NewEvent event (HM.fromList attrs) Nothing
  AddAttribute span k v -> liftIO $ Trace.addAttribute span k v
  SetStatus span status -> liftIO $ Trace.setStatus span status


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


-- | Standard attributes for batch-processing root spans (queue consumers,
-- gRPC handlers). Always emits @messaging.batch.message_count@; emits
-- @ce.type@ only when present in the CloudEvents header map.
batchSpanAttrs :: Int -> HM.HashMap Text Text -> [(Text, Attribute)]
batchSpanAttrs n attrs =
  ("messaging.batch.message_count", OA.toAttribute n)
    : toList (("ce.type",) . OA.toAttribute @Text <$> HM.lookup "ce-type" attrs)
