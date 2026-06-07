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
) where

import Data.HashMap.Strict qualified as HM
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Ki qualified as Ki
import Effectful.TH
import OpenTelemetry.Attributes (Attribute)
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
import UnliftIO.Exception (finally)


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
      -- Get current context
      ctx <- Context.getContext
      -- Create span with current context
      sp <- Trace.createSpan tracer ctx name (defaultSpanArguments{Trace.kind = Server, Trace.attributes = attrMap})
      -- Insert span into context for propagation
      let newCtx = Context.insertSpan sp ctx
      -- Set the new context
      Context.adjustContext (const newCtx)
      -- Run the action with the new context
      result <- unlift (f sp)
      -- Restore the original context
      Context.adjustContext (const ctx)
      -- End the span
      Trace.endSpan sp Nothing
      pure result
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
  Ki.fork scope do
    prevCtx <- liftIO $ Context.attachContext ctx
    action `finally` liftIO (Context.adjustContext (const prevCtx))
