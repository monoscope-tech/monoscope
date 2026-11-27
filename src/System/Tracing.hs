module System.Tracing (
  Tracing,
  runTracing,
  monoscopeTracer,
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
  inRootSpan,
  forkBackground,
  forkBackgroundFor,

  -- * Common attribute builders
  batchSpanAttrs,
  messagingAttrs,
  MessagingSystem (..),
  MessagingOp (..),
  NotInstrumented,

  -- * Outbound HTTP
  newTracedManager,
  redactUrl,
) where

import Data.Aeson qualified as AE
import Data.Char (isAsciiLower)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, forkIO)
import Effectful.Dispatch.Dynamic
import Effectful.Ki qualified as Ki
import Effectful.Log (Log)
import Effectful.TH
import Network.HTTP.Client qualified as HC
import OpenTelemetry.Attributes (Attribute)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Instrumentation.HttpClient.Raw qualified as OHC
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
import System.Logging qualified as Log
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
    let tracer = monoscopeTracer tp
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


monoscopeTracer :: TracerProvider -> Trace.Tracer
monoscopeTracer tp = Trace.makeTracer tp "monoscope" Trace.tracerOptions


withSpan :: Tracing :> es => Text -> [(Text, Attribute)] -> (Span -> Eff es a) -> Eff es a
withSpan = withSpanKind Internal


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


-- | Run @act@ as the root span of a new trace. Long-lived fibers are themselves
-- auto-instrumented spans that never end, so a tick run under one joins a single
-- process-lifetime trace, and the performance detector reads its periodic queries
-- as an N+1. The fiber's own context is restored afterwards.
inRootSpan :: TracerProvider -> Text -> IO a -> NotInstrumented (IO a)
inRootSpan tp name act =
  bracket (Context.attachContext Context.empty) Context.detachContext
    $ const
    $ Trace.inSpan (monoscopeTracer tp) name defaultSpanArguments act


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


-- | A pooled manager whose every request is an OTel Client span that sends @traceparent@. The
-- tracer is captured when the manager is built, so build it after the global provider is set.
-- URLs carry credentials (webhook tokens, signed ids), so the URL attributes the
-- instrumentation recorded are overwritten, in both semconv spellings, with 'redactUrl'ed copies.
newTracedManager :: HC.ManagerSettings -> IO HC.Manager
newTracedManager settings = do
  traced <- OHC.instrumentManagerSettings OHC.httpClientInstrumentationConfig settings
  HC.newManager
    traced
      { HC.managerModifyRequest = \r -> do
          r' <- HC.managerModifyRequest traced r
          let host = decodeUtf8 (HC.host r')
              target = redactUrl host $ decodeUtf8 (HC.path r' <> HC.queryString r')
              full = (if HC.secure r' then "https://" else "http://") <> host <> ":" <> show (HC.port r') <> target
              attrs :: [(Text, Text)]
              attrs = [("url.full", full), ("http.url", full), ("http.target", target), ("url.path", T.takeWhile (/= '?') target), ("url.query", T.dropWhile (/= '?') target)]
          whenJustM (Context.lookupSpan <$> Context.getContext) \sp -> traverse_ (uncurry (Trace.addAttribute sp)) attrs
          pure r'
      }


-- | A request target fit for a span. Only the first-party APIs we call keep their path, and even
-- there a 16+ character segment that is not a lowercase slug and every query value are masked.
-- Any other host is a customer's (webhooks, probes, scrapes, self-hosted git), where a secret of
-- any length can sit in the path, so it keeps nothing.
--
-- >>> redactUrl "api.stripe.com" "/v1/invoices?limit=12&subscription=sub_1Abc"
-- "/v1/invoices?limit={redacted}&subscription={redacted}"
-- >>> redactUrl "discord.com" "/api/v10/webhooks/1234567890123456789/aW50ZXJhY3Rpb246MTIzNDU2Nzg5"
-- "/api/v10/webhooks/{redacted}/{redacted}"
-- >>> redactUrl "api.github.com" "/repos/monoscope-tech/monoscope-web-components/issues/12/comments"
-- "/repos/monoscope-tech/monoscope-web-components/issues/12/comments"
-- >>> map (redactUrl "hooks.slack.com") ["/services/T0AB12CD3/B0EF45GH6/xoxbAbCdEfGh", "/hook/s3cret?x=1"]
-- ["/{redacted}","/{redacted}"]
redactUrl :: Text -> Text -> Text
redactUrl host target
  | host `notElem` firstParty = "/{redacted}"
  | otherwise = T.intercalate "/" (map segment $ T.splitOn "/" path) <> bool ("?" <> T.intercalate "&" (map value $ T.splitOn "&" $ T.drop 1 query)) "" (T.null query)
  where
    firstParty = ["api.stripe.com", "api.github.com", "api.lemonsqueezy.com", "slack.com", "discord.com", "api.openai.com", "api.twilio.com", "api.convertkit.com", "api.bitbucket.org", "gitlab.com", "pubsub.googleapis.com", "oauth2.googleapis.com"] :: [Text]
    (path, query) = T.breakOn "?" target
    segment s = if T.length s >= 16 && not (T.all (\c -> isAsciiLower c || c == '-') s) then "{redacted}" else s
    value kv = fst (T.breakOn "=" kv) <> "={redacted}"


-- | OTel messaging semconv for a span that sends to or processes from one queue, so the service
-- graph draws the queue as a peer.
messagingAttrs :: MessagingSystem -> Text -> MessagingOp -> [(Text, Attribute)]
messagingAttrs system destination op =
  [ ("messaging.system", case system of Kafka -> "kafka"; GcpPubsub -> "gcp_pubsub"; OddJobs -> "odd_jobs")
  , ("messaging.destination.name", OA.toAttribute destination)
  , ("messaging.operation.type", case op of Send -> "send"; Process -> "process")
  ]


data MessagingSystem = Kafka | GcpPubsub | OddJobs


data MessagingOp = Send | Process


-- | Excluded in @auto-instrument-config.toml@: wrap a hot helper's result type to keep the
-- AutoInstrument plugin from opening a span per call.
type NotInstrumented :: Type -> Type
type NotInstrumented a = a


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
