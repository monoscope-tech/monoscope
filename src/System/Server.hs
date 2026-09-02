module System.Server (runMonoscope, mkServer, cancelAllConcurrently, hashedAssetMiddleware) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently_, race, waitAnyCatch)
import Control.Concurrent.STM (check)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Pool as Pool (destroyAllResources)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Fail (runFailIO)
import Effectful.Ki qualified as Ki
import Effectful.Time (runTime)
import GHC.Profiling (startHeapProfTimer, startProfTimer, stopHeapProfTimer, stopProfTimer)
import Kafka.Consumer qualified as KC
import Kafka.Producer qualified as KProd
import Kafka.Types qualified as KT
import Log (LogLevel (..), runLogT)
import Log qualified as LogBase
import Network.HTTP.Types (methodGet, methodHead, status200, status404, status500)
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setOnException, setOnExceptionResponse, setPort)
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip (GzipFiles (..), GzipSettings (..), defaultGzipSettings, gzip)
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace (TracerProvider)
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.Replay (processReplayEvents)
import Pkg.DeriveUtils (staticAssetHashes, stripAssetHash)
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.LiveTail qualified as LiveTail
import Pkg.Metrics qualified as Metrics
import Pkg.Queue qualified as Queue
import ProcessMessage (processMessages)
import Relude
import Servant (FromHttpApiData (..))
import Servant qualified
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (
  AuthContext (backgroundScope, config, extractionWorker, jobsPool, liveTail, pool, timefusionPgPool),
  DeploymentEnv (Dev),
  EnvConfig (..),
  getAppContext,
 )
import System.Exit (ExitCode (ExitFailure))
import System.Logging qualified as Logging
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM, sigUSR2)
import System.TimeManager (TimeoutThread)
import System.Timeout (timeout)
import System.Tracing (withSpan_)
import System.Types (ATBackgroundCtx, effToServantHandler, runBackground)
import Web.Auth qualified as Auth
import Web.Routes qualified as Routes


runMonoscope :: TracerProvider -> IO ()
runMonoscope tp =
  Safe.bracket
    (getAppContext & runFailIO & runEff)
    (runEff . shutdownMonoscope)
    \env -> runEff . runTime . runConcurrent . Ki.runStructuredConcurrency $ Ki.scoped \backgroundScope -> do
      let baseURL = "http://localhost:" <> show env.config.port
      liftIO $ blueMessage $ "Starting Monoscope server on " <> baseURL
      let withLogger = Logging.makeLogger env.config.loggingDestination
      -- App-lifetime scope for fire-and-forget handler work (Slack/Twilio). The scope
      -- lives until shutdown, then ki reaps any still-running background fibers.
      envWithLiveTail <- liftIO (withLiveTailTransport env)
      withLogger \l -> runServer l envWithLiveTail{backgroundScope = Just backgroundScope} tp


-- | One log shape for every Live Tail fiber.
--
-- Seven call sites wrote this by hand; the source name and level are decisions that should be
-- made once, not re-typed per fiber where they can quietly diverge.
liveTailLog :: LogBase.Logger -> Text -> AE.Value -> IO ()
liveTailLog lg msg = runLogT "live-tail" lg LogInfo . LogBase.logAttention msg


-- | A supervised Live Tail fiber: run @act@ on a tick, forever, surviving any exception.
--
-- The catch is the point. These fibers sit beside ingestion, and one that dies on a transient
-- database blip takes a feature down silently until the next deploy. Expressing "never exits"
-- once means it cannot be forgotten in the next fiber someone adds.
liveTailFiber :: LogBase.Logger -> Text -> Int -> IO () -> IO ()
liveTailFiber lg what intervalMs act =
  forever $ do
    Safe.handleAny (\(e :: SomeException) -> liveTailLog lg ("live_tail: " <> what <> " failed") (AE.object ["error" AE..= show @Text e])) act
    threadDelay (intervalMs * 1000)


-- | Record a Live Tail event inside its own span, so the counter carries an exemplar.
--
-- Metrics recorded outside a span get no trace link — silently, since nothing fails and the
-- counter still moves. 'runBackground' interprets the 'Tracing' effect but does not open a
-- span, so every fiber-side counter was landing without one: the chart would show a spike with
-- nothing to click through to.
--
-- Called only on ticks that have something to record, never per tick. The relay poller runs
-- four times a second and the cache refresher every two — a span each would be hundreds of
-- thousands of empty spans per pod per day, which is a cost we would be paying to describe
-- nothing happening. The events these wrap (overflow, rejected filters, decode failures) are
-- rare, so the baseline is zero and the exemplar exists exactly when someone wants to drill.
--
-- Levels are deliberately excluded. 'Metrics.liveTailCacheLoaded' records every tick and needs
-- no exemplar: nobody drills from "500 subscriptions loaded" into a trace.
liveTailEvent :: LogBase.Logger -> AuthContext -> TracerProvider -> Text -> ATBackgroundCtx () -> IO ()
liveTailEvent lg env tp name act = runBackground lg env tp (withSpan_ name [] act)


-- | Run @act@ at most once per @interval@, tracked by @ref@.
--
-- Extracted from two hand-rolled copies. The hazard it removes is specific: forgetting the
-- reset write turns a throttle into a busy loop, and nothing about the symptom points at the
-- missing line.
everyN :: NominalDiffTime -> IORef UTCTime -> IO () -> IO ()
everyN interval ref act = do
  now <- getCurrentTime
  prev <- readIORef ref
  when (diffUTCTime now prev >= interval) (writeIORef ref now >> act)


-- | Point Live Tail's @emit@ at the transport this deployment actually has.
--
-- 'System.Config.configToEnv' wires the local hub, which is right for a single process and
-- wrong the moment ingest and web are separate pods. Rebinding here rather than there is what
-- keeps a context built outside the server (tests, CLI, jobs) working without a broker.
withLiveTailTransport :: AuthContext -> IO AuthContext
withLiveTailTransport env = case env.liveTail.transport of
  LiveTail.KafkaTopic topic -> do
    producer <- Queue.getOrInitKafkaProducer env.config
    let emit e =
          void
            $ KProd.produceMessage
              producer
              KProd.ProducerRecord
                { KProd.prTopic = KT.TopicName topic
                , -- Keyed by subscription so one tail's rows keep their order within a
                  -- partition. Ordering across subscriptions is meaningless — they are
                  -- separate screens.
                  KProd.prKey = Just (encodeUtf8 e.subscriptionId.toText)
                , KProd.prPartition = KProd.UnassignedPartition
                , KProd.prValue = Just (toStrict (AE.encode e))
                , KProd.prHeaders = mempty
                }
    pure env{liveTail = env.liveTail{LiveTail.emit = emit}}
  LiveTail.PostgresRelay ->
    -- One insert per matched row would put a round trip on the ingest hot path, so the relay
    -- emit only buffers; `liveTailRelayFlusher` drains the buffer on a tick. A full buffer
    -- drops rather than blocks, for the same reason the per-browser queue does.
    pure env{liveTail = env.liveTail{LiveTail.emit = LiveTail.bufferForRelay env.liveTail.relayBuffer}}


-- | Reload the ingest pod's subscription cache, forever.
--
-- Runs wherever telemetry is processed. A failed refresh keeps the previous contents rather
-- than clearing them: subscriptions carry their own expiry, so a stale cache degrades to
-- "tails keep working until their lease would have lapsed anyway", while an emptied one would
-- black out every live tail in the fleet on a single transient query error.
liveTailCacheRefresher :: LogBase.Logger -> AuthContext -> TracerProvider -> IO ()
liveTailCacheRefresher appLogger env tp =
  liveTailFiber appLogger "subscription cache refresh" (LiveTail.cacheRefreshSecs * 1000) do
    rows <- runBackground appLogger env tp (LiveTail.activeSubscriptions LiveTail.maxCached)
    now <- getCurrentTime
    bad <- LiveTail.refreshSubCache env.liveTail.cache now rows
    -- A level, not an event: recorded every tick and outside a span on purpose.
    Metrics.recordMs Metrics.liveTailCacheLoaded (fromIntegral (length rows)) []
    unless (null bad) do
      -- The log is for us; the notice is for the person staring at a tail that will never
      -- produce another row. Without it the only symptom is silence, which reads as "nothing
      -- is happening" rather than "this stopped working".
      liveTailEvent appLogger env tp "live_tail.filters_rejected" do
        Metrics.count Metrics.liveTailCacheRejected (length bad) []
        LiveTail.noticeBrokenFilters env.liveTail bad
      liveTailLog appLogger "live_tail: stored filters no longer compile; those subscriptions will not match" (AE.object ["count" AE..= length bad])
    when (length rows >= LiveTail.maxCached)
      $ liveTailLog appLogger "live_tail: subscription cache hit its cap; some tails are not being matched" (AE.object ["cap" AE..= LiveTail.maxCached])


-- | Drain the buffered relay rows into Postgres, and sweep expired ones.
--
-- Runs on ingest pods. Batched on a tick rather than written per row: the alternative is a
-- database round trip inside the ingestion path, which is the one thing Live Tail is not
-- allowed to cost.
liveTailRelayFlusher :: LogBase.Logger -> AuthContext -> TracerProvider -> IO ()
liveTailRelayFlusher appLogger env tp =
  liveTailFiber appLogger "relay flush" LiveTail.relayPollMs do
    (pending, lost) <- LiveTail.takeRelayBuffer env.liveTail.relayBuffer
    -- Overflow means the writer fell behind ingest; the rows are gone either way, but going
    -- quietly is what makes it undiagnosable.
    when (lost > 0) do
      liveTailEvent appLogger env tp "live_tail.relay_overflow" $ Metrics.count Metrics.liveTailRelayDropped lost []
      liveTailLog appLogger "live_tail: relay buffer overflowed; those tails are missing rows" (AE.object ["dropped" AE..= lost])
    -- Guarded so an idle pod does not pay for an effect-stack setup every tick.
    unless (null pending) $ runBackground appLogger env tp (LiveTail.relayPublish pending)


-- | Poll the relay for rows addressed to this pod's connections.
--
-- Starts at the current end of the table, matching the Kafka consumer's @Latest@: a row
-- written before this pod held a connection is a row already missed, and replaying it would
-- show a "live" tail rows from minutes ago. Skips the query entirely while no connection is
-- open, so a pod nobody is tailing through costs nothing.
liveTailRelayPoller :: LogBase.Logger -> AuthContext -> TracerProvider -> IO ()
liveTailRelayPoller appLogger env tp = do
  watermark <- newIORef =<< runBackground appLogger env tp LiveTail.relayWatermark
  lastReap <- newIORef =<< getCurrentTime
  skewed <- newIORef (0 :: Int, 0 :: Int)
  lastReport <- newIORef =<< getCurrentTime
  liveTailFiber appLogger "relay poll" LiveTail.relayPollMs do
    idle <- LiveTail.hubIsEmpty env.liveTail.hub
    unless idle do
      seen <- readIORef watermark
      (results, next) <- runBackground appLogger env tp (LiveTail.relayDrain seen)
      writeIORef watermark next
      -- Same three-way handling as the Kafka consumer: a version skew is expected mid-deploy
      -- and someone else's to deliver, an undecodable row never is, and both are counted
      -- rather than silently skipped.
      for_ results \case
        LiveTail.Delivered e -> LiveTail.deliver env.liveTail.hub e
        LiveTail.VersionMismatch _ -> modifyIORef' skewed (first (+ 1))
        LiveTail.Undecodable _ -> modifyIORef' skewed (second (+ 1))
    reportSkew appLogger env tp skewed lastReport
    -- Any pod may reap; the delete is idempotent and bounded by age.
    everyN (fromIntegral LiveTail.relayRetentionSecs) lastReap
      $ runBackground appLogger env tp LiveTail.relayReap


-- | Fan the side-topic into this pod's hub.
--
-- Every web pod needs every message, because a load balancer may put any browser's SSE
-- connection on any pod — hence a consumer group unique to this process rather than a shared
-- one, which would instead split the partitions between pods and deliver each row to exactly
-- one of them (the wrong pod, most of the time).
--
-- Starts at the latest offset and never commits: there is nothing here worth resuming. A row
-- a browser was not connected to receive is a row it has already missed, and replaying it on
-- reconnect would show a "live" tail rows from minutes ago.
liveTailConsumer :: LogBase.Logger -> AuthContext -> TracerProvider -> Text -> IO ()
liveTailConsumer appLogger env tp topic = do
  nonce <- T.take 8 . UUID.toText <$> UUID.nextRandom
  let props =
        KC.brokersList (map KC.BrokerAddress env.config.kafkaBrokers)
          <> KC.groupId (KC.ConsumerGroupId ("mono_live_tail_" <> nonce))
          <> KC.clientId (KC.ClientId ("mono-live-tail-" <> nonce))
          <> KC.noAutoCommit
          <> foldMap (uncurry KC.extraProp) (Queue.kafkaSaslExtraProps env.config)
      sub = KC.topics [KT.TopicName topic] <> KC.offsetReset KC.Latest
  Safe.bracket (KC.newConsumer props sub) (either (const pass) (void . KC.closeConsumer)) \case
    Left e -> liveTailLog appLogger "live_tail: consumer failed to start" (AE.object ["error" AE..= show @Text e])
    Right c -> do
      -- Skew and corruption are counted, not logged per message: both fail for *every* message
      -- while they last, so a per-message line would be a flood proportional to ingest volume.
      -- The summary is what a human can act on — and silence here is what would turn a
      -- half-finished rolling deploy into an unexplained fleet-wide empty tail.
      skewed <- newIORef (0 :: Int, 0 :: Int) -- (version-mismatched, undecodable)
      lastReport <- newIORef =<< getCurrentTime
      forever do
        KC.pollMessage c (KT.Timeout 1000) >>= \case
          Left _ -> pass -- timeout or transient: the next poll retries
          Right rec -> forM_ (KC.crValue rec) \raw ->
            case LiveTail.decodeEnvelope raw of
              LiveTail.Delivered e -> LiveTail.deliver env.liveTail.hub e
              LiveTail.VersionMismatch _ -> modifyIORef' skewed (first (+ 1))
              LiveTail.Undecodable _ -> modifyIORef' skewed (second (+ 1))
        reportSkew appLogger env tp skewed lastReport


-- | Surface accumulated live-tail decode failures once a minute, then reset.
--
-- A version mismatch is expected mid-deploy and resolves itself; one that persists means a pod
-- is stuck on an old build and its users' tails are silently empty. Undecodable messages are
-- never expected at all.
reportSkew :: LogBase.Logger -> AuthContext -> TracerProvider -> IORef (Int, Int) -> IORef UTCTime -> IO ()
reportSkew appLogger env tp skewed lastReport = everyN 60 lastReport do
  (vers, bad) <- atomicModifyIORef' skewed ((0, 0),)
  when (vers > 0 || bad > 0) do
    -- Shared by the Kafka consumer and the relay poller, so both transports report a skew the
    -- same way. `reason` is bounded to these two values.
    liveTailEvent appLogger env tp "live_tail.decode_failed" do
      Metrics.count Metrics.liveTailDecodeFailed vers [("reason", OA.toAttribute ("skew" :: Text))]
      Metrics.count Metrics.liveTailDecodeFailed bad [("reason", OA.toAttribute ("undecodable" :: Text))]
    liveTailLog
      appLogger
      "live_tail: side-topic messages dropped; those tails are missing rows"
      (AE.object ["version_mismatched" AE..= vers, "undecodable" AE..= bad, "envelope_version" AE..= LiveTail.envelopeVersion])


-- | Serve @…/main.2c58501e.js@ from @…/main.js@ on disk — the read side of 'assetUrl',
-- against the same 'staticAssetHashes' the URL was rendered from.
--
-- The hash is verified rather than merely stripped. During a rolling deploy a request
-- for the new build's URL can land on a replica still holding the old file; answering
-- it with those bytes is what poisons the CDN for the whole (year-long) max-age, so a
-- replica that doesn't have the requested build must 404 instead. Paths that exist
-- verbatim — Vite's own hashed output, and the unhashed URLs the TS bundle falls back
-- to — are passed straight through.
hashedAssetMiddleware :: Middleware
hashedAssetMiddleware app req respond
  | not ("public/" `T.isPrefixOf` path) || HM.member path staticAssetHashes = app req respond
  | Just (onDisk, claimed) <- stripAssetHash path =
      if HM.lookup onDisk staticAssetHashes == Just claimed
        then app req{pathInfo = T.splitOn "/" onDisk, rawPathInfo = encodeUtf8 $ "/" <> onDisk} respond
        else respond $ responseLBS status404 [] ""
  | otherwise = app req respond
  where
    path = T.intercalate "/" req.pathInfo


optionsMiddleware :: Middleware
optionsMiddleware app req respond =
  if requestMethod req == "OPTIONS"
    then
      respond
        $ responseLBS
          status200
          [ ("Access-Control-Allow-Origin", "*")
          , ("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH,HEAD")
          , ("Access-Control-Allow-Headers", "*")
          ]
          ""
    else app req respond


runServer :: IOE :> es => LogBase.Logger -> AuthContext -> TracerProvider -> Eff es ()
runServer appLogger env tp = do
  loggingMiddleware <- Logging.runLog (show env.config.environment) appLogger env.config.logLevel WaiLog.mkLogMiddleware
  let server = mkServer appLogger env tp
  -- Warp's request-handler threads can outlive `waitAnyCancel` and call
  -- onException after the bulk logger has shut down; swallow that.
  -- A per-connection TimeoutThread is Warp's normal HTTP-op timeout, not a fault;
  -- logging it at attention floods the alert log during any slowdown and buries
  -- real unhandled exceptions (see the 2026-07-06 wedged-node incident).
  let onExc _ exception
        | Just (_ :: TimeoutThread) <- Safe.fromException exception = pass
        | otherwise = void $ Safe.tryAny $ runLogT "monoscope" appLogger LogAttention do
            LogBase.logAttention "Unhandled exception" (AE.object ["exception" AE..= show @String exception])
  let onExcResp _ = responseLBS status500 [("Content-Type", "text/html; charset=utf-8")] (Auth.errorPageHtml env.config 500)
  let warpSettings =
        defaultSettings
          & setPort env.config.port
          & setGracefulShutdownTimeout (Just 30)
          & setOnException onExc
          & setOnExceptionResponse onExcResp
  let compressionSettings =
        defaultGzipSettings
          { gzipFiles = GzipCompress
          , gzipSizeThreshold = 860 -- Compress responses larger than 860 bytes
          , -- Never compress an SSE stream. The size threshold alone would hold the first 860
            -- bytes of a Live Tail response waiting to decide, and a compressor sitting on a
            -- connection whose whole purpose is to deliver a row the instant it exists is the
            -- one buffering hop no response header can opt out of.
            -- Prefix, not equality: a later edit that appends a charset to the content type
            -- would otherwise silently switch compression back on.
            gzipCheckMime = \mime -> not ("text/event-stream" `BS.isPrefixOf` mime) && defaultGzipSettings.gzipCheckMime mime
          }

  let corsPolicy =
        simpleCorsResourcePolicy
          { corsOrigins = Nothing -- allow all origins
          , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH", "HEAD"]
          , corsRequestHeaders = ["*"] -- allow all headers
          , corsExposedHeaders = Nothing
          , corsMaxAge = Just 86400
          }
  otelWaiMw <- liftIO newOpenTelemetryWaiMiddleware
  let wrappedServer =
        optionsMiddleware
          . cors (const $ Just corsPolicy)
          . heartbeatMiddleware
          . gzip compressionSettings
          . otelWaiMw
          -- . loggingMiddleware
          . hashedAssetMiddleware
          $ server
  let bgJobWorker = BackgroundJobs.jobsWorkerInit appLogger env tp
      effectiveReplayBatch =
        if env.config.replayBatchSize == 0
          then max 1 (env.config.messagesPerPubsubPullBatch `div` 2)
          else env.config.replayBatchSize
  let logExc = logException (show env.config.environment) appLogger env.config.logLevel
  -- Extraction worker shard fibers. Each shard runs `processEagerBatch` per
  -- batch inside its own `runBackground` effect stack. The error-decay fiber
  -- owns propagateMergedCounts/updateOccurrenceCounts on a 1-minute tick.
  let runEager batch shard = void $ runBackground appLogger env tp $ BackgroundJobs.processEagerBatch batch shard
  let shardsIndexed = zip [0 :: Int ..] (V.toList env.extractionWorker.shards)
  let fiber name = async . supervise logExc name
  -- Per-instance buffer drainers: the extraction / schema-learning pipeline
  -- flushes the spans THIS instance ingested, so it must run anywhere ingestion
  -- runs — including CONSUMER_ONLY replicas (schema learning powers stats/facets).
  let schemaFibers =
        [ fiber ("extraction-worker-" <> show i) $ ExtractionWorker.runShardWorker (logExc ("extraction-worker-" <> show i)) runEager shard
        | (i, shard) <- shardsIndexed
        ]
          <> [fiber ("drain-flusher-" <> show i) $ BackgroundJobs.runDrainFlusher appLogger env tp shard | (i, shard) <- shardsIndexed]
          <> [fiber ("rehydration-worker-" <> show i) $ ExtractionWorker.runRehydrationWorker (logExc ("rehydration-worker-" <> show i)) shard | (i, shard) <- shardsIndexed]
          <> [ fiber "drain-age-flush" $ BackgroundJobs.runDrainAgeFlushTimer appLogger env
             , fiber "schema-flusher" $ void $ BackgroundJobs.runSchemaFlusherFiber appLogger env tp
             ]
  -- Global periodic-job timers operating on shared DB state — single-owner,
  -- so they belong with the main server and are skipped on CONSUMER_ONLY replicas.
  let jobFibers =
        [ fiber "error-decay" $ BackgroundJobs.runErrorDecayFiber appLogger env tp
        , fiber "session-backfill" $ BackgroundJobs.runSessionBackfillTimer appLogger env tp
        ]
  let consumerOnly = env.config.consumerOnly -- CONSUMER_ONLY: queue consumers + schema-learning workers; skip Warp/gRPC/odd-jobs/global-timers
  -- Live Tail splits across two roles, and one process can hold both. Matching needs the
  -- subscription cache wherever telemetry is processed; delivery needs the side-topic
  -- consumer wherever a browser can connect.
  --
  -- The cache and the relay writer are deliberately __unconditional__. They used to be gated
  -- on @enableKafkaService || enablePubsubService || enableOtlpGrpcService@, which is a guess
  -- at "does this process ingest?" — and it is wrong for OTLP-over-HTTP, which arrives on the
  -- ordinary web listener with none of those flags set. A pod that ingests without a refreshed
  -- cache matches every record against an empty subscription list, so the tail reports @live@
  -- and stays empty forever. This is the same lesson as 'LiveTail.Transport': the failure was
  -- never the cost of running the fiber, it was inferring the topology. An idle cache refresh
  -- is one indexed query every 'cacheRefreshSecs' returning nothing, and the relay flusher on
  -- an empty buffer does no work at all.
      liveTailKafkaTopic = case env.liveTail.transport of LiveTail.KafkaTopic t -> Just t; LiveTail.PostgresRelay -> Nothing
      liveTailRelay = env.liveTail.transport == LiveTail.PostgresRelay
  -- Always accept hand-offs: every ingesting instance (incl. CONSUMER_ONLY) feeds its extraction worker.
  liftIO $ atomically $ writeTVar env.extractionWorker.acceptingBatches True
  -- Runtime profiling switch: SIGUSR2 toggles both the CPU sampling windows
  -- and the heap-census timer without a restart (`docker kill -s USR2 <ctr>`).
  -- Starts DISABLED — the RTS timers from GHCRTS (-p, -hc -i60) are stopped at
  -- boot and armed on the first SIGUSR2 after deployment. A no-op in vanilla
  -- dev builds (rts_isProfiled = 0).
  profilingOn <- liftIO $ newTVarIO False
  when (rtsIsProfiled /= 0) $ liftIO $ stopProfTimer >> stopHeapProfTimer
  void
    $ liftIO
    $ installHandler
      sigUSR2
      ( Catch do
          nowOn <- atomically $ modifyTVar' profilingOn not >> readTVar profilingOn
          if nowOn then startHeapProfTimer else stopProfTimer >> stopHeapProfTimer
          logExc "profiling" $ if nowOn then "enabled via SIGUSR2" else "disabled via SIGUSR2"
      )
      Nothing
  asyncs <-
    liftIO
      $ sequenceA
      $ catMaybes
      $ [ -- intentionally unsupervised: Warp crash triggers waitAnyCancel → process exit.
          -- The exception is logged here because `awaitShutdown` discards fiber results,
          -- so an unlogged Warp death is a silent crash-loop (2026-08-04 incident: healthy
          -- replicas dropped their listener every ~10-50min with nothing in the logs).
          guard (not consumerOnly) $> async ((runSettings warpSettings wrappedServer >> logExc "warp" "runSettings returned cleanly") `Safe.withException` \(e :: SomeException) -> logExc "warp" ("runSettings threw: " <> show e))
        , guard env.config.enablePubsubService $> async (supervise logExc "pubsub" $ Queue.pubsubService appLogger env tp env.config.requestPubsubTopics processMessages)
        , guard (not consumerOnly) $> async (supervise logExc "background-jobs" bgJobWorker)
        , guard (not consumerOnly && env.config.enableOtlpGrpcService) $> async (supervise logExc "otlp-grpc" $ OtlpServer.runServer appLogger env tp)
        , -- TWO identical ingest consumers per node, on purpose (commit 4ee5350a, "Extra
          -- kafka processor fiber per node"): they join the same consumer group, so the
          -- broker splits the partitions between them and the node gets a second
          -- independent poll/decode/insert pipeline. Not a copy-paste — deleting either
          -- line halves this node's ingest throughput.
          guard (env.config.enableKafkaService && not (any T.null env.config.kafkaTopics)) $> async (supervise logExc "kafka" $ Queue.kafkaService appLogger env tp Queue.KafkaPrimary "ingest" env.config.kafkaDeadLetterTopic env.config.kafkaTopics env.config.messagesPerPubsubPullBatch OtlpServer.processList)
        , guard (env.config.enableKafkaService && not (any T.null env.config.kafkaTopics)) $> async (supervise logExc "kafka" $ Queue.kafkaService appLogger env tp Queue.KafkaPrimary "ingest" env.config.kafkaDeadLetterTopic env.config.kafkaTopics env.config.messagesPerPubsubPullBatch OtlpServer.processList)
        , -- Small batch: DLQ replay is a retry carousel, not steady-state ingest;
          -- a poison-heavy batch should not stall a large offset window.
          guard (env.config.enableKafkaService && env.config.enableKafkaDeadLetterService && not (T.null env.config.kafkaDeadLetterTopic)) $> async (supervise logExc "kafka-dlq" $ Queue.kafkaService appLogger env tp Queue.KafkaDlqReplay "dlq" env.config.kafkaDeadLetterTopic (map fst (Queue.retryTiers env.config.kafkaDeadLetterTopic)) 1000 OtlpServer.processList)
        , guard env.config.enableReplayService $> async (supervise logExc "kafka-replay" $ Queue.kafkaService appLogger env tp Queue.KafkaPrimary "replay" env.config.rrwebDeadLetterTopic env.config.rrwebTopics effectiveReplayBatch processReplayEvents)
        , guard (rtsIsProfiled /= 0) $> async (supervise logExc "cpu-profiler" $ cpuProfileCycler profilingOn)
        , -- Matching happens wherever telemetry lands, and every process is a place telemetry
          -- can land — OTLP/HTTP needs no ingest flag at all. Unconditional beats guessing.
          Just $ async (supervise logExc "live-tail-cache" $ liveTailCacheRefresher appLogger env tp)
        , -- Relay writer follows ingest, reader follows HTTP; both are no-ops on the Kafka
          -- transport, which carries the rows itself. Neither is gated on a role: the writer
          -- flushes an empty buffer for free, and the reader skips its query outright while
          -- this pod's hub has no connections ('hubIsEmpty').
          guard liveTailRelay $> async (supervise logExc "live-tail-relay-write" $ liveTailRelayFlusher appLogger env tp)
        , guard liveTailRelay $> async (supervise logExc "live-tail-relay-read" $ liveTailRelayPoller appLogger env tp)
        , -- Delivery happens wherever the browser lands, so the consumer follows HTTP. Kafka
          -- transport only: with the local hub, publish and deliver are already the same
          -- process and a consumer would deliver a second copy of every row.
          [ async (supervise logExc "live-tail-consumer" $ liveTailConsumer appLogger env tp t)
          | not consumerOnly
          , Just t <- [liveTailKafkaTopic]
          ]
            & listToMaybe
        ]
      <> fmap Just schemaFibers
      <> (if consumerOnly then [] else fmap Just jobFibers)
  -- The hard-exit watchdog calls exitImmediately (_exit), which under ghcid
  -- (`make live-reload`) would kill the whole `cabal repl` process on every
  -- reload's interrupt-driven teardown. Only arm it in deployed envs.
  liftIO $ awaitShutdown (logExc "shutdown") (env.config.environment /= Dev) asyncs


-- | Wall-clock budget for tearing every service fiber down once shutdown
-- begins. A backstop only: a well-behaved fiber dies on the first
-- AsyncCancelled, so this caps the wait when one is wedged (uninterruptible
-- librdkafka poll, Warp mid-drain) and guarantees the process eventually exits.
shutdownDeadlineUs :: Int
shutdownDeadlineUs = 15_000_000


-- | Hard backstop from the moment shutdown begins to a guaranteed process exit.
-- Generous enough for a normal graceful drain (buffer flush + pool close, ~20-25s)
-- but bounded so a wedged fiber reap / kafka-producer close / pool destroy can't
-- leave a zombie — a live process whose Warp socket is already closed still holds
-- its swarm VIP slot and black-holes 1/N of traffic (2026-07-06 incident).
hardExitDeadlineUs :: Int
hardExitDeadlineUs = 40_000_000


-- | Block until either a service fiber exits on its own or we receive
-- SIGINT/SIGTERM, then cancel every fiber. Replaces async's @waitAnyCancel@,
-- whose @mapM_ cancel@ cancels sequentially and blocks on each fiber's death
-- before signalling the next — so one slow fiber (Warp's graceful drain, a
-- stuck poll) kept every other fiber alive, which is why Ctrl-C left the kafka
-- workers retrying dead-DB writes instead of exiting. Catching SIGTERM also lets
-- @docker stop@/k8s drain gracefully (flush buffers, close pools) rather than
-- SIGKILL-dropping in-flight data.
--
-- The cancellation step runs in 'finally', not sequenced after the 'race':
-- an exception landing on this thread while blocked in 'race' (e.g. ghcid's
-- interrupt-then-reload on `make live-reload`, which throws 'UserInterrupt'
-- into the running `:run Start.startApp`) used to skip straight past
-- 'cancelAllConcurrently', leaking every fiber — including the OTLP gRPC
-- listener — as an orphaned thread with its socket still bound. The next
-- reload's `otlp-grpc` fiber then failed to bind port 4317 ("Address already
-- in use") and, being wrapped in 'supervise', retried that failure forever.
awaitShutdown :: (Text -> IO ()) -> Bool -> [Async a] -> IO ()
awaitShutdown logEnd hardExit asyncs = do
  stop <- newEmptyMVar
  for_ [sigINT, sigTERM] \s -> installHandler s (Catch (void (tryPutMVar stop ()))) Nothing
  -- Arm the hard-exit watchdog the instant shutdown starts (a fiber died or a
  -- signal arrived). It outlives the fiber reap and 'shutdownMonoscope' cleanup
  -- that run after this returns, so even if one of those wedges the process still
  -- dies and Swarm gets a clean restart instead of a black-holing zombie.
  -- Skipped under ghcid/dev ('hardExit' False): 'exitImmediately' is _exit and
  -- would kill the shared 'cabal repl' process on every reload's teardown.
  let teardown = do
        when hardExit $ void $ forkIO $ threadDelay hardExitDeadlineUs >> exitImmediately (ExitFailure 1)
        cancelAllConcurrently shutdownDeadlineUs asyncs
  -- Say WHY we are shutting down: signal vs a service fiber ending (and how it
  -- ended). Fiber results are otherwise discarded, which made crashes invisible.
  -- tryAsync (not try): an ASYNC exception landing on this thread — e.g. a ki
  -- background-scope child dying — is otherwise never logged, because the rethrow
  -- races the hard-exit watchdog while the ki scope reap blocks on a wedged
  -- librdkafka fiber, so the process dies before the RTS can print it
  -- (2026-08-05 silent crash-loop).
  ( Safe.tryAsync (race (takeMVar stop) (waitAnyCatch asyncs)) >>= \case
      Left (e :: SomeException) -> logEnd ("shutdown thread killed by: " <> show e) >> Safe.throwIO e
      Right (Left ()) -> logEnd "SIGINT/SIGTERM received; shutting down"
      Right (Right (_, Right _)) -> logEnd "a service fiber exited cleanly; shutting down"
      Right (Right (_, Left e)) -> logEnd ("a service fiber died: " <> show e <> "; shutting down")
    )
    `Safe.finally` teardown


-- | Cancel every fiber CONCURRENTLY (vs async's sequential @mapM_ cancel@),
-- bounded by @deadlineUs@ so a fiber that refuses to die can't hang shutdown.
cancelAllConcurrently :: Int -> [Async a] -> IO ()
cancelAllConcurrently deadlineUs asyncs = void $ timeout deadlineUs $ mapConcurrently_ cancel asyncs


instance FromHttpApiData ByteString where
  parseUrlPiece = Right . encodeUtf8


mkServer :: LogBase.Logger -> AuthContext -> TracerProvider -> Servant.Application
mkServer logger env tp = do
  genericServeTWithContext
    (effToServantHandler env logger tp)
    -- OTLP/HTTP handlers injected here: Routes can't import OtlpServer
    -- (proto-lens orphan IsLabel instances clash with generic-lens #labels)
    (Routes.server logger env tp (OtlpServer.httpTracesExport logger env tp) (OtlpServer.httpLogsExport logger env tp))
    (Routes.genAuthServerContext logger env)


shutdownMonoscope :: AuthContext -> Eff '[IOE] ()
shutdownMonoscope env =
  liftIO $ do
    -- Phase A: stop accepting new batches; wait for in-flight ingress to drain.
    atomically $ writeTVar env.extractionWorker.acceptingBatches False
    awaitDrained env.extractionWorker 500_000 10
    -- Phase B: force-flush drain buffers so buffered spans get pattern-tagged.
    now <- getCurrentTime
    ExtractionWorker.forceFlushAllBuffers env.extractionWorker now
    awaitDrained env.extractionWorker 500_000 5
    Queue.closeSharedKafkaProducer
    Pool.destroyAllResources env.pool
    Pool.destroyAllResources env.jobsPool
    Pool.destroyAllResources env.timefusionPgPool
  where
    awaitDrained worker delayUs budget
      | budget <= (0 :: Int) = pass
      | otherwise = do
          drained <- ExtractionWorker.allQueuesDrained worker
          unless drained $ threadDelay delayUs >> awaitDrained worker delayUs (budget - 1)


-- | Non-zero iff the binary was built the profiling way (production Dockerfile:
-- @profiling-detail: late@). Vanilla dev builds return 0, so the cpu-profiler
-- fiber never starts locally.
foreign import ccall unsafe "rts_isProfiled" rtsIsProfiled :: Int


-- | Duty-cycled always-on CPU profiling, mirroring timefusion's rolling 60s
-- pprof windows. The RTS time profiler (+RTS -p, from the image's GHCRTS)
-- streams cost-centre samples into the eventlog; sampling continuously would
-- grow it by GBs/day, so run a 60s window every 15 min. Heap censuses
-- (-hc -i60) are independent of this timer and stay on continuously.
-- Parks while the SIGUSR2 kill switch is off (a mid-window disable stops
-- samples immediately via the handler's stopProfTimer; the parked check here
-- just prevents new windows). Analyze off-host: hs-speedscope (CPU),
-- eventlog2html (heap).
cpuProfileCycler :: TVar Bool -> IO ()
cpuProfileCycler enabled = forever do
  atomically $ check =<< readTVar enabled
  startProfTimer
  threadDelay 60_000_000
  stopProfTimer
  threadDelay (14 * 60_000_000)


logException :: Text -> LogBase.Logger -> LogLevel -> Text -> Text -> IO ()
logException envTxt logger logLevel name msg =
  runLogT envTxt logger logLevel
    $ LogBase.logAttention ("Service thread " <> name) msg


-- | Supervisor: restarts on crash with 1s delay. Rethrows async exceptions for
-- shutdown, except `TimeoutThread` (per-HTTP-op timeout, not a shutdown signal).
supervise :: (Text -> Text -> IO ()) -> Text -> IO () -> IO ()
supervise logExc name action = forever $ do
  Safe.tryAsync action >>= \case
    Right () -> logExc name "exited cleanly, restarting"
    Left e
      | Just (_ :: TimeoutThread) <- Safe.fromException e -> logExc name ("recoverable timeout: " <> show @Text e)
      | Safe.isAsyncException e -> Safe.throwIO e
      | otherwise -> logExc name ("crashed: " <> show @Text e)
  threadDelay 1_000_000


heartbeatMiddleware :: Middleware
heartbeatMiddleware app req sendResponse =
  case rawPathInfo req of
    "/heartbeat" ->
      if getVerb
        then heartbeat
        else app req sendResponse
    _ -> app req sendResponse
  where
    getVerb = (requestMethod req == methodGet) || (requestMethod req == methodHead)
    heartbeat = sendResponse $ responseLBS status200 [("Content-Type", "text/plain")] "Ok"
