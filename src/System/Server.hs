module System.Server (runMonoscope, mkServer, cancelAllConcurrently) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently_, race, waitAny)
import Control.Concurrent.STM (check)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Pool as Pool (destroyAllResources)
import Data.Text qualified as T
import Data.Time.Clock qualified
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Fail (runFailIO)
import Effectful.Ki qualified as Ki
import Effectful.Time (runTime)
import GHC.Profiling (startHeapProfTimer, startProfTimer, stopHeapProfTimer, stopProfTimer)
import Log (LogLevel (..), runLogT)
import Log qualified as LogBase
import Network.HTTP.Types (methodGet, methodHead, status200, status500)
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setOnException, setOnExceptionResponse, setPort)
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip (GzipFiles (..), GzipSettings (..), defaultGzipSettings, gzip)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace (TracerProvider)
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.Replay (processReplayEvents)
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.Queue qualified as Queue
import ProcessMessage (processMessages)
import Relude
import Servant (FromHttpApiData (..))
import Servant qualified
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (
  AuthContext (backgroundScope, config, extractionWorker, jobsPool, pool, timefusionPgPool),
  EnvConfig (..),
  getAppContext,
 )
import System.Exit (ExitCode (ExitFailure))
import System.Logging qualified as Logging
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM, sigUSR2)
import System.TimeManager (TimeoutThread)
import System.Timeout (timeout)
import System.Types (effToServantHandler, runBackground)
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
      withLogger \l -> runServer l env{backgroundScope = Just backgroundScope} tp


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
          $ server
  let bgJobWorker = BackgroundJobs.jobsWorkerInit appLogger env tp
      effectiveReplayBatch =
        if env.config.replayBatchSize == 0
          then max 1 (env.config.messagesPerPubsubPullBatch `div` 2)
          else env.config.replayBatchSize
  let logExc = logException env.config.environment appLogger env.config.logLevel
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
      $ [ guard (not consumerOnly) $> async (runSettings warpSettings wrappedServer) -- intentionally unsupervised: Warp crash triggers waitAnyCancel → process exit
        , guard env.config.enablePubsubService $> async (supervise logExc "pubsub" $ Queue.pubsubService appLogger env tp env.config.requestPubsubTopics processMessages)
        , guard (not consumerOnly) $> async (supervise logExc "background-jobs" bgJobWorker)
        , guard (not consumerOnly) $> async (supervise logExc "otlp-grpc" $ OtlpServer.runServer appLogger env tp)
        , guard (env.config.enableKafkaService && not (any T.null env.config.kafkaTopics)) $> async (supervise logExc "kafka" $ Queue.kafkaService appLogger env tp Queue.KafkaPrimary "ingest" env.config.kafkaTopics env.config.messagesPerPubsubPullBatch OtlpServer.processList)
        , guard (env.config.enableKafkaService && not (any T.null env.config.kafkaTopics)) $> async (supervise logExc "kafka" $ Queue.kafkaService appLogger env tp Queue.KafkaPrimary "ingest" env.config.kafkaTopics env.config.messagesPerPubsubPullBatch OtlpServer.processList)
        , -- Small batch: DLQ replay is a retry carousel, not steady-state ingest;
          -- a poison-heavy batch should not stall a large offset window.
          guard (env.config.enableKafkaService && env.config.enableKafkaDeadLetterService && not (T.null env.config.kafkaDeadLetterTopic)) $> async (supervise logExc "kafka-dlq" $ Queue.kafkaService appLogger env tp Queue.KafkaDlqReplay "dlq" (map fst (Queue.retryTiers env.config.kafkaDeadLetterTopic)) 1000 OtlpServer.processList)
        , guard env.config.enableReplayService $> async (supervise logExc "kafka-replay" $ Queue.kafkaService appLogger env tp Queue.KafkaPrimary "replay" env.config.rrwebTopics effectiveReplayBatch processReplayEvents)
        , guard (rtsIsProfiled /= 0) $> async (supervise logExc "cpu-profiler" $ cpuProfileCycler profilingOn)
        ]
      <> fmap Just schemaFibers
      <> (if consumerOnly then [] else fmap Just jobFibers)
  liftIO $ awaitShutdown asyncs


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
awaitShutdown :: [Async a] -> IO ()
awaitShutdown asyncs = do
  stop <- newEmptyMVar
  for_ [sigINT, sigTERM] \s -> installHandler s (Catch (void (tryPutMVar stop ()))) Nothing
  -- Arm the hard-exit watchdog the instant shutdown starts (a fiber died or a
  -- signal arrived). It outlives the fiber reap and 'shutdownMonoscope' cleanup
  -- that run after this returns, so even if one of those wedges the process still
  -- dies and Swarm gets a clean restart instead of a black-holing zombie.
  let teardown = do
        void $ forkIO $ threadDelay hardExitDeadlineUs >> exitImmediately (ExitFailure 1)
        cancelAllConcurrently shutdownDeadlineUs asyncs
  void (race (takeMVar stop) (waitAny asyncs)) `Safe.finally` teardown


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
    now <- Data.Time.Clock.getCurrentTime
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
