module System.Server (runMonoscope) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent (threadDelay)
import Data.Time.Clock qualified
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Pool as Pool (destroyAllResources)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Fail (runFailIO)
import Effectful.Time (runTime)
import Log (LogLevel (..), runLogT)
import Log qualified as LogBase
import Network.HTTP.Types (methodGet, methodHead, status200, status500)
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setOnException, setOnExceptionResponse, setPort)
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip (GzipFiles (..), GzipSettings (..), defaultGzipSettings, gzip)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
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
  AuthContext (config, extractionWorker, jobsPool, pool, timefusionPgPool),
  EnvConfig (..),
  getAppContext,
 )
import System.Logging qualified as Logging
import System.Types (effToServantHandler, runBackground)
import Web.Auth qualified as Auth
import Web.Routes qualified as Routes


runMonoscope :: TracerProvider -> IO ()
runMonoscope tp =
  Safe.bracket
    (getAppContext & runFailIO & runEff)
    (runEff . shutdownMonoscope)
    \env -> runEff . runTime . runConcurrent $ do
      let baseURL = "http://localhost:" <> show env.config.port
      liftIO $ blueMessage $ "Starting Monoscope server on " <> baseURL
      let withLogger = Logging.makeLogger env.config.loggingDestination
      withLogger \l -> runServer l env tp


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
  let onExc _ exception = runLogT "monoscope" appLogger LogAttention do
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
  let wrappedServer =
        optionsMiddleware
          . cors (const $ Just corsPolicy)
          . heartbeatMiddleware
          . gzip compressionSettings
          . newOpenTelemetryWaiMiddleware' tp
          -- . loggingMiddleware
          $ server
  let bgJobWorker = BackgroundJobs.jobsWorkerInit appLogger env tp
  let logExc = logException env.config.environment appLogger env.config.logLevel
  -- Extraction worker shard fibers. Each shard runs `processEagerBatch` per
  -- batch inside its own `runBackground` effect stack. The error-decay fiber
  -- owns propagateMergedCounts/updateOccurrenceCounts on a 1-minute tick.
  let runEager batch shard = void $ runBackground appLogger env tp $ BackgroundJobs.processEagerBatch batch shard
  let shardsIndexed = zip [0 :: Int ..] (V.toList env.extractionWorker.shards)
  let fiber name = async . supervise logExc name
  let workerFibers =
        [ fiber ("extraction-worker-" <> show i) $ ExtractionWorker.runShardWorker (logExc ("extraction-worker-" <> show i)) runEager shard
        | (i, shard) <- shardsIndexed
        ]
          <> [fiber ("drain-flusher-" <> show i) $ BackgroundJobs.runDrainFlusher appLogger env tp shard | (i, shard) <- shardsIndexed]
          <> [fiber ("rehydration-worker-" <> show i) $ ExtractionWorker.runRehydrationWorker (logExc ("rehydration-worker-" <> show i)) shard | (i, shard) <- shardsIndexed]
          <> [ fiber "drain-age-flush" $ BackgroundJobs.runDrainAgeFlushTimer appLogger env
             , fiber "error-decay" $ BackgroundJobs.runErrorDecayFiber appLogger env tp
             ]
  liftIO $ atomically $ writeTVar env.extractionWorker.acceptingBatches True
  asyncs <-
    liftIO
      $ sequenceA
      $ catMaybes
      $ [ Just $ async $ runSettings warpSettings wrappedServer -- intentionally unsupervised: Warp crash triggers waitAnyCancel → process exit
        , guard env.config.enablePubsubService $> async (supervise logExc "pubsub" $ Queue.pubsubService appLogger env tp env.config.requestPubsubTopics processMessages)
        , Just $ async $ supervise logExc "background-jobs" bgJobWorker
        , Just $ async $ supervise logExc "otlp-grpc" $ OtlpServer.runServer appLogger env tp
        , guard (env.config.enableKafkaService && not (any T.null env.config.kafkaTopics)) $> async (supervise logExc "kafka" $ Queue.kafkaService appLogger env tp env.config.kafkaTopics OtlpServer.processList)
        , guard env.config.enableReplayService $> async (supervise logExc "kafka-replay" $ Queue.kafkaService appLogger env tp env.config.rrwebTopics processReplayEvents)
        ]
      <> fmap Just workerFibers
  void $ liftIO $ waitAnyCancel asyncs


instance FromHttpApiData ByteString where
  parseUrlPiece = Right . encodeUtf8


mkServer :: LogBase.Logger -> AuthContext -> TracerProvider -> Servant.Application
mkServer logger env tp = do
  genericServeTWithContext
    (effToServantHandler env logger tp)
    (Routes.server env.pool)
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


logException :: Text -> LogBase.Logger -> LogLevel -> Text -> Text -> IO ()
logException envTxt logger logLevel name msg =
  runLogT envTxt logger logLevel
    $ LogBase.logAttention ("Service thread " <> name) msg


-- | Supervisor: restarts a service on crash with 1s delay. Re-throws async exceptions for clean shutdown.
supervise :: (Text -> Text -> IO ()) -> Text -> IO () -> IO ()
supervise logExc name action = forever $ do
  Safe.tryAny action >>= \case
    Right () -> logExc name "exited cleanly, restarting"
    Left e | Safe.isAsyncException e -> Safe.throwIO e
    Left e -> logExc name ("crashed: " <> show @Text e)
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
