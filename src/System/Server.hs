{-# LANGUAGE RankNTypes #-}

module System.Server (runMonoscope) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Pool as Pool (destroyAllResources)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Fail (runFailIO)
import Effectful.Time (runTime)
import Log qualified
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnException, setPort)
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Gzip (GzipFiles (..), GzipSettings (..), def, gzip)
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Trace (TracerProvider)
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.Replay (processReplayEvents)
import Pkg.Queue qualified as Queue
import ProcessMessage (processMessages)
import Relude
import Servant (FromHttpApiData (..))
import Servant qualified
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (
  AuthContext (config, jobsPool, pool, timefusionPgPool),
  EnvConfig (..),
  getAppContext,
 )
import System.Logging qualified as Logging
import System.Types (effToServantHandler)
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


runServer :: IOE :> es => Log.Logger -> AuthContext -> TracerProvider -> Eff es ()
runServer appLogger env tp = do
  loggingMiddleware <- Logging.runLog (show env.config.environment) appLogger WaiLog.mkLogMiddleware
  let server = mkServer appLogger env tp
  let warpSettings =
        defaultSettings
          & setPort env.config.port
          & setOnException \_ exception ->
            Log.runLogT "monoscope" appLogger Log.LogAttention
              $ Log.logAttention "Unhandled exception"
              $ AE.object ["exception" AE..= show @String exception]
  let compressionSettings =
        def
          { gzipFiles = GzipCompress
          , gzipSizeThreshold = 860 -- Compress responses larger than 860 bytes
          }
  let wrappedServer =
        heartbeatMiddleware
          . gzip compressionSettings
          . newOpenTelemetryWaiMiddleware' tp
          -- . loggingMiddleware
          $ server
  let bgJobWorker = BackgroundJobs.jobsWorkerInit appLogger env tp
  let exceptionLogger = logException env.config.environment appLogger
  asyncs <-
    liftIO
      $ sequence
      $ concat
        [ [async $ runSettings warpSettings wrappedServer]
        , [async $ Safe.withException (Queue.pubsubService appLogger env tp env.config.requestPubsubTopics processMessages) exceptionLogger | env.config.enablePubsubService]
        , [async $ Safe.withException bgJobWorker exceptionLogger]
        , [async $ Safe.withException (OtlpServer.runServer appLogger env tp) exceptionLogger]
        , [async $ Safe.withException (Queue.kafkaService appLogger env tp env.config.kafkaTopics OtlpServer.processList) exceptionLogger | env.config.enableKafkaService && (not . any T.null) env.config.kafkaTopics]
        , [async $ Safe.withException (Queue.kafkaService appLogger env tp env.config.rrwebTopics processReplayEvents) exceptionLogger | env.config.enableReplayService]
        ]
  void $ liftIO $ waitAnyCancel asyncs


instance FromHttpApiData ByteString where
  parseUrlPiece = Right . encodeUtf8


mkServer :: Log.Logger -> AuthContext -> TracerProvider -> Servant.Application
mkServer logger env tp = do
  genericServeTWithContext
    (effToServantHandler env logger tp)
    (Routes.server env.pool)
    (Routes.genAuthServerContext logger env)


shutdownMonoscope :: AuthContext -> Eff '[IOE] ()
shutdownMonoscope env =
  liftIO $ do
    Pool.destroyAllResources env.pool
    Pool.destroyAllResources env.jobsPool
    Pool.destroyAllResources env.timefusionPgPool


logException :: Text -> Log.Logger -> Safe.SomeException -> IO ()
logException envTxt logger exception =
  runEff
    . runTime
    . Logging.runLog envTxt logger
    $ Log.logAttention "odd-jobs runner crashed " (show @Text exception)
