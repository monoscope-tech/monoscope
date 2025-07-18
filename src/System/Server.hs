{-# LANGUAGE RankNTypes #-}

module System.Server (runAPItoolkit) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Pool as Pool (destroyAllResources)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Fail (runFailIO)
import Effectful.Time (runTime)
import Log qualified
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setOnException, setPort)
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Trace (TracerProvider)
import Opentelemetry.OtlpServer qualified as OtlpServer
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


runAPItoolkit :: TracerProvider -> IO ()
runAPItoolkit tp =
  Safe.bracket
    (getAppContext & runFailIO & runEff)
    (runEff . shutdownAPItoolkit)
    \env -> runEff . runTime . runConcurrent $ do
      let baseURL = "http://localhost:" <> show env.config.port
      liftIO $ blueMessage $ "Starting APItoolkit server on " <> baseURL
      let withLogger = Logging.makeLogger env.config.loggingDestination
      withLogger \l -> runServer l env tp


runServer :: IOE :> es => Log.Logger -> AuthContext -> TracerProvider -> Eff es ()
runServer appLogger env tp = do
  loggingMiddleware <- Logging.runLog (show env.config.environment) appLogger WaiLog.mkLogMiddleware
  let server = mkServer appLogger env tp
  let warpSettings =
        defaultSettings
          & setPort env.config.port
          & setOnException \mRequest exception -> Log.runLogT "apitoolkit" appLogger Log.LogAttention $ do
            Log.logAttention "Unhandled exception" $ AE.object ["exception" AE..= show @String exception]
            Safe.throw exception
  let wrappedServer =
        heartbeatMiddleware
          . newOpenTelemetryWaiMiddleware' tp
          -- . loggingMiddleware
          -- . const
          $ server
  let bgJobWorker = BackgroundJobs.jobsWorkerInit appLogger env tp

  -- let ojStartArgs =
  --       OJCli.UIStartArgs
  --         { uistartAuth = OJCli.AuthNone
  --         , uistartPort = 8081
  --         }

  -- let ojLogger logLevel logEvent = logger <& show (logLevel, logEvent)
  -- let ojTable = "background_jobs" :: OJTypes.TableName
  -- let ojCfg = OJConfig.mkUIConfig ojLogger ojTable poolConn id
  let exceptionLogger = logException env.config.environment appLogger
  asyncs <-
    liftIO
      $ sequence
      $ concat @[]
        [ [async $ runSettings warpSettings wrappedServer]
        , -- , [async $ OJCli.defaultWebUI ojStartArgs ojCfg] -- Uncomment or modify as needed
          [async $ Safe.withException (Queue.pubsubService appLogger env tp env.config.requestPubsubTopics processMessages) exceptionLogger | env.config.enablePubsubService]
        , [async $ Safe.withException bgJobWorker exceptionLogger | env.config.enableBackgroundJobs]
        , [async $ Safe.withException (OtlpServer.runServer appLogger env tp) exceptionLogger]
        , [async $ Safe.withException (Queue.kafkaService appLogger env tp OtlpServer.processList) exceptionLogger | env.config.enableKafkaService && (not . any T.null) env.config.kafkaTopics]
        , [async $ Safe.withException (Queue.kafkaService appLogger env tp OtlpServer.processList) exceptionLogger | env.config.enableKafkaService && (not . any T.null) env.config.kafkaTopics]
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


shutdownAPItoolkit :: AuthContext -> Eff '[IOE] ()
shutdownAPItoolkit env =
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
