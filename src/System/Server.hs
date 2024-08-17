{-# LANGUAGE RankNTypes #-}

module System.Server (runAPItoolkit) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Exception.Safe qualified as Safe
import Control.Lens ((^?), _Just)
import Control.Lens qualified as L
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Base64 qualified as LB64
import Data.Generics.Product (field)
import Data.Pool as Pool (destroyAllResources)
import Data.Text.Lazy.Encoding qualified as LT
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Fail (runFailIO)
import Effectful.Time (runTime)
import Gogol qualified as Google
import Gogol.Auth.ApplicationDefault qualified as Google
import Gogol.Data.Base64 (_Base64)
import Gogol.PubSub qualified as PubSub
import Log qualified
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setOnException,
  setPort,
 )
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Opentelemetry.OtlpServer qualified as OtlpServer
import ProcessMessage (processMessages)
import Relude
import Pkg.Queue qualified as Queue
import Servant qualified
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (
  AuthContext (config, jobsPool, pool),
  EnvConfig (
    enableBackgroundJobs,
    enablePubsubService,
    environment,
    googleServiceAccountB64,
    loggingDestination,
    messagesPerPubsubPullBatch,
    port,
    requestPubsubTopics,
    otlpStreamTopics
  ),
  getAppContext,
 )
import System.Logging qualified as Logging
import System.Types (effToServantHandler, runBackground)
import UnliftIO.Exception (tryAny)
import Web.Routes qualified as Routes


runAPItoolkit :: IO ()
runAPItoolkit =
  Safe.bracket
    (getAppContext & runFailIO & runEff)
    (runEff . shutdownAPItoolkit)
    \env -> runEff . runTime . runConcurrent $ do
      let baseURL = "http://localhost:" <> show env.config.port
      liftIO $ blueMessage $ "Starting APItoolkit server on " <> baseURL
      let withLogger = Logging.makeLogger env.config.loggingDestination
      withLogger (`runServer` env)


runServer :: IOE :> es => Log.Logger -> AuthContext -> Eff es ()
runServer appLogger env = do
  loggingMiddleware <- Logging.runLog (show env.config.environment) appLogger WaiLog.mkLogMiddleware
  let server = mkServer appLogger env
  let warpSettings =
        defaultSettings
          & setPort env.config.port
          & setOnException \mRequest exception -> Log.runLogT "apitoolkit" appLogger Log.LogAttention $ do
            Log.logAttention "Unhandled exception" $ Aeson.object ["exception" Aeson..= show @String exception]
            Safe.throw exception

  let wrappedServer =
        heartbeatMiddleware
          . loggingMiddleware
          . const
          $ server
  let bgJobWorker = BackgroundJobs.jobsWorkerInit appLogger env

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
          [async $ Safe.withException (Queue.pubsubService appLogger env env.config.requestPubsubTopics processMessages) exceptionLogger | env.config.enablePubsubService]
        , [async $ Safe.withException bgJobWorker exceptionLogger | env.config.enableBackgroundJobs]
        , [async $ Safe.withException (OtlpServer.runServer appLogger env) exceptionLogger]
        , [async $ Safe.withException (Queue.pubsubService appLogger env env.config.otlpStreamTopics OtlpServer.processList) exceptionLogger | (not . null) env.config.otlpStreamTopics]
        ]
  void $ liftIO $ waitAnyCancel asyncs


mkServer
  :: Log.Logger
  -> AuthContext
  -> Servant.Application
mkServer logger env = do
  genericServeTWithContext
    (effToServantHandler env logger)
    (Routes.server env.pool)
    (Routes.genAuthServerContext logger env)


shutdownAPItoolkit :: AuthContext -> Eff '[IOE] ()
shutdownAPItoolkit env =
  liftIO $ do
    Pool.destroyAllResources env.pool
    Pool.destroyAllResources env.jobsPool


logException
  :: Text
  -> Log.Logger
  -> Safe.SomeException
  -> IO ()
logException envTxt logger exception =
  runEff
    . runTime
    . Logging.runLog envTxt logger
    $ Log.logAttention "odd-jobs runner crashed " (show @Text exception)
