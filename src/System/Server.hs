{-# LANGUAGE RankNTypes #-}

module System.Server (runAPItoolkit) where

import BackgroundJobs qualified
import Colourista.IO (blueMessage)
import Control.Concurrent.Async
import Control.Exception (try)
import Control.Exception.Safe qualified as Safe
import Control.Lens qualified as L
import Control.Monad.Except qualified as T
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Base64 qualified as LB64
import Data.Generics.Product (field)
import Data.Pool as Pool
import Data.Text.Display
import Data.Text.Lazy.Encoding qualified as LT
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Static
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Fail (runFailIO)
import Effectful.PostgreSQL.Transact.Effect (runDB)
import Effectful.Reader.Static (ask, asks)
import Effectful.Reader.Static qualified
import Effectful.Time (runTime)
import Gogol qualified as Google
import Gogol.Auth.ApplicationDefault qualified as Google
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
import ProcessMessage
import Relude hiding (ask, asks)
import Servant qualified
import Servant.Server (Handler, ServerError)
import Servant.Server.Generic (genericServeTWithContext)
import System.Config
import System.Logging qualified as Logging
import System.Types
import Web.Routes qualified as Routes


runAPItoolkit :: IO ()
runAPItoolkit =
  Safe.bracket
    (getAppContext & runFailIO & runEff)
    (runEff . shutdownTalstack)
    \env -> runEff . runTime . runConcurrent $ do
      let baseURL = "http://localhost:" <> show env.config.port
      liftIO $ blueMessage $ "Starting APItoolkit server on " <> baseURL
      let withLogger = Logging.makeLogger env.config.loggingDestination
      withLogger (`runServer` env)


runServer :: (Concurrent :> es, IOE :> es) => Log.Logger -> AuthContext -> Eff es ()
runServer appLogger env = do
  loggingMiddleware <- Logging.runLog (show env.config.environment) appLogger WaiLog.mkLogMiddleware
  let server = mkServer appLogger env
  let warpSettings =
        defaultSettings
          & setPort (fromIntegral env.config.port)
          & setOnException \mRequest exception -> Log.runLogT "apitoolkit" appLogger Log.LogAttention $ do
            Log.logAttention "Unhandled exception" $ Aeson.object ["exception" Aeson..= show @String exception]
            Safe.throw exception

  let wrappedServer =
        heartbeatMiddleware
          . loggingMiddleware
          . const
          $ server
  let bgJobWorker = BackgroundJobs.jobsWorkerInit env.jobsPool appLogger env.config

  -- let ojStartArgs =
  --       OJCli.UIStartArgs
  --         { uistartAuth = OJCli.AuthNone
  --         , uistartPort = 8081
  --         }

  -- let ojLogger logLevel logEvent = logger <& show (logLevel, logEvent)
  -- let ojTable = "background_jobs" :: OJTypes.TableName
  -- let ojCfg = OJConfig.mkUIConfig ojLogger ojTable poolConn id
  asyncs <-
    liftIO
      $ sequence
        [ async $ runSettings warpSettings wrappedServer
        , async $ Safe.withException bgJobWorker (logException (env.config.environment) appLogger)
        , async $ runBackground appLogger env $ pubsubService
        -- , async $ OJCli.defaultWebUI ojStartArgs ojCfg
        ]
  _ <- liftIO $ waitAnyCancel asyncs
  pass


-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: ATBackgroundCtx ()
pubsubService = do
  appCtx <- ask @AuthContext
  let envConfig = appCtx.config
  env <- liftIO $ case envConfig.googleServiceAccountB64 of
    "" -> Google.newEnv <&> (Google.envScopes L..~ pubSubScope)
    sa -> do
      let credJSON = either error id $ LB64.decodeBase64 (LT.encodeUtf8 sa)
      let credsE = Google.fromJSONCredentials credJSON
      let creds = either (error . toText) id credsE
      -- let Right creds = credsE
      managerG <- Google.newManager Google.tlsManagerSettings
      Google.newEnvWith creds (\_ _ -> pass) managerG <&> (Google.envScopes L..~ pubSubScope)

  let pullReq = PubSub.newPullRequest & field @"maxMessages" L.?~ fromIntegral (envConfig.messagesPerPubsubPullBatch)

  forever
    $ runResourceT
      do
        forM envConfig.requestPubsubTopics \topic -> do
          let subscription = "projects/past-3/subscriptions/" <> topic <> "-sub"
          pullResp <- Google.send env $ PubSub.newPubSubProjectsSubscriptionsPull pullReq subscription
          let messages = (pullResp L.^. field @"receivedMessages") & fromMaybe []
          msgIds <- liftIO $ processMessages appCtx.logger envConfig appCtx.jobsPool messages appCtx.projectCache
          let acknowlegReq = PubSub.newAcknowledgeRequest & field @"ackIds" L..~ Just (catMaybes msgIds)
          unless (null msgIds) $ void $ PubSub.newPubSubProjectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send env


-- pubSubScope :: Proxy PubSub.Pubsub'FullControl
pubSubScope :: Proxy '["https://www.googleapis.com/auth/pubsub"]
pubSubScope = Proxy


mkServer
  :: Log.Logger
  -> AuthContext
  -> Servant.Application
mkServer logger env = do
  genericServeTWithContext
    (naturalTransform env logger)
    (Routes.server env.pool)
    (Routes.genAuthServerContext logger env)


naturalTransform :: AuthContext -> Log.Logger -> ATBaseCtx a -> Handler a
naturalTransform env logger app =
  app
    & Effectful.Reader.Static.runReader env
    & runDB env.pool
    & runTime
    & Logging.runLog (show env.config.environment) logger
    & effToHandler


handlerToEff
  :: forall (es :: [Effect]) (a :: Type)
   . Error ServerError :> es
  => Handler a
  -> Eff es a
handlerToEff handler = do
  v <- unsafeEff_ $ Servant.runHandler handler
  either throwError pure v


effToHandler
  :: forall (a :: Type)
   . ()
  => Eff '[Error ServerError, IOE] a
  -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
  either T.throwError pure v


shutdownTalstack :: AuthContext -> Eff '[IOE] ()
shutdownTalstack env =
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
