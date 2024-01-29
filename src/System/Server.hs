{-# LANGUAGE RankNTypes #-}

module System.Server (runAPItoolkit) where

import Colourista.IO (blueMessage)
import Control.Exception.Safe qualified as Safe
import Control.Monad.Except qualified as T
import Data.Aeson qualified as Aeson
import Data.Pool as Pool
import Data.Text.Display
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Static
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Fail (runFailIO)
import Effectful.PostgreSQL.Transact.Effect (runDB)
import Effectful.Reader.Static qualified
import Effectful.Time (runTime)
import Log qualified
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setOnException,
  setPort,
 )
import Network.Wai.Log qualified as WaiLog
import Network.Wai.Middleware.Heartbeat (heartbeatMiddleware)
import Relude
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
  liftIO
    $ runSettings warpSettings
    $ heartbeatMiddleware
    . loggingMiddleware
    . const
    $ server


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
