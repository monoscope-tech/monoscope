{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Start
  ( startApp,
  )
where

import Colog.Core (LogAction (..), logStringStdout, (<&))
import qualified Config
import Configuration.Dotenv as Dotenv
import Control.Concurrent.Async
import Control.Lens ((&), (.~), (<&>), (?~), (^.), (^?), _Just)
import Control.Monad (when)
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Data.Aeson
  ( FromJSON,
    Object,
    ToJSON,
    eitherDecode,
  )
import Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Base64 as B64 (decodeBase64)
import Data.Maybe (catMaybes)
import Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection, Only (Only), close, connectPostgreSQL, query_)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory), runMigration)
import Database.PostgreSQL.Simple.Migration as Migrations
import Deriving.Aeson
  ( CamelToSnake,
    CustomJSON (CustomJSON),
    FieldLabelModifier,
    Generic,
    OmitNothingFields,
    StripPrefix,
  )
import GHC.Generics ()
import qualified Network.Google as Google
import qualified Network.Google.Env as Env
import qualified Network.Google.PubSub as PubSub
import qualified Network.Google.Resource.PubSub.Projects.Subscriptions.Pull as PubSubPull
import Network.Wai.Handler.Warp (run)
import ProcessMessage
import Relude
import qualified Relude.Unsafe as Unsafe
import qualified Server
import System.Envy (FromEnv, Option (Option), decodeEnv, gFromEnvCustom, runEnv)
import System.IO (stdout)

startApp :: IO ()
startApp = do
  let logger = logStringStdout
  Dotenv.loadFile Dotenv.defaultConfig
  env <- decodeEnv :: IO (Either String Config.EnvConfig)
  logger <& "..." <> show env
  case env of
    Left err -> logger <& "Error: " <> show err
    Right envConfig -> do
      logger <& "Starting server at port " <> show (Config.port envConfig)
      conn <- connectPostgreSQL $ encodeUtf8 (Config.databaseUrl envConfig)
      initializationRes <- Migrations.runMigration conn Migrations.defaultOptions MigrationInitialization
      logger <& "migration initialized " <> show initializationRes
      migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ MigrationDirectory (Config.migrationsDir envConfig)
      logger <& "migration result: " <> show migrationRes
      poolConn <- Pool.createPool (pure conn) close 1 100000000 50
      let serverCtx = Config.AuthContext{ env=envConfig,
                pool = poolConn
                }

      concurrently_
        (pubsubService logger envConfig poolConn)
        (run (Config.port envConfig) $ Server.app serverCtx)

-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: LogAction IO String -> Config.EnvConfig -> Pool.Pool Connection -> IO ()
pubsubService logger envConfig conn = do
  lgr <- Google.newLogger Google.Trace stdout
  env <-
    Google.newEnv
      <&> (Google.envLogger .~ lgr)
        . (Google.envScopes .~ PubSub.pubSubScope)
  let pullReq = PubSub.pullRequest & PubSub.prMaxMessages ?~ 1
  let subscription = "projects/past-3/subscriptions/apitoolkit-go-client-sub"

  forever $
    runResourceT . Google.runGoogle env $ do
      pullResp <- Google.send $ PubSub.projectsSubscriptionsPull pullReq subscription
      let messages = pullResp ^. PubSub.prReceivedMessages
      msgIds <- liftIO $ mapM (processMessage logger envConfig conn) messages
      let acknowlegReq = PubSub.acknowledgeRequest & PubSub.arAckIds .~ catMaybes msgIds
      if null msgIds
        then pure ()
        else (PubSub.projectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send) >> pure ()
