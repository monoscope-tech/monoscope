{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Start
  ( startApp,
  )
where

import Colog.Core (LogAction (..), logStringStdout, (<&))
import qualified Config
import Configuration.Dotenv as Dotenv
import Control.Concurrent.Async
import Control.Exception (catch, try)
import qualified Control.Lens as L
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
import Data.Pool as Pool
import Database.PostgreSQL.Entity.DBT (withPool)
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
import qualified Models.Users.Users as Users
import qualified Network.Google as Google
import qualified Network.Google.Env as Env
import qualified Network.Google.PubSub as PubSub
import qualified Network.Google.Resource.PubSub.Projects.Subscriptions.Pull as PubSubPull
import Network.Wai.Handler.Warp (run)
import Optics.Operators
import ProcessMessage
import Relude
import qualified Relude.Unsafe as Unsafe
import qualified Server
import System.Envy (FromEnv, Option (Option), decodeEnv, gFromEnvCustom, runEnv)

startApp :: IO ()
startApp = do
  let logger = logStringStdout
  loadFileErr <- try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException [(String, String)])
  logger <& "Load .env Resp " <> show loadFileErr

  env <- decodeEnv :: IO (Either String Config.EnvConfig)
  logger <& "..." <> show env
  case env of
    Left err -> logger <& "Error: " <> show err
    Right envConfig -> do
      let createPgConnIO = connectPostgreSQL $ encodeUtf8 (envConfig ^. #databaseUrl)
      conn <- createPgConnIO
      initializationRes <- Migrations.runMigration conn Migrations.defaultOptions MigrationInitialization
      logger <& "migration initialized " <> show initializationRes
      migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ MigrationDirectory ((toString $ envConfig ^. #migrationsDir) :: FilePath)
      logger <& "migration result: " <> show migrationRes

      poolConn <-
        Pool.createPool
          createPgConnIO
          close
          2 -- stripes
          600 -- unused connections are kept open for 10 minutes (60*10)
          50 -- max 50 connections open per stripe

      -- If a test email is set, attempt to add that test email user to all projects in the database
      -- We watch and catch thrown exceptions which would likely happen if the user does not yet exist in the db
      case envConfig ^. #testEmail of
        Just email -> do
          err <- try (withPool poolConn (Users.addUserToAllProjects email)) :: IO (Either SomeException Int64)
          case err of
            Left err -> logger <& "unable to run addUserToAllProjects " <> show err
            Right resp -> logger <& "addUserToAllProjects resp" <> show resp
        Nothing -> pass

      let serverCtx =
            Config.AuthContext
              { env = envConfig,
                pool = poolConn
              }

      logger <& "\n"
      logger <& "Starting server at port " <> show (envConfig ^. #port)
      logger <& "\n"
      concurrently_
        (pubsubService logger envConfig poolConn)
        (run (Config.port envConfig) $ Server.app poolConn serverCtx)

-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: LogAction IO String -> Config.EnvConfig -> Pool.Pool Connection -> IO ()
pubsubService logger envConfig conn = do
  lgr <- Google.newLogger Google.Trace stdout
  env <-
    Google.newEnv
      <&> (Google.envLogger L..~ lgr)
        . (Google.envScopes L..~ PubSub.pubSubScope)
  let pullReq = PubSub.pullRequest & PubSub.prMaxMessages L.?~ fromIntegral (envConfig ^. #messagesPerPubsubPullBatch)
  let subscription = "projects/past-3/subscriptions/apitoolkit-go-client-sub"

  forever $
    runResourceT . Google.runGoogle env $ do
      pullResp <- Google.send $ PubSub.projectsSubscriptionsPull pullReq subscription
      let messages = pullResp L.^. PubSub.prReceivedMessages
      msgIds <- liftIO $ mapM (processMessage logger envConfig conn) messages
      let acknowlegReq = PubSub.acknowledgeRequest & PubSub.arAckIds L..~ catMaybes msgIds
      if null msgIds
        then pass
        else (PubSub.projectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send) >> pass
