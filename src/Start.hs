{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Start
  ( startApp,
  )
where

import Colog.Core (LogAction (..), logStringStdout, (<&))
import Config qualified
import Configuration.Dotenv as Dotenv
import Control.Concurrent.Async
import Control.Exception (try)
import Control.Lens qualified as L
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool as Pool
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration as Migrations
import GHC.Generics ()
import Models.Users.Users qualified as Users
import Network.Google qualified as Google
import Network.Google.PubSub qualified as PubSub
import Network.Wai.Handler.Warp (run)
import Optics.Operators
import ProcessMessage
import Relude
import Server qualified
import System.Envy (decodeEnv)

startApp :: IO ()
startApp = do
  let logger = logStringStdout
  loadFileErr <- try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException [(String, String)])
  case loadFileErr of
    Left err -> logger <& "Load .env error " <> show loadFileErr
    Right _ -> pass

  env <- decodeEnv :: IO (Either String Config.EnvConfig)
  case env of
    Left err -> logger <& "Error decoding env variables : " <> show err
    Right envConfig -> do
      let createPgConnIO = connectPostgreSQL $ encodeUtf8 (envConfig ^. #databaseUrl)
      conn <- createPgConnIO
      when (envConfig ^. #migrateAndInitializeOnStart) do
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
          when (envConfig ^. #migrateAndInitializeOnStart) do
            err <- try (withPool poolConn (Users.addUserToAllProjects email)) :: IO (Either SomeException Int64)
            case err of
              Left err' -> logger <& "unable to run addUserToAllProjects " <> show err'
              Right resp -> logger <& "addUserToAllProjects resp" <> show resp
        Nothing -> pass

      let serverCtx =
            Config.AuthContext
              { env = envConfig,
                pool = poolConn,
                logger = logger
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
  env <-
    Google.newEnv
      <&> (Google.envScopes L..~ PubSub.pubSubScope)
  let pullReq = PubSub.pullRequest & PubSub.prMaxMessages L.?~ fromIntegral (envConfig ^. #messagesPerPubsubPullBatch)
  let subscription = "projects/past-3/subscriptions/apitoolkit-go-client-sub"

  forever $
    runResourceT . Google.runGoogle env $ do
      pullResp <- Google.send $ PubSub.projectsSubscriptionsPull pullReq subscription
      let messages = pullResp L.^. PubSub.prReceivedMessages
      msgIds <- liftIO $ mapM (processMessage logger envConfig conn) messages
      let acknowlegReq = PubSub.acknowledgeRequest & PubSub.arAckIds L..~ catMaybes msgIds
      unless (null msgIds) do
        _ <- PubSub.projectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send
        pass
