{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Start
  ( startApp,
  )
where

import BackgroundJobs qualified
import Colog.Core (LogAction (..), logStringStdout, (<&))
import Config qualified
import Configuration.Dotenv as Dotenv
import Control.Concurrent.Async
import Control.Exception (try)
import Control.Lens qualified as L
import Control.Monad.Trans.Resource (runResourceT)
import Data.Cache
import Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration as Migrations
import GHC.Generics ()
import Models.Projects.Projects qualified as Projects
import Network.Google qualified as Google
import Network.Google.PubSub qualified as PubSub
import Network.Wai.Handler.Warp (run)
import Optics.Operators
import ProcessMessage
import Relude
import Server qualified
import System.Clock
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
    Right envConfig ->
      do
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
            60 -- unused connections are kept open for 10 minutes (60*10)
            20 -- max 50 connections open per stripe
        projectCache <- newCache (Just $ TimeSpec (60 * 60) 0) :: IO (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
        let serverCtx =
              Config.AuthContext
                { env = envConfig,
                  pool = poolConn,
                  logger = logger,
                  projectCache = projectCache
                }

        logger <& "\n"
        logger <& "🚀 Starting server at port " <> show (envConfig ^. #port)
        logger <& "\n"

        asyncs <-
          sequence
            [ async (pubsubService logger envConfig poolConn projectCache),
              async (pubsubService logger envConfig poolConn projectCache),
              async (run (Config.port envConfig) $ Server.app logger poolConn serverCtx),
              async $ BackgroundJobs.jobsWorkerInit poolConn logger envConfig
            ]
        _ <- waitAnyCancel asyncs
        pass

-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: LogAction IO String -> Config.EnvConfig -> Pool.Pool Connection -> Cache Projects.ProjectId Projects.ProjectCache -> IO ()
pubsubService logger envConfig conn projectCache = do
  env <-
    Google.newEnv
      <&> (Google.envScopes L..~ PubSub.pubSubScope)
  let pullReq = PubSub.pullRequest & PubSub.prMaxMessages L.?~ fromIntegral (envConfig ^. #messagesPerPubsubPullBatch)

  forever $
    runResourceT . Google.runGoogle env $ do
      forM (envConfig ^. #requestPubsubTopics) \topic -> do
        let subscription = "projects/past-3/subscriptions/" <> topic <> "-sub"
        pullResp <- Google.send $ PubSub.projectsSubscriptionsPull pullReq subscription
        let messages = pullResp L.^. PubSub.prReceivedMessages
        msgIds <- liftIO $ processMessages logger envConfig conn messages projectCache
        let acknowlegReq = PubSub.acknowledgeRequest & PubSub.arAckIds L..~ catMaybes msgIds
        unless (null msgIds) $ void $ PubSub.projectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send
