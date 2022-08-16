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
import Data.Generics.Product (field)
import Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Migration as Migrations
import GHC.Generics ()
import GHC.IO.Encoding hiding (close)
import Gogol qualified as Google
import Gogol.PubSub qualified as PubSub
import Models.Projects.Projects qualified as Projects
import Network.Wai.Handler.Warp (run)
import Optics.Operators
import ProcessMessage
import Relude
import Server qualified
import System.Clock
import System.Envy (decodeEnv)

startApp :: IO ()
startApp = do
  setLocaleEncoding utf8
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
            1 -- stripes
            30 -- unused connections are kept open for 5 minutes (60*5) ~ Not sure if this is seconds or minutes
            3 -- max 50 connections open per stripe
            -- poolConn <- Pool.newPool Pool.PoolConfig {createResource = createPgConnIO, freeResource = close, poolCacheTTL = 60 * 10, poolMaxResources = 20}
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
      <&> (Google.envScopes L..~ pubSubScope)
  let pullReq = PubSub.newPullRequest & field @"maxMessages" L.?~ fromIntegral (envConfig ^. #messagesPerPubsubPullBatch)

  forever $
    runResourceT $ do
      forM (envConfig ^. #requestPubsubTopics) \topic -> do
        let subscription = "projects/past-3/subscriptions/" <> topic <> "-sub"
        pullResp <- Google.send env $ PubSub.newPubSubProjectsSubscriptionsPull pullReq subscription
        let messages = (pullResp L.^. field @"receivedMessages") & fromMaybe []
        msgIds <- liftIO $ processMessages logger envConfig conn messages projectCache
        let acknowlegReq = PubSub.newAcknowledgeRequest & field @"ackIds" L..~ Just (catMaybes msgIds)
        unless (null msgIds) $ void $ PubSub.newPubSubProjectsSubscriptionsAcknowledge acknowlegReq subscription & Google.send env

-- pubSubScope :: Proxy PubSub.Pubsub'FullControl
pubSubScope :: Proxy '["https://www.googleapis.com/auth/pubsub"]
pubSubScope = Proxy
