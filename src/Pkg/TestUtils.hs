module Pkg.TestUtils (
  withSetup,
  withTestResources,
  fromRightShow,
  TestResources (..),
  testSessionHeader,
  refreshSession,
  runTestEffect,
  testRequestMsgs,
  TestRequestMessages (..),
  convert,
  runTestBackground,
  runTestBackgroundWithNotifications,
  runTestBg,
  testServant,
  testServantWithNotifications,
  runAllBackgroundJobs,
  getPendingBackgroundJobs,
  logBackgroundJobsInfo,
  runBackgroundJobsWhere,
  setBjRunAtInThePast,
  toServantResponse,
  toBaseServantResponse,
  atAuthToBase,
  effToServantHandlerTest,
  runQueryEffect,
  runHasqlEffect,
  frozenTime,
  -- Helper functions for tests
  processMessagesAndBackgroundJobs,
  createTestSpans,
  -- OTLP/Telemetry helpers
  createTestAPIKey,
  ingestLog,
  ingestTrace,
  ingestMetric,
  ingestLogWithHeader,
  ingestTraceWithHeader,
  ingestMetricWithHeader,
  ingestTraceWithException,
  -- CLI test helpers
  runHTTPtoServant,
  testPid,
  -- OTLP mock data constructors
  createOtelLogAtTime,
  createOtelSpanAtTime,
  createOtelTraceAtTime,
  createOtelTraceWithExceptionAtTime,
  createGaugeMetricAtTime,
  mkSpanRequest,
  mkResource,
  mkAttr,
)
where

import BackgroundJobs qualified
import Configuration.Dotenv qualified as Dotenv
import Control.Concurrent (threadDelay)
import Control.Exception (finally, throwIO, try)
import Control.Exception.Safe qualified as Safe
import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (KeyValue (..))
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as LBS
import Data.Cache (Cache (..), newCache)
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.Notify qualified
import Data.Effectful.UUID (UUIDEff, runStaticUUID, runUUID)
import Data.Effectful.Wreq (HTTP (..), runHTTPGolden, runHTTPWreq)
import Data.Either.Extra
import Data.HashMap.Strict qualified as HM
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, Only (..), close, connect, connectPostgreSQL, defaultConnectInfo, execute, execute_)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization))
import Database.PostgreSQL.Simple.Migration qualified as Migration
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.Postgres.Temp (cacheAction, cacheConfig, toConnectionString, withConfig, withDbCache)
import Database.Postgres.Temp qualified as TmpPostgres
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (runLabeled)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Effectful.Time (Time, runFrozenTime, runTime)
import Log qualified
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common.Protobuf (Proto (..))
import Network.HTTP.Client (createCookieJar, defaultRequest)
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..))
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Types.Version (http11)
import Network.Wreq qualified as W
import OddJobs.Job (Job (..))
import OpenTelemetry.Trace (TracerProvider, getGlobalTracerProvider)
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pages.Settings qualified as Api
import Data.Effectful.Hasql (Hasql, runHasqlPool)
import Hasql.Pool qualified as HPool
import Pkg.DeriveUtils (AesonText (..), DB, UUIDId (..), mkHasqlPool, runConnectionPool)
import ProcessMessage qualified
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as LS
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as LSF
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as TS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as TSF
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as PCF
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as PLF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as PR
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as PRF
import Proto.Opentelemetry.Proto.Trace.V1.Trace (Span'Event, Status)
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as PTF
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import Servant.Server qualified as ServantS
import System.Clock (TimeSpec (TimeSpec))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Config qualified as Config
import System.Directory (getFileSize, listDirectory)
import System.Envy (DefConfig (..), decodeWithDefaults)
import System.Logging qualified as Logging
import System.Tracing (Tracing)
import System.Tracing qualified as Tracing
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, RespHeaders, atAuthToBase, atAuthToBaseTest, effToServantHandlerTest)
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)


migrationsDirr :: FilePath
migrationsDirr = "./static/migrations/"


-- | Test Discord public key (hex-encoded Ed25519 public key derived from deterministic seed)
-- Matches the public key generated from 32 bytes of 0x42 in BotTestHelpers
testDiscordPublicKeyHex :: Text
testDiscordPublicKeyHex = "2152f8d19b791d24453242e15f2eab6cb7cffa7b6a5ed30097960e069881db12"


migrate :: TmpPostgres.DB -> IO ()
migrate db = do
  conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
  initializationRes <- Migration.runMigration conn Migration.defaultOptions MigrationInitialization

  migrationRes <- Migration.runMigration conn Migration.defaultOptions $ MigrationDirectory migrationsDirr
  -- Set nil user as sudo for tests and create test project
  let q =
        [sql| UPDATE users.users SET is_sudo = true WHERE id = '00000000-0000-0000-0000-000000000000';

              INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
              VALUES ('00000000-0000-0000-0000-000000000000', 'Demo Project', 'Startup', true, NULL, true, true)
              ON CONFLICT (id) DO UPDATE SET payment_plan = 'Startup', active = true, deleted_at = NULL;
              
              INSERT into projects.project_api_keys (active, project_id, title, key_prefix) 
              SELECT True, '00000000-0000-0000-0000-000000000000', 'test', 'z6YeJcRJNH0zy9JOg6ZsQzxM9GHBHdSeu+7ugOpZ9jtR94qV'
              WHERE NOT EXISTS (
                SELECT 1 FROM projects.project_api_keys 
                WHERE key_prefix = 'z6YeJcRJNH0zy9JOg6ZsQzxM9GHBHdSeu+7ugOpZ9jtR94qV'
              );
        |]
  _ <- execute conn q ()
  pass


-- Setup function that spins up a database with the db migrations already executed.
-- source: https://jfischoff.github.io/blog/keeping-database-tests-fast.html
withSetup :: (Pool Connection -> ByteString -> IO ()) -> IO ()
withSetup f = do
  useExternalDB <- lookupEnv "USE_EXTERNAL_DB"
  case useExternalDB of
    Just "true" -> withExternalDBSetup f
    _ -> withLocalSetup f


-- Local setup using tmp-postgres. The callback receives both the postgresql-simple
-- pool and the raw libpq connection string so a parallel hasql pool can be built.
withLocalSetup :: (Pool Connection -> ByteString -> IO ()) -> IO ()
withLocalSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x
  throwE $ withDbCache $ \dbCache -> do
    let combinedConfig =
          cacheConfig dbCache
            <> TmpPostgres.verboseConfig
            <> mempty
              { TmpPostgres.postgresConfigFile =
                  [ ("shared_preload_libraries", "'timescaledb'")
                  , ("max_connections", "200")
                  ]
              }
    dirSize <- sum <$> (listDirectory migrationsDirr >>= mapM (getFileSize . (migrationsDirr <>)))
    migratedConfig <- throwE $ cacheAction ("./.tmp/postgres/" <> show dirSize) migrate combinedConfig
    withConfig migratedConfig $ \db -> do
      let cstr = toConnectionString db
      pool <- newPool (defaultPoolConfig (connectPostgreSQL cstr) close 60 50)
      f pool cstr


-- Test DB connection info, host configurable via DB_HOST env var
testConnInfo :: String -> ConnectInfo
testConnInfo dbName = defaultConnectInfo{connectUser = "postgres", connectPassword = "postgres", connectDatabase = dbName}


-- External database setup using template database approach for better isolation and performance
withExternalDBSetup :: (Pool Connection -> ByteString -> IO ()) -> IO ()
withExternalDBSetup f = do
  dbHost <- fromMaybe "localhost" <$> lookupEnv "DB_HOST"
  let connInfo dbName = (testConnInfo dbName){connectHost = dbHost}
      templateDbName = "monoscope_test_template"

  -- Create or update template database
  ensureTemplateDatabase connInfo templateDbName

  -- Generate unique test database name using UUID (replace hyphens with underscores)
  uuid <- nextRandom
  let testDbName = "monoscope_test_" <> T.replace "-" "_" (show uuid)

  -- Create test database from template (retry on contention)
  masterConn <- connect $ connInfo "postgres"
  let createFromTemplate attempt = do
        result <- try @SomeException $ execute masterConn (Query $ encodeUtf8 $ "CREATE DATABASE " <> testDbName <> " TEMPLATE " <> templateDbName) ()
        case result of
          Right _ -> pass
          Left e
            | attempt < (3 :: Int)
            , "being accessed by other users" `T.isInfixOf` show e -> do
                -- Terminate connections to template and retry
                void (PGS.query_ masterConn (Query $ encodeUtf8 $ "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '" <> templateDbName <> "' AND pid <> pg_backend_pid()") :: IO [Only Bool])
                threadDelay (100000 * (attempt + 1))
                createFromTemplate (attempt + 1)
            | otherwise -> throwIO e
  createFromTemplate 0
  close masterConn

  -- Connect to the new test database
  pool <- newPool (defaultPoolConfig (connect $ connInfo (toString testDbName)) close 60 10)
  let cstr =
        encodeUtf8
          ( "host="
              <> toText dbHost
              <> " user=postgres password=postgres dbname="
              <> testDbName
          )

  -- Run tests and cleanup
  finally (f pool cstr) $ do
    -- Close all connections in the pool
    destroyAllResources pool

    -- Give connections time to fully close
    threadDelay 100000 -- 100ms

    -- Forcefully terminate any remaining connections to test database
    masterConn' <- connect $ connInfo "postgres"
    (_ :: [Only Bool]) <-
      PGS.query_
        masterConn'
        (Query $ encodeUtf8 $ "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = '" <> testDbName <> "' AND pid <> pg_backend_pid()")

    -- Now drop the database
    void
      $ execute
        masterConn'
        (Query $ encodeUtf8 $ "DROP DATABASE IF EXISTS " <> testDbName)
        ()
    close masterConn'


-- Helper function to ensure template database exists and is up to date
ensureTemplateDatabase :: (String -> ConnectInfo) -> Text -> IO ()
ensureTemplateDatabase connInfo templateDbName = do
  masterConn <- connect $ connInfo "postgres"

  -- Check if template database exists
  [Only exists] <-
    PGS.query
      masterConn
      "SELECT EXISTS(SELECT 1 FROM pg_database WHERE datname = ?)"
      (Only templateDbName)

  -- Get current migration directory checksum for cache invalidation
  -- Use per-file sizes (sorted by name) so content changes that preserve total size are detected
  files <- sort <$> listDirectory migrationsDirr
  sizes <- mapM (getFileSize . (migrationsDirr <>)) files
  let migrationChecksum = show (zip files sizes)

  -- Check if template needs updating
  needsUpdate <-
    if exists
      then do
        -- Try to connect to template and check a marker we'll set
        templateConn <- connect $ connInfo (toString templateDbName)

        -- Check if our migration checksum marker exists and matches
        markerExists <-
          PGS.query
            templateConn
            "SELECT EXISTS(SELECT 1 FROM pg_tables WHERE schemaname = 'public' AND tablename = 'template_db_info')"
            ()
            :: IO [Only Bool]

        case markerExists of
          [Only True] -> do
            checksums <-
              PGS.query
                templateConn
                "SELECT migration_checksum FROM template_db_info LIMIT 1"
                ()
                :: IO [Only Text]
            close templateConn
            pure $ case checksums of
              [Only checksum] -> checksum /= toText migrationChecksum
              _ -> True
          _ -> do
            close templateConn
            pure True
      else pure True

  when needsUpdate $ do
    -- Drop existing template if it exists
    when exists $ do
      -- First, terminate any connections to the template database
      void
        $ execute
          masterConn
          ( Query
              $ encodeUtf8
              $ "DO $$ BEGIN  PERFORM pg_terminate_backend(pid) FROM pg_stat_activity  WHERE datname = '"
              <> templateDbName
              <> "' AND pid <> pg_backend_pid(); END $$;"
          )
          ()

      -- Mark database as template
      _ <-
        execute
          masterConn
          (Query $ encodeUtf8 $ "ALTER DATABASE " <> templateDbName <> " is_template = false")
          ()
      _ <-
        execute
          masterConn
          (Query $ encodeUtf8 $ "DROP DATABASE IF EXISTS " <> templateDbName)
          ()
      pass

    -- Create fresh template database
    _ <-
      execute
        masterConn
        (Query $ encodeUtf8 $ "CREATE DATABASE " <> templateDbName)
        ()

    -- Connect to template database and set up schema
    templateConn <- connect $ connInfo (toString templateDbName)

    -- Run migrations
    _ <- Migration.runMigration templateConn Migration.defaultOptions MigrationInitialization
    _ <- Migration.runMigration templateConn Migration.defaultOptions $ MigrationDirectory migrationsDirr

    -- Set nil user as sudo for tests and create test project
    let setupData =
          [sql| UPDATE users.users SET is_sudo = true WHERE id = '00000000-0000-0000-0000-000000000000';

                INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
                VALUES ('00000000-0000-0000-0000-000000000000', 'Demo Project', 'FREE', true, NULL, true, true)
                ON CONFLICT (id) DO UPDATE SET payment_plan = 'FREE', active = true, deleted_at = NULL;
                
                INSERT into projects.project_api_keys (active, project_id, title, key_prefix) 
                SELECT True, '00000000-0000-0000-0000-000000000000', 'test', 'z6YeJcRJNH0zy9JOg6ZsQzxM9GHBHdSeu+7ugOpZ9jtR94qV'
                WHERE NOT EXISTS (
                  SELECT 1 FROM projects.project_api_keys 
                  WHERE key_prefix = 'z6YeJcRJNH0zy9JOg6ZsQzxM9GHBHdSeu+7ugOpZ9jtR94qV'
                );
          |]
    _ <- execute templateConn setupData ()

    -- Create marker table with migration checksum
    _ <-
      execute_
        templateConn
        "CREATE TABLE IF NOT EXISTS template_db_info (migration_checksum TEXT)"
    _ <-
      execute
        templateConn
        "INSERT INTO template_db_info (migration_checksum) VALUES (?)"
        (Only $ toText migrationChecksum)

    close templateConn

    -- Mark database as template
    _ <-
      execute
        masterConn
        (Query $ encodeUtf8 $ "ALTER DATABASE " <> templateDbName <> " is_template = true")
        ()
    pass

  close masterConn


-- | `testSessionHeader` would log a user in and automatically generate a session header
-- which can be reused in subsequent tests
testSessionHeader :: MonadIO m => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Projects.Session)
testSessionHeader pool = do
  pSessId <-
    Auth.authorizeUserAndPersist Nothing "firstName" "lastName" "https://placehold.it/500x500" "test@monoscope.tech"
      & runStaticUUID (map (UUID.fromWords 0 0 0) [1 .. 100])
      & runHTTPGolden "./tests/golden/"
      & runConnectionPool pool
      & runTime
      & runEff
      & liftIO

  -- Insert test user into users table
  _ <- liftIO
    $ withResource pool \conn ->
      PGS.execute
        conn
        [sql|UPDATE users.users SET is_sudo = true WHERE id = '00000000-0000-0000-0000-000000000001'|]
        ()

  -- Grant sudo privileges to test user
  _ <- liftIO
    $ withResource pool \conn ->
      PGS.execute
        conn
        [sql|UPDATE users.users SET is_sudo = true WHERE id = '00000000-0000-0000-0000-000000000001'|]
        ()

  -- Create a test project and add it to the user's session
  let testProjectId = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
  _ <- liftIO
    $ withResource pool \conn ->
      PGS.execute
        conn
        [sql|INSERT INTO projects.projects (id, title, description, payment_plan, active, deleted_at, weekly_notif, daily_notif)
         VALUES (?, 'Test Project', 'Test Description', 'Startup', true, NULL, true, true)
         ON CONFLICT (id) DO UPDATE SET title = 'Test Project', description = 'Test Description', payment_plan = 'Startup', active = true, deleted_at = NULL, weekly_notif = true, daily_notif = true|]
        (Only testProjectId)

  -- Add project member permissions (test project and nil UUID project used by many tests)
  _ <- liftIO
    $ withResource pool \conn ->
      PGS.execute
        conn
        [sql|INSERT INTO projects.project_members (project_id, user_id, permission)
         VALUES (?, '00000000-0000-0000-0000-000000000001', 'admin'), ('00000000-0000-0000-0000-000000000000', '00000000-0000-0000-0000-000000000001', 'admin')
         ON CONFLICT (project_id, user_id) DO UPDATE SET permission = 'admin', active = TRUE|]
        (Only testProjectId)

  tp <- liftIO getGlobalTracerProvider
  logger <- liftIO $ Log.mkLogger "test" (const pass)
  runTestEffect pool logger tp (Auth.sessionByID (Just pSessId) "requestID" True "light" Nothing False)
    & liftIO
    <&> fromRightShow


-- | Refresh a session to pick up any new projects added to the user
refreshSession :: MonadIO m => Pool Connection -> Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Projects.Session -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Projects.Session)
refreshSession pool sessionHeaders = do
  let session = Servant.getResponse sessionHeaders
      pSessId = session.sessionId
  tp <- liftIO getGlobalTracerProvider
  logger <- liftIO $ Log.mkLogger "test" (const pass)
  runTestEffect pool logger tp (Auth.sessionByID (Just pSessId) "requestID" session.isSidebarClosed session.theme Nothing False)
    & liftIO
    <&> fromRightShow


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


-- | Default frozen time used in tests
frozenTime :: UTCTime
frozenTime = Unsafe.read "2025-01-01 00:00:00 UTC"


runTestBackground :: UTCTime -> Config.AuthContext -> ATBackgroundCtx a -> IO a
runTestBackground t authCtx action = LogBulk.withBulkStdOutLogger \logger ->
  runTestBackgroundWithLogger t logger authCtx action


-- | Run background context and return both notifications and result
runTestBackgroundWithNotifications :: UTCTime -> Log.Logger -> Config.AuthContext -> ATBackgroundCtx a -> IO ([Data.Effectful.Notify.Notification], a)
runTestBackgroundWithNotifications t logger appCtx process = do
  tp <- getGlobalTracerProvider
  notifRef <- newIORef []
  result <-
    process
      & Data.Effectful.Notify.runNotifyTest notifRef
      & Effectful.Reader.Static.runReader appCtx
      & runConnectionPool appCtx.pool
      & runLabeled @"timefusion" (runConnectionPool appCtx.timefusionPgPool)
      & runHasqlPool appCtx.hasqlPool
      & runLabeled @"timefusion" (runHasqlPool appCtx.hasqlTimefusionPool)
      & runFrozenTime t
      & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger appCtx.config.logLevel
      & Tracing.runTracing tp
      & runUUID
      & runHTTPGolden "./tests/golden/"
      & ELLM.runLLMGolden "./tests/golden/"
      & runConcurrent
      & Ki.runStructuredConcurrency
      & Effectful.runEff
  notifications <- reverse <$> readIORef notifRef
  pure (notifications, result)


runTestBackgroundWithLogger :: UTCTime -> Log.Logger -> Config.AuthContext -> ATBackgroundCtx a -> IO a
runTestBackgroundWithLogger t logger appCtx process = do
  (notifications, result) <- runTestBackgroundWithNotifications t logger appCtx process
  -- Log the notifications that would have been sent
  forM_ notifications \notification -> do
    let notifInfo = case notification of
          Data.Effectful.Notify.EmailNotification emailData ->
            ("Email" :: Text, Data.Effectful.Notify.receiver emailData, Just (Data.Effectful.Notify.subject emailData))
          Data.Effectful.Notify.SlackNotification slackData ->
            ("Slack" :: Text, slackData.channelId, Nothing :: Maybe Text)
          Data.Effectful.Notify.DiscordNotification discordData ->
            ("Discord" :: Text, discordData.channelId, Nothing :: Maybe Text)
          Data.Effectful.Notify.WhatsAppNotification whatsappData ->
            ("WhatsApp" :: Text, Data.Effectful.Notify.to whatsappData, Just (Data.Effectful.Notify.template whatsappData))
          Data.Effectful.Notify.PagerdutyNotification pdData ->
            ("PagerDuty" :: Text, pdData.dedupKey, Just pdData.summary)
    Log.logInfo "Notification" notifInfo
      & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger appCtx.config.logLevel
      & Effectful.runEff
    Log.logTrace "Notification payload" notification
      & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger appCtx.config.logLevel
      & Effectful.runEff
  pure result


-- | Run an effect action in test context (for non-servant handlers like Auth.sessionByID)
runTestEffect :: Pool Connection -> Log.Logger -> TracerProvider -> (forall es. (DB es, Error ServantS.ServerError :> es, HTTP :> es, Log :> es, Time :> es, Tracing :> es, UUIDEff :> es) => Eff es a) -> IO (Either ServantS.ServerError a)
runTestEffect pool logger tp action = do
  logLevel <- Logging.getLogLevelFromEnv
  action
    & runErrorNoCallStack @ServantS.ServerError
    & runConnectionPool pool
    & runTime
    & Logging.runLog "test" logger logLevel
    & Tracing.runTracing tp
    & runUUID
    & runHTTPWreq
    & runEff


-- | Run a background job action using TestResources
runTestBg :: UTCTime -> TestResources -> ATBackgroundCtx a -> IO a
runTestBg t TestResources{..} = runTestBackgroundWithLogger t trLogger trATCtx


-- New type to hold all our resources
data TestResources = TestResources
  { trPool :: Pool Connection
  , trProjectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , trSessAndHeader :: Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Projects.Session
  , trATCtx :: AuthContext
  , trLogger :: Log.Logger
  , trTracerProvider :: TracerProvider
  }


-- Compose withSetup with additional IO actions
withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources f = withSetup $ \pool cstr -> LogBulk.withBulkStdOutLogger \logger -> do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  projectKeyCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  logsPatternCache <- newCache (Just $ TimeSpec (30 * 60) 0) -- Cache for log patterns, 30 minutes TTL
  sessAndHeader <- testSessionHeader pool
  tp <- getGlobalTracerProvider
  -- Parallel hasql pools sharing the same test DB
  hasqlMain <- mkHasqlPool 5 cstr
  hasqlJobs <- mkHasqlPool 5 cstr
  hasqlTf <- mkHasqlPool 5 cstr

  -- Load .env file for tests (to get OPENAI_API_KEY and other non-database configs)
  _ <- Safe.try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ())

  -- Load config from environment variables
  envConfig <- decodeWithDefaults defConfig

  let atAuthCtx =
        AuthContext
          envConfig
          pool
          pool
          pool
          hasqlMain
          hasqlJobs
          hasqlTf
          projectCache
          logsPatternCache
          projectKeyCache
          ( envConfig
              { -- Override to ensure test database is used (never production DB from .env)
                databaseUrl = "test-db-connection-from-pool"
              , timefusionPgUrl = "test-db-connection-from-pool"
              , apiKeyEncryptionSecretKey = "monoscope123456123456monoscope12"
              , convertkitApiKey = ""
              , convertkitApiSecret = ""
              , requestPubsubTopics = ["monoscope-prod-default"]
              , enableBackgroundJobs = True
              , enableEventsTableUpdates = True
              , enableDailyJobScheduling = False
              , -- Fallback values for external services (CI mode without .env)
                -- .env values take priority if set, otherwise use test defaults
                discordPublicKey = bool envConfig.discordPublicKey testDiscordPublicKeyHex (T.null envConfig.discordPublicKey)
              , twilioAccountSid = bool envConfig.twilioAccountSid "ACtest_account_sid_for_tests_only" (T.null envConfig.twilioAccountSid)
              , twilioAuthToken = bool envConfig.twilioAuthToken "test_auth_token_for_tests_only" (T.null envConfig.twilioAuthToken)
              , whatsappFromNumber = bool envConfig.whatsappFromNumber "+15555551234" (T.null envConfig.whatsappFromNumber)
              , slackBotToken = bool envConfig.slackBotToken "xoxb-test-token-not-real" (T.null envConfig.slackBotToken)
              , discordBotToken = bool envConfig.discordBotToken "test-discord-bot-token" (T.null envConfig.discordBotToken)
              , openaiApiKey = bool envConfig.openaiApiKey "sk-test-key-not-real" (T.null envConfig.openaiApiKey)
              }
          )
  f
    TestResources
      { trPool = pool
      , trProjectCache = projectCache
      , trSessAndHeader = sessAndHeader
      , trATCtx = atAuthCtx
      , trLogger = logger
      , trTracerProvider = tp
      }


toServantResponse
  :: AuthContext
  -> Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Projects.Session
  -> Log.Logger
  -> ATAuthCtx (RespHeaders a)
  -> IO (RespHeaders a, a)
toServantResponse trATCtx trSessAndHeader trLogger k = do
  tp <- getGlobalTracerProvider
  headersResp <-
    ( atAuthToBase trSessAndHeader k
        & effToServantHandlerTest trATCtx trLogger tp
        & ServantS.runHandler
    )
      <&> fromRightShow
  pure (headersResp, Servant.getResponse headersResp)


testServant :: TestResources -> ATAuthCtx (RespHeaders a) -> IO (RespHeaders a, a)
testServant tr = toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger


-- | Like testServant but also captures notifications sent during handler execution
testServantWithNotifications :: TestResources -> ATAuthCtx (RespHeaders a) -> IO ([Data.Effectful.Notify.Notification], (RespHeaders a, a))
testServantWithNotifications TestResources{..} k = do
  tp <- getGlobalTracerProvider
  notifRef <- newIORef []
  headersResp <-
    ( atAuthToBaseTest notifRef trSessAndHeader k
        & effToServantHandlerTest trATCtx trLogger tp
        & ServantS.runHandler
    )
      <&> fromRightShow
  notifications <- reverse <$> readIORef notifRef
  pure (notifications, (headersResp, Servant.getResponse headersResp))


-- | Run a base context handler (like webhookPostH, replayPostH) in test context
toBaseServantResponse
  :: AuthContext
  -> Log.Logger
  -> ATBaseCtx a
  -> IO a
toBaseServantResponse trATCtx trLogger k = do
  tp <- getGlobalTracerProvider
  ( k
      & effToServantHandlerTest trATCtx trLogger tp
      & ServantS.runHandler
    )
    <&> fromRightShow


-- | Run a query effect (like Charts.queryMetrics) in test context
-- This is for effects that return data directly (not wrapped in RespHeaders)
-- Uses frozen time to match background job context
runQueryEffect :: TestResources -> (forall es. (DB es, Effectful.Reader.Static.Reader AuthContext :> es, Error ServantS.ServerError :> es, IOE :> es, Time :> es) => Eff es a) -> IO a
runQueryEffect TestResources{..} action = do
  action
    & runErrorNoCallStack @ServantS.ServerError
    & Effectful.Reader.Static.runReader trATCtx
    & runConnectionPool trATCtx.pool
    & runFrozenTime (Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime)
    & runEff
    <&> fromRightShow


-- | Hasql twin of `runQueryEffect`.
runHasqlEffect :: TestResources -> (forall es. (Hasql :> es, Effectful.Reader.Static.Reader AuthContext :> es, Error ServantS.ServerError :> es, IOE :> es, Time :> es) => Eff es a) -> IO a
runHasqlEffect TestResources{..} action = do
  action
    & runErrorNoCallStack @ServantS.ServerError
    & Effectful.Reader.Static.runReader trATCtx
    & runHasqlPool trATCtx.hasqlPool
    & runFrozenTime (Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime)
    & runEff
    <&> fromRightShow


msg1 :: Text -> AE.Value
msg1 timestamp =
  [aesonQQ|{"duration":476434,
            "host":"172.31.29.11",
            "method":"GET",
            "path_params":{},
            "project_id":"00000000-0000-0000-0000-000000000000",
            "proto_minor":1,
            "proto_major":1,"query_params":{},
            "raw_url":"/","referer":"","request_body":"e30=",
            "request_headers":{
              "connection":["upgrade"],"host":["172.31.29.11"],
              "x-real-ip":["172.31.81.1"],"x-forwarded-for":["172.31.81.1"],
              "user-agent":["ELB-HealthChecker/2.0"],"accept-encoding":["gzip, compressed"]},
              "response_body":"V2VsY29tZSB0byBSZXRhaWxsb29w","response_headers":{"x-powered-by":["Express"],
              "vary":["Origin"],"access-control-allow-credentials":["true"],"content-type":["text/html; charset=utf-8"],
              "content-length":["21"],"etag":["W/\"15-2rFUmgZR2gmQik/+S8kDb7KSIZk\""]
            },
            "sdk_type":"JsExpress",
            "status_code":200,
            "timestamp": #{timestamp},
            "url_path":"/","errors":[],"tags":[]}
      |]


msg2 :: Text -> AE.Value
msg2 timestamp =
  [aesonQQ|{"timestamp": #{timestamp},
            "request_headers":{
                "Accept":["application/json, text/plain, */*"],
                "Accept-Encoding":["gzip, deflate, br"],
                "Accept-Language":["en-US,en;q=0.9"],"Access-Control-Allow-Headers":["Content-Type"],
                "Access-Control-Allow-Origin":["*"],"Authorization":["Bearer null"],"Content-Length":["62"],
                "Content-Type":["application/json"],"Forwarded":["for=\"[2a01:4b00:f65f:0:59b2:efe6:d68b:691c]\";proto=https"],
                "Origin":["https://test-admin-git-selectbox-daemon-team.vercel.app"],
                "Referer":["https://test-admin-git-selectbox-daemon-team.vercel.app/"],
                "Sec-Ch-Ua":["\"Google Chrome\";v=\"117\", \"Not;A=Brand\";v=\"8\", \"Chromium\";v=\"117\""],
                "Sec-Ch-Ua-Mobile":["?0"],"Sec-Ch-Ua-Platform":["\"macOS\""],"Sec-Fetch-Dest":["empty"],
                "Sec-Fetch-Mode":["cors"],"Sec-Fetch-Site":["cross-site"],
                "Traceparent":["00-064f45d39dadb50679db4755b295ff84-e79ae1ef6a4e42c0-00"],
                "User-Agent":["Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"],
                "X-Cloud-Trace-Context":["064f45d39dadb50679db4755b295ff84/16688899787571741376"],
                "X-Forwarded-For":["2a01:4b00:f65f:0:59b2:efe6:d68b:691c"],"X-Forwarded-Proto":["https"]},
                "query_params":{},"path_params":{},"response_headers":{"Access-Control-Allow-Credentials":["true"],
                "Access-Control-Allow-Origin":["*"],
                "Access-Control-Expose-Headers":["Content-Length,Authorization,X-Access-Token,X-Refresh-Token,Content-Type"],
                "Content-Type":["application/json; charset=utf-8"]},"method":"POST","sdk_type":"GoGin","host":"api.test.com",
                "raw_url":"/api/v1/user/login","referer":"https://test-admin-git-selectbox-daemon-team.vercel.app/",
            "project_id":"00000000-0000-0000-0000-000000000000","url_path":"/api/v1/user/login",
            "response_body":"eyJlcnJvcnMiOiJjcnlwdG8vYmNyeXB0OiBoYXNoZWRQYXNzd29yZCBpcyBub3QgdGhlIGhhc2ggb2YgdGhlIGdpdmVuIHBhc3N3b3JkIiwibWVzc2FnZSI6ImludmFsaWQgY3JlZGVudGlhbHMiLCJzdGF0dXMiOiJVbnByb2Nlc3NhYmxlIEVudGl0eSIsInRpbWVzdGFtcCI6Ik1vbmRheSwgMTYtT2N0LTIzIDIxOjQ3OjQxIFVUQyJ9",
            "request_body":"eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOiJhZG1pbkBncm92ZXBheS5jby51ayJ9",
            "proto_minor":1,
            "status_code":422,"proto_major":1,"duration":103636077}
        |]


testRequestMsgs :: TestRequestMessages
testRequestMsgs =
  RequestMessages
    { reqMsg1 = msg1
    , reqMsg2 = msg2
    }


data TestRequestMessages = RequestMessages
  { reqMsg1 :: Text -> AE.Value
  , reqMsg2 :: Text -> AE.Value
  }


-- FIXME: rename to some clearer. like toRequestMessage.
-- convert is too general
convert :: AE.Value -> Maybe ProcessMessage.RequestMessage
convert val = case AE.fromJSON val of
  AE.Success p -> Just p
  AE.Error _ -> Nothing


-- | Get all pending background jobs without executing them
-- This is useful for assertions and debugging in tests
getPendingBackgroundJobs :: AuthContext -> IO (V.Vector (Job, BackgroundJobs.BgJobs))
getPendingBackgroundJobs authCtx = do
  jobs <- withResource authCtx.pool getBackgroundJobs
  V.forM jobs \job -> do
    bgJob <- BackgroundJobs.throwParsePayload job
    pure (job, bgJob)


-- | Log background jobs info for debugging (respects LOG_LEVEL)
logBackgroundJobsInfo :: Log.Logger -> V.Vector (Job, BackgroundJobs.BgJobs) -> IO ()
logBackgroundJobsInfo logger jobs = do
  logLevel <- Logging.getLogLevelFromEnv
  let jobsList =
        V.toList jobs <&> \(job, bgJob) ->
          AE.object
            [ "type" AE..= BackgroundJobs.jobTypeName bgJob
            , "id" AE..= job.jobId
            ]
  runEff
    $ Logging.runLog "test" logger logLevel
    $ Log.logTrace "Background Jobs Queue" (AE.object ["count" AE..= V.length jobs, "jobs" AE..= jobsList])


runAllBackgroundJobs :: UTCTime -> AuthContext -> IO (V.Vector Job)
runAllBackgroundJobs t authCtx = do
  jobs <- withResource authCtx.pool getBackgroundJobs
  LogBulk.withBulkStdOutLogger \logger ->
    -- Run jobs concurrently using Ki structured concurrency
    runEff $ runConcurrent $ Ki.runStructuredConcurrency $ Ki.scoped \scope -> do
      threads <- V.forM jobs $ \job -> Ki.fork scope $ liftIO $ testJobsRunner t logger authCtx job
      V.forM_ threads $ Ki.atomically . Ki.await
  pure jobs


-- | Run background jobs matching a predicate
-- Useful for running only specific job types in tests
runBackgroundJobsWhere :: UTCTime -> AuthContext -> (BackgroundJobs.BgJobs -> Bool) -> IO (V.Vector Job)
runBackgroundJobsWhere t authCtx predicate = do
  jobs <- withResource authCtx.pool getBackgroundJobs
  jobsWithParsed <- V.forM jobs \job -> do
    bgJob <- BackgroundJobs.throwParsePayload job
    pure (job, bgJob)
  let filteredJobs = V.filter (\(_, bgJob) -> predicate bgJob) jobsWithParsed
  LogBulk.withBulkStdOutLogger \logger ->
    -- Run filtered jobs concurrently using Ki structured concurrency
    runEff $ runConcurrent $ Ki.runStructuredConcurrency $ Ki.scoped \scope -> do
      threads <- V.forM filteredJobs $ \(job, _) -> Ki.fork scope $ liftIO $ testJobsRunner t logger authCtx job
      V.forM_ threads $ Ki.atomically . Ki.await
  pure $ V.map fst filteredJobs


-- Test version of jobsRunner that uses the test notification runner
testJobsRunner :: UTCTime -> Log.Logger -> Config.AuthContext -> Job -> IO ()
testJobsRunner t logger authCtx job = Relude.when authCtx.config.enableBackgroundJobs $ do
  bgJob <- BackgroundJobs.throwParsePayload job
  void $ runTestBackgroundWithLogger t logger authCtx (BackgroundJobs.processBackgroundJob authCtx bgJob)


getBackgroundJobs :: Connection -> IO (V.Vector Job)
getBackgroundJobs conn = V.fromList <$> PGS.query conn q ()
  where
    q = [sql|SELECT id, created_at, updated_at, run_at, status, payload,last_error, attempts, locked_at, locked_by FROM background_jobs|]


setBjRunAtInThePast :: UTCTime -> Connection -> IO ()
setBjRunAtInThePast now conn = void $ PGS.execute conn q (Only now)
  where
    q = [sql|UPDATE background_jobs SET run_at = ?::timestamptz - INTERVAL '1 day' WHERE status = 'pending'|]


-- Helper function to process messages and run background jobs
processMessagesAndBackgroundJobs :: TestResources -> [(Text, ByteString)] -> IO ()
processMessagesAndBackgroundJobs tr@TestResources{..} msgs = do
  currentTime <- getCurrentTime
  let futureTime = addUTCTime 1 currentTime
  let testProjectId = UUIDId UUID.nil

  _ <- runTestBg frozenTime tr do
    _ <- ProcessMessage.processMessages msgs HM.empty
    _ <- BackgroundJobs.processOneMinuteErrors futureTime testProjectId
    BackgroundJobs.processFiveMinuteSpans futureTime testProjectId

  void $ runAllBackgroundJobs frozenTime trATCtx


-- Helper function to create test spans for endpoints
createTestSpans :: TestResources -> Projects.ProjectId -> Int -> IO ()
createTestSpans TestResources{..} projectId numRequestsPerEndpoint = do
  endpoints <-
    withResource trPool \conn ->
      V.fromList
        <$> PGS.query
          conn
          [sql|
    SELECT url_path, url_params, method, host, hash
    FROM apis.endpoints
    WHERE project_id = ?
  |]
          (Only projectId)
        :: IO (V.Vector (Text, AE.Value, Text, Text, Text))

  currentTime <- getCurrentTime
  forM_ endpoints $ \(path, _, method, host, hash) -> do
    forM_ [1 .. numRequestsPerEndpoint] $ \(_ :: Int) -> do
      spanId <- nextRandom
      traceIdVal <- nextRandom
      let attributes =
            AE.object
              [ "http.request.method" .= method
              , "http.request.path" .= path
              , "http.response.status_code" .= (200 :: Int)
              , "net.host.name" .= host
              , "url.path" .= path
              , "url.full" .= path
              ]
      let resource =
            AE.object
              [ "service" .= AE.object ["name" .= ("test-service" :: Text)]
              ]
      let context =
            Telemetry.Context
              { trace_id = Just $ UUID.toText traceIdVal
              , span_id = Just $ UUID.toText spanId
              , trace_state = Nothing
              , trace_flags = Nothing
              , is_remote = Nothing
              }
      -- Create OtelLogsAndSpans record to generate summary
      let otelRecord =
            Telemetry.OtelLogsAndSpans
              { id = UUID.toText spanId
              , project_id = projectId.toText
              , timestamp = currentTime
              , parent_id = Nothing
              , observed_timestamp = Nothing
              , hashes = V.singleton hash
              , name = Just (method <> " " <> path)
              , kind = Just "SERVER"
              , status_code = Just "200"
              , status_message = Nothing
              , level = Nothing
              , severity = Nothing
              , body = Nothing
              , duration = Just 100000000
              , start_time = currentTime
              , end_time = Nothing
              , context = Just context
              , events = Nothing
              , links = Nothing
              , attributes = case attributes of AE.Object km -> Just $ AesonText $ AEKM.toMapText km; _ -> Nothing
              , resource = case resource of AE.Object km -> Just $ AesonText $ AEKM.toMapText km; _ -> Nothing
              , date = currentTime
              , summary = V.empty -- Will be generated
              , errors = Nothing
              }
      let summary = Telemetry.generateSummary otelRecord
      void $ withResource trPool \conn ->
        PGS.execute
          conn
          [sql|
        INSERT INTO otel_logs_and_spans
        (id, project_id, timestamp, start_time, name, kind, status_code,
         duration, hashes, attributes, context, resource, resource___service___name, date, summary)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |]
          ( spanId
          , projectId.toText
          , currentTime
          , currentTime
          , Just (method <> " " <> path)
          , Just "SERVER"
          , Just "200"
          , Just (100000000 :: Int64)
          , V.singleton hash
          , case attributes of AE.Object km -> Just $ AEKM.toMapText km; _ -> Nothing
          , Just context
          , case resource of AE.Object km -> Just $ AEKM.toMapText km; _ -> Nothing
          , Just ("test-service" :: Text)
          , currentTime
          , summary
          )


-- OTLP/Telemetry helper functions for ingesting test data
-- These helpers allow tests to ingest data through handlers instead of raw SQL

-- | Helper to create an API key for testing using handler
createTestAPIKey :: TestResources -> Projects.ProjectId -> Text -> IO Text
createTestAPIKey tr projectId keyName = do
  (_, result) <- toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger $ Api.apiPostH projectId (Api.GenerateAPIKeyForm keyName Nothing)
  case result of
    Api.ApiPost _ _ (Just (_, keyText)) -> pure keyText
    _ -> error "Failed to create API key via handler"


-- | Ingest via resource attribute (key embedded in payload) or via header (key in Authorization header)
ingestLog, ingestLogWithHeader :: TestResources -> Text -> Text -> UTCTime -> IO ()
ingestLog tr apiKey bodyText timestamp =
  void $ OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createOtelLogAtTime apiKey bodyText timestamp)
ingestLogWithHeader tr apiKey bodyText timestamp =
  void $ runTestBg frozenTime tr $ OtlpServer.processLogsRequest (Just apiKey) (createOtelLogAtTime "" bodyText timestamp)


ingestTrace, ingestTraceWithHeader :: TestResources -> Text -> Text -> UTCTime -> IO ()
ingestTrace tr apiKey spanName timestamp = do
  req <- createOtelTraceAtTime apiKey spanName timestamp
  void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto req)
ingestTraceWithHeader tr apiKey spanName timestamp = do
  req <- createOtelTraceAtTime "" spanName timestamp
  void $ runTestBg frozenTime tr $ OtlpServer.processTraceRequest (Just apiKey) req


ingestTraceWithException :: TestResources -> Text -> Text -> Text -> Text -> Text -> UTCTime -> IO ()
ingestTraceWithException tr apiKey spanName excType excMessage excStacktrace timestamp = do
  req <- createOtelTraceWithExceptionAtTime apiKey spanName excType excMessage excStacktrace timestamp
  void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto req)


ingestMetric, ingestMetricWithHeader :: TestResources -> Text -> Text -> Double -> UTCTime -> IO ()
ingestMetric tr apiKey metricName value timestamp =
  void $ OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createGaugeMetricAtTime apiKey metricName value timestamp)
ingestMetricWithHeader tr apiKey metricName value timestamp =
  void $ runTestBg frozenTime tr $ OtlpServer.processMetricsRequest (Just apiKey) (createGaugeMetricAtTime "" metricName value timestamp)


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


mockResponse :: LBS.ByteString -> Response LBS.ByteString
mockResponse body =
  Response
    { responseStatus = ok200
    , responseVersion = http11
    , responseHeaders = [("Content-Type", "application/json")]
    , responseBody = body
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose pass
    , responseOriginalRequest = defaultRequest
    , responseEarlyHints = []
    }


extractParams :: W.Options -> [(Text, Text)]
extractParams opts = opts ^. W.params


-- | Test HTTP interpreter that routes CLI requests to server handlers.
-- Intercepts GetWith and routes based on URL path to the real handlers.
runHTTPtoServant :: IOE :> es => TestResources -> Eff (HTTP ': es) a -> Eff es a
runHTTPtoServant tr = interpret $ \_ -> \case
  GetWith opts url -> liftIO $ routeRequest tr (extractPath url) (extractParams opts)
  Get url -> liftIO $ routeRequest tr (extractPath url) []
  _ -> error "runHTTPtoServant: only GET is supported for CLI tests"


extractPath :: String -> Text
extractPath url =
  let t = toText url
      -- Strip scheme + host to get path
      afterScheme = fromMaybe t $ T.stripPrefix "https://" t <|> T.stripPrefix "http://" t
      path = T.dropWhile (/= '/') afterScheme
   in if T.null path then "/" else T.takeWhile (/= '?') path


lookupParam :: Text -> [(Text, Text)] -> Maybe Text
lookupParam key = fmap snd . find ((== key) . fst)


routeRequest :: TestResources -> Text -> [(Text, Text)] -> IO (Response LBS.ByteString)
routeRequest tr path params
  | "/log_explorer/schema" `T.isPrefixOf` path =
      pure $ mockResponse $ AE.encode Schema.telemetrySchemaJson
  | "/log_explorer" `T.isPrefixOf` path = do
      (_, pg) <-
        testServant tr
          $ Log.apiLogH testPid query Nothing Nothing since from to Nothing source Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing jsonParam Nothing Nothing Nothing Nothing
      pure $ mockResponse $ AE.encode pg
  | "/chart_data" `T.isPrefixOf` path = do
      result <-
        runQueryEffect tr
          $ Charts.queryMetrics Nothing Nothing (Just testPid) query Nothing since from to source []
      pure $ mockResponse $ AE.encode result
  | otherwise = error $ "runHTTPtoServant: unhandled path: " <> path
  where
    p = lookupParam
    query = p "query" params
    since = p "since" params
    from = p "from" params
    to = p "to" params
    source = p "source" params
    jsonParam = p "json" params


----------------------------------------------------------------------
-- OTLP Mock Data Constructors
----------------------------------------------------------------------

mkAttr :: Text -> Text -> PC.KeyValue
mkAttr k v = defMessage & PCF.key .~ k & PCF.value .~ (defMessage & PCF.stringValue .~ v)


hexPad :: Int -> Text -> ByteString
hexPad len t = fromRight "" $ B16.decode $ encodeUtf8 $ T.replicate (len - T.length t) "0" <> t


toNanos :: UTCTime -> Word64
toNanos = round . (* 1e9) . utcTimeToPOSIXSeconds


mkResource :: Text -> [PC.KeyValue] -> PR.Resource
mkResource apiKey extras = defMessage & PRF.attributes .~ ([mkAttr "service.name" "test-service", mkAttr "at-project-key" apiKey] <> extras)


createOtelLogAtTime :: Text -> Text -> UTCTime -> LS.ExportLogsServiceRequest
createOtelLogAtTime apiKey bodyText timestamp =
  let logRecord = defMessage & PLF.timeUnixNano .~ toNanos timestamp & PLF.body .~ (defMessage & PCF.stringValue .~ bodyText) & PLF.severityText .~ "INFO"
      scopeLog = defMessage & PLF.logRecords .~ [logRecord]
   in defMessage & LSF.resourceLogs .~ [defMessage & PLF.resource .~ mkResource apiKey [] & PLF.scopeLogs .~ [scopeLog]]


mkSpanRequest :: Text -> Text -> Maybe Text -> Text -> [Span'Event] -> Maybe Status -> [PC.KeyValue] -> PR.Resource -> UTCTime -> TS.ExportTraceServiceRequest
mkSpanRequest trId spanId parentSpanIdM spanName events statusM attrs resource timestamp =
  let startTime = toNanos timestamp
      endTime = startTime + 100000000
      otelSpan =
        defMessage
          & PTF.traceId
          .~ hexPad 32 trId & PTF.spanId
          .~ hexPad 16 spanId
            & PTF.parentSpanId
          .~ maybe "" (hexPad 16) parentSpanIdM
            & PTF.name
          .~ spanName & PTF.kind
          .~ PT.Span'SPAN_KIND_SERVER
            & PTF.startTimeUnixNano
          .~ startTime & PTF.endTimeUnixNano
          .~ endTime
            & PTF.attributes
          .~ attrs & PTF.events
          .~ events
            & maybe id (PTF.status .~) statusM
      scopeSpan = defMessage & PTF.spans .~ [otelSpan]
   in defMessage & TSF.resourceSpans .~ [defMessage & PTF.resource .~ resource & PTF.scopeSpans .~ [scopeSpan]]


createOtelSpanAtTime :: Text -> Text -> Text -> Maybe Text -> Text -> UTCTime -> TS.ExportTraceServiceRequest
createOtelSpanAtTime apiKey trId spanId parentSpanIdM spanName =
  mkSpanRequest trId spanId parentSpanIdM spanName [] Nothing [mkAttr "http.method" "GET"] (mkResource apiKey [])


createOtelTraceAtTime :: Text -> Text -> UTCTime -> IO TS.ExportTraceServiceRequest
createOtelTraceAtTime apiKey spanName timestamp = do
  trIdText <- UUID.toText <$> nextRandom
  spanIdText <- UUID.toText <$> nextRandom
  pure $ createOtelSpanAtTime apiKey trIdText spanIdText Nothing spanName timestamp


createGaugeMetricAtTime :: Text -> Text -> Double -> UTCTime -> MS.ExportMetricsServiceRequest
createGaugeMetricAtTime apiKey metricName value timestamp =
  let dataPoint = defMessage & PMF.timeUnixNano .~ toNanos timestamp & PMF.asDouble .~ value
      gauge = defMessage & PMF.dataPoints .~ [dataPoint]
      metric = defMessage & PMF.name .~ metricName & PMF.description .~ ("Test gauge metric: " <> metricName) & PMF.unit .~ "1" & PMF.gauge .~ gauge
      scopeMetric = defMessage & PMF.metrics .~ [metric]
   in defMessage & MSF.resourceMetrics .~ [defMessage & PMF.resource .~ mkResource apiKey [] & PMF.scopeMetrics .~ [scopeMetric]]


createOtelTraceWithExceptionAtTime :: Text -> Text -> Text -> Text -> Text -> UTCTime -> IO TS.ExportTraceServiceRequest
createOtelTraceWithExceptionAtTime apiKey spanName excType excMessage excStacktrace timestamp = do
  trIdText <- UUID.toText <$> nextRandom
  spanIdText <- UUID.toText <$> nextRandom
  let exceptionEvent =
        defMessage & PTF.name
          .~ "exception" & PTF.timeUnixNano
          .~ toNanos timestamp
            & PTF.attributes
          .~ [mkAttr "exception.type" excType, mkAttr "exception.message" excMessage, mkAttr "exception.stacktrace" excStacktrace]
      spanStatus = defMessage & PTF.code .~ PT.Status'STATUS_CODE_ERROR & PTF.message .~ excMessage
      resource = mkResource apiKey [mkAttr "telemetry.sdk.language" "nodejs"]
      -- No http.request.method: exception spans shouldn't match the HTTP span filter (attributes___http___request___method IS NOT NULL)
      attrs = [mkAttr "http.route" "/api/users/:id", mkAttr "http.response.status_code" "500"]
  pure $ mkSpanRequest trIdText spanIdText Nothing spanName [exceptionEvent] (Just spanStatus) attrs resource timestamp
