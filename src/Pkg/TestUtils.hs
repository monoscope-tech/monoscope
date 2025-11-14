module Pkg.TestUtils (
  withSetup,
  abort,
  rollback,
  withTestResources,
  fromRightShow,
  TestResources (..),
  testSessionHeader,
  testRequestMsgs,
  TestRequestMessages (..),
  convert,
  runTestBackground,
  runTestBg,
  runAllBackgroundJobs,
  getPendingBackgroundJobs,
  logBackgroundJobsInfo,
  runBackgroundJobsWhere,
  refreshMaterializedView,
  setBjRunAtInThePast,
  toServantResponse,
  -- Helper functions for tests
  processMessagesAndBackgroundJobs,
  createRequestDumps,
  processEndpointAnomalyJobs,
  processAllBackgroundJobsMultipleTimes,
  processShapeAndFieldAnomalyJobs,
  processFormatAnomalyJobs,
)
where

import BackgroundJobs qualified
import Control.Exception (bracket_, finally, mask, throwIO)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (KeyValue (..))
import Data.Cache (Cache (..), newCache)
import Data.Default (Default (..))
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.Notify qualified
import Data.Effectful.UUID (runStaticUUID, runUUID)
import Data.Effectful.Wreq (runHTTPGolden, runHTTPWreq)
import Data.Either.Extra
import Data.HashMap.Strict qualified as HashMap
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Connection, Only (..), close, connectPostgreSQL, execute, execute_, query)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization))
import Database.PostgreSQL.Simple.Migration qualified as Migration
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Transaction (newSavepoint, rollbackToAndReleaseSavepoint)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact qualified as PgT
import Database.Postgres.Temp (cacheAction, cacheConfig, toConnectionString, withConfig, withDbCache)
import Database.Postgres.Temp qualified as TmpPostgres
import Effectful
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (runLabeled)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Reader.Static qualified
import Effectful.Time (runFrozenTime, runTime)
import Log qualified
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.SummaryGenerator qualified as SummaryGenerator
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import OddJobs.Job (Job (..))
import OpenTelemetry.Trace (TracerProvider, getGlobalTracerProvider)
import Pkg.DeriveUtils (AesonText (..))
import ProcessMessage qualified
import Relude
import Relude.Unsafe qualified as Unsafe
import RequestMessages qualified
import Servant qualified
import Servant.Server qualified as ServantS
import System.Clock (TimeSpec (TimeSpec))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Config qualified as Config
import System.Directory (getFileSize, listDirectory)
import System.Logging qualified as Logging
import System.Tracing qualified as Tracing
import System.Types (ATAuthCtx, ATBackgroundCtx, RespHeaders, atAuthToBase, effToServantHandlerTest)
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)


migrationsDirr :: FilePath
migrationsDirr = "./static/migrations/"


migrate :: TmpPostgres.DB -> IO ()
migrate db = do
  conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
  initializationRes <- Migration.runMigration conn Migration.defaultOptions MigrationInitialization

  migrationRes <- Migration.runMigration conn Migration.defaultOptions $ MigrationDirectory migrationsDirr
  -- Create a nil user and projects to make subsequent tests easier (with conflict handling)
  let q =
        [sql| INSERT into users.users (id, first_name, last_name, email) 
              VALUES ('00000000-0000-0000-0000-000000000000', 'FN', 'LN', 'test@apitoolkit.io')
              ON CONFLICT (id) DO NOTHING;
              
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
  _ <- execute conn q ()
  pass


-- Setup function that spins up a database with the db migrations already executed.
-- source: https://jfischoff.github.io/blog/keeping-database-tests-fast.html
withSetup :: (Pool Connection -> IO ()) -> IO ()
withSetup f = do
  useExternalDB <- lookupEnv "USE_EXTERNAL_DB"
  case useExternalDB of
    Just "true" -> withExternalDBSetup f
    _ -> withLocalSetup f


-- Local setup using tmp-postgres
withLocalSetup :: (Pool Connection -> IO ()) -> IO ()
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
    withConfig migratedConfig $ \db -> f =<< newPool (defaultPoolConfig (connectPostgreSQL $ toConnectionString db) close 60 50)


-- External database setup using template database approach for better isolation and performance
withExternalDBSetup :: (Pool Connection -> IO ()) -> IO ()
withExternalDBSetup f = do
  let masterConnStr = "host=localhost port=5432 user=postgres password=postgres dbname=postgres"
      templateDbName = "apitoolkit_test_template"

  -- Create or update template database
  ensureTemplateDatabase masterConnStr templateDbName

  -- Generate unique test database name using UUID (replace hyphens with underscores)
  uuid <- nextRandom
  let testDbName = "apitoolkit_test_" <> T.replace "-" "_" (T.pack $ show uuid)

  -- Create test database from template
  masterConn <- connectPostgreSQL masterConnStr
  _ <-
    execute
      masterConn
      (Query $ encodeUtf8 $ "CREATE DATABASE " <> testDbName <> " TEMPLATE " <> templateDbName)
      ()
  close masterConn

  -- Connect to the new test database
  let testConnStr = "host=localhost port=5432 user=postgres password=postgres dbname=" <> encodeUtf8 testDbName
  pool <- newPool (defaultPoolConfig (connectPostgreSQL testConnStr) close 60 50)

  -- Run tests and cleanup
  finally (f pool) $ do
    -- Close all connections in the pool
    destroyAllResources pool

    -- Drop the test database
    masterConn' <- connectPostgreSQL masterConnStr
    _ <-
      execute
        masterConn'
        (Query $ encodeUtf8 $ "DROP DATABASE IF EXISTS " <> testDbName)
        ()
    close masterConn'


-- Helper function to ensure template database exists and is up to date
ensureTemplateDatabase :: ByteString -> Text -> IO ()
ensureTemplateDatabase masterConnStr templateDbName = do
  masterConn <- connectPostgreSQL masterConnStr

  -- Check if template database exists
  [Only exists] <-
    query
      masterConn
      "SELECT EXISTS(SELECT 1 FROM pg_database WHERE datname = ?)"
      (Only templateDbName)

  -- Get current migration directory checksum for cache invalidation
  dirSize <- sum <$> (listDirectory migrationsDirr >>= mapM (getFileSize . (migrationsDirr <>)))
  let migrationChecksum = show dirSize

  -- Check if template needs updating
  needsUpdate <-
    if exists
      then do
        -- Try to connect to template and check a marker we'll set
        let templateConnStr = "host=localhost port=5432 user=postgres password=postgres dbname=" <> encodeUtf8 templateDbName
        templateConn <- connectPostgreSQL templateConnStr

        -- Check if our migration checksum marker exists and matches
        markerExists <-
          query
            templateConn
            "SELECT EXISTS(SELECT 1 FROM pg_tables WHERE schemaname = 'public' AND tablename = 'template_db_info')"
            ()
            :: IO [Only Bool]

        needsUpdate' <- case markerExists of
          [Only True] -> do
            checksums <-
              query
                templateConn
                "SELECT migration_checksum FROM template_db_info LIMIT 1"
                ()
                :: IO [Only Text]
            close templateConn
            pure $ case checksums of
              [Only checksum] -> checksum /= T.pack migrationChecksum
              _ -> True
          _ -> do
            close templateConn
            pure True

        pure needsUpdate'
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
      pure ()

    -- Create fresh template database
    _ <-
      execute
        masterConn
        (Query $ encodeUtf8 $ "CREATE DATABASE " <> templateDbName)
        ()

    -- Connect to template database and set up schema
    let templateConnStr = "host=localhost port=5432 user=postgres password=postgres dbname=" <> encodeUtf8 templateDbName
    templateConn <- connectPostgreSQL templateConnStr

    -- Run migrations
    _ <- Migration.runMigration templateConn Migration.defaultOptions MigrationInitialization
    _ <- Migration.runMigration templateConn Migration.defaultOptions $ MigrationDirectory migrationsDirr

    -- Create test user and project
    let setupData =
          [sql| INSERT into users.users (id, first_name, last_name, email) 
                VALUES ('00000000-0000-0000-0000-000000000000', 'FN', 'LN', 'test@apitoolkit.io')
                ON CONFLICT (id) DO NOTHING;
                
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
        (Only $ T.pack migrationChecksum)

    close templateConn

    -- Mark database as template
    _ <-
      execute
        masterConn
        (Query $ encodeUtf8 $ "ALTER DATABASE " <> templateDbName <> " is_template = true")
        ()
    pure ()

  close masterConn


-- throw away all db changes that happened within this abort block
abort :: (Connection -> IO a) -> Connection -> IO a
abort f conn =
  bracket_
    (execute_ conn "BEGIN")
    (execute_ conn "ROLLBACK")
    (f conn)


-- can be a nested transaction, and creates a savepoint which then gets auto rolled back,
-- while reusing all the setup and db operations that happened up until that savepoint
rollback :: Connection -> IO a -> IO a
rollback conn actionToRollback = mask $ \restore -> do
  sp <- newSavepoint conn
  restore actionToRollback `finally` rollbackToAndReleaseSavepoint conn sp


-- | `testSessionHeader` would log a user in and automatically generate a session header
-- which can be reused in subsequent tests
testSessionHeader :: MonadIO m => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session)
testSessionHeader pool = do
  pSessId <-
    Auth.authorizeUserAndPersist Nothing "firstName" "lastName" "https://placehold.it/500x500" "test@apitoolkit.io"
      & runStaticUUID (map (UUID.fromWords 0 0 0) [1 .. 10])
      & runHTTPGolden "./tests/golden/"
      & DB.runDB pool
      & runTime
      & runEff
      & liftIO

  -- Create a test project and add it to the user's session
  let testProjectId = Projects.ProjectId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
  _ <-
    withPool pool
      $ DBT.execute
        [sql|INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
         VALUES (?, 'Test Project', 'FREE', true, NULL, true, true)
         ON CONFLICT (id) DO UPDATE SET payment_plan = 'FREE', active = true, deleted_at = NULL, weekly_notif = true, daily_notif = true|]
        (Only testProjectId)

  -- Add project member permissions
  _ <-
    withPool pool
      $ DBT.execute
        [sql|INSERT INTO projects.project_members (project_id, user_id, permission)
         VALUES (?, '00000000-0000-0000-0000-000000000001', 'admin')
         ON CONFLICT (project_id, user_id) DO UPDATE SET permission = 'admin'|]
        (Only testProjectId)

  tp <- liftIO getGlobalTracerProvider
  logger <- liftIO $ Log.mkLogger "test" (\_ -> pure ())
  Auth.sessionByID (Just pSessId) "requestID" False "light" Nothing
    & runErrorNoCallStack @Servant.ServerError
    & DB.runDB pool
    & runTime
    & Logging.runLog "test" logger
    & Tracing.runTracing tp
    & runUUID
    & runHTTPWreq
    & runEff
    & liftIO
    <&> fromRightShow


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


runTestBackground :: Config.AuthContext -> ATBackgroundCtx a -> IO a
runTestBackground authCtx action = LogBulk.withBulkStdOutLogger \logger ->
  runTestBackgroundWithLogger logger authCtx action


runTestBackgroundWithLogger :: Log.Logger -> Config.AuthContext -> ATBackgroundCtx a -> IO a
runTestBackgroundWithLogger logger appCtx process = do
  tp <- getGlobalTracerProvider
  (notifications, result) <-
    process
      & Data.Effectful.Notify.runNotifyTest
      & Effectful.Reader.Static.runReader appCtx
      & DB.runDB appCtx.pool
      & runLabeled @"timefusion" (DB.runDB appCtx.timefusionPgPool)
      & runFrozenTime (Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime)
      & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
      & Tracing.runTracing tp
      & runUUID
      & runHTTPWreq
      & ELLM.runLLMGolden "./tests/golden/"
      & runConcurrent
      & Ki.runStructuredConcurrency
      & Effectful.runEff
  -- Log the notifications that would have been sent
  forM_ notifications \notification -> do
    let notifInfo = case notification of
          Data.Effectful.Notify.EmailNotification emailData ->
            ("Email" :: Text, Data.Effectful.Notify.receiver emailData, fmap fst (Data.Effectful.Notify.templateOptions emailData))
          Data.Effectful.Notify.SlackNotification slackData ->
            ("Slack" :: Text, Data.Effectful.Notify.webhookUrl slackData, Nothing :: Maybe Text)
          Data.Effectful.Notify.DiscordNotification discordData ->
            ("Discord" :: Text, Data.Effectful.Notify.channelId discordData, Nothing :: Maybe Text)
          Data.Effectful.Notify.WhatsAppNotification whatsappData ->
            ("WhatsApp" :: Text, Data.Effectful.Notify.to whatsappData, Just (Data.Effectful.Notify.template whatsappData))
    Log.logInfo "Notification" notifInfo
      & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
      & Effectful.runEff
    Log.logTrace "Notification payload" notification
      & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
      & Effectful.runEff
  pure result


-- | Run a background job action using TestResources
runTestBg :: TestResources -> ATBackgroundCtx a -> IO a
runTestBg TestResources{..} = runTestBackgroundWithLogger trLogger trATCtx


-- New type to hold all our resources
data TestResources = TestResources
  { trPool :: Pool Connection
  , trProjectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , trSessAndHeader :: Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  , trATCtx :: AuthContext
  , trLogger :: Log.Logger
  , trTracerProvider :: TracerProvider
  }


-- Compose withSetup with additional IO actions
withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources f = withSetup $ \pool -> LogBulk.withBulkStdOutLogger \logger -> do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  projectKeyCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  logsPatternCache <- newCache (Just $ TimeSpec (30 * 60) 0) -- Cache for log patterns, 30 minutes TTL
  sessAndHeader <- testSessionHeader pool
  tp <- getGlobalTracerProvider

  -- Load OpenAI API key from environment for tests
  openaiKey <- fromMaybe "" <$> lookupEnv "OPENAI_API_KEY"

  let atAuthCtx =
        AuthContext
          (def @EnvConfig)
          pool
          pool
          pool
          projectCache
          logsPatternCache
          projectKeyCache
          ( (def :: EnvConfig)
              { apiKeyEncryptionSecretKey = "apitoolkit123456123456apitoolkit"
              , convertkitApiKey = ""
              , convertkitApiSecret = ""
              , requestPubsubTopics = ["monoscope-prod-default"]
              , enableBackgroundJobs = True
              , enableEventsTableUpdates = True
              , enableDailyJobScheduling = False
              , openaiApiKey = toText openaiKey
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
  -> Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  -> Log.Logger
  -> ATAuthCtx (RespHeaders a)
  -> IO a
toServantResponse trATCtx trSessAndHeader trLogger k = do
  tp <- getGlobalTracerProvider
  ( atAuthToBase trSessAndHeader k
      & effToServantHandlerTest trATCtx trLogger tp
      & ServantS.runHandler
    )
    <&> Servant.getResponse
    . fromRightShow


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
convert :: AE.Value -> Maybe RequestMessages.RequestMessage
convert val = case AE.fromJSON val of
  AE.Success p -> Just p
  AE.Error _ -> Nothing


-- | Get all pending background jobs without executing them
-- This is useful for assertions and debugging in tests
getPendingBackgroundJobs :: AuthContext -> IO (V.Vector (Job, BackgroundJobs.BgJobs))
getPendingBackgroundJobs authCtx = do
  jobs <- withPool authCtx.pool getBackgroundJobs
  jobsWithParsed <- V.forM jobs \job -> do
    bgJob <- BackgroundJobs.throwParsePayload job
    pure (job, bgJob)
  pure jobsWithParsed


-- | Log background jobs info for debugging
logBackgroundJobsInfo :: V.Vector (Job, BackgroundJobs.BgJobs) -> IO ()
logBackgroundJobsInfo jobs = do
  putTextLn $ "\n=== Background Jobs Queue (Count: " <> show (V.length jobs) <> ") ==="
  V.forM_ jobs \(job, bgJob) -> do
    let jobType = BackgroundJobs.jobTypeName bgJob
    putTextLn $ "  - " <> jobType <> " (ID: " <> show job.jobId <> ")"
  putTextLn "=================================\n"


runAllBackgroundJobs :: AuthContext -> IO (V.Vector Job)
runAllBackgroundJobs authCtx = do
  jobs <- withPool authCtx.pool getBackgroundJobs
  LogBulk.withBulkStdOutLogger \logger ->
    -- Run jobs concurrently using Ki structured concurrency
    runEff $ runConcurrent $ Ki.runStructuredConcurrency $ Ki.scoped \scope -> do
      threads <- V.forM jobs $ \job -> Ki.fork scope $ liftIO $ testJobsRunner logger authCtx job
      V.forM_ threads $ Ki.atomically . Ki.await
  pure jobs


-- | Run background jobs matching a predicate
-- Useful for running only specific job types in tests
runBackgroundJobsWhere :: AuthContext -> (BackgroundJobs.BgJobs -> Bool) -> IO (V.Vector Job)
runBackgroundJobsWhere authCtx predicate = do
  jobs <- withPool authCtx.pool getBackgroundJobs
  jobsWithParsed <- V.forM jobs \job -> do
    bgJob <- BackgroundJobs.throwParsePayload job
    pure (job, bgJob)
  let filteredJobs = V.filter (\(_, bgJob) -> predicate bgJob) jobsWithParsed
  LogBulk.withBulkStdOutLogger \logger ->
    -- Run filtered jobs concurrently using Ki structured concurrency
    runEff $ runConcurrent $ Ki.runStructuredConcurrency $ Ki.scoped \scope -> do
      threads <- V.forM filteredJobs $ \(job, _) -> Ki.fork scope $ liftIO $ testJobsRunner logger authCtx job
      V.forM_ threads $ Ki.atomically . Ki.await
  pure $ V.map fst filteredJobs


-- Test version of jobsRunner that uses the test notification runner
testJobsRunner :: Log.Logger -> Config.AuthContext -> Job -> IO ()
testJobsRunner logger authCtx job = Relude.when authCtx.config.enableBackgroundJobs $ do
  bgJob <- BackgroundJobs.throwParsePayload job
  void $ runTestBackgroundWithLogger logger authCtx (BackgroundJobs.processBackgroundJob authCtx job bgJob)


getBackgroundJobs :: PgT.DBT IO (V.Vector Job)
getBackgroundJobs = DBT.query q ()
  where
    q = [sql|SELECT id, created_at, updated_at, run_at, status, payload,last_error, attempts, locked_at, locked_by FROM background_jobs|]


setBjRunAtInThePast :: PgT.DBT IO ()
setBjRunAtInThePast = void $ PgT.execute q ()
  where
    q = [sql|UPDATE background_jobs SET run_at = CURRENT_DATE - INTERVAL '1 day' WHERE status = 'pending'|]


refreshMaterializedView :: Text -> PgT.DBT IO Int64
refreshMaterializedView name = PgT.execute (Query $ encodeUtf8 q) ()
  where
    q = [text|REFRESH MATERIALIZED VIEW $name|]


-- Helper function to process messages and run background jobs
processMessagesAndBackgroundJobs :: TestResources -> [(Text, ByteString)] -> IO ()
processMessagesAndBackgroundJobs tr@TestResources{..} msgs = do
  currentTime <- getCurrentTime
  let futureTime = addUTCTime 1 currentTime
  let testProjectId = Projects.ProjectId UUID.nil

  _ <- runTestBg tr do
    _ <- ProcessMessage.processMessages msgs HashMap.empty
    _ <- BackgroundJobs.processOneMinuteErrors futureTime testProjectId
    BackgroundJobs.processFiveMinuteSpans futureTime testProjectId

  void $ runAllBackgroundJobs trATCtx


-- Helper function to create request dumps for endpoints
createRequestDumps :: TestResources -> Projects.ProjectId -> Int -> IO ()
createRequestDumps TestResources{..} projectId numRequestsPerEndpoint = do
  endpoints <-
    withPool trPool
      $ DBT.query
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
      let summary = SummaryGenerator.generateSummary otelRecord
      withPool trPool
        $ DBT.execute
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


-- Helper function to process endpoint anomaly jobs
processEndpointAnomalyJobs :: TestResources -> IO ()
processEndpointAnomalyJobs tr@TestResources{..} = do
  bgJobs <- runAllBackgroundJobs trATCtx
  whenJust (V.find isEndpointAnomalyJob bgJobs) \job -> do
    bgJob <- BackgroundJobs.throwParsePayload job
    void $ runTestBg tr $ BackgroundJobs.processBackgroundJob trATCtx job bgJob
  where
    isEndpointAnomalyJob (Job{jobPayload}) = case jobPayload of
      AE.Object obj ->
        case (AEKM.lookup "tag" obj, AEKM.lookup "anomalyType" obj) of
          (Just (AE.String "NewAnomaly"), Just (AE.String "endpoint")) -> True
          _ -> False
      _ -> False


-- Helper function to process all background jobs until none remain
-- Some background jobs spawn other jobs, so we run iteratively
processAllBackgroundJobsMultipleTimes :: TestResources -> IO ()
processAllBackgroundJobsMultipleTimes TestResources{..} = go 10 -- max 10 iterations to prevent infinite loops
  where
    go 0 = pass -- Safety limit reached
    go n = do
      jobs <- runAllBackgroundJobs trATCtx
      unless (V.null jobs) $ go (n - 1)


-- Helper function to process shape and field anomaly jobs
processShapeAndFieldAnomalyJobs :: TestResources -> IO ()
processShapeAndFieldAnomalyJobs tr@TestResources{..} = do
  bgJobs <- runAllBackgroundJobs trATCtx
  let anomalyJobs = V.filter isShapeOrFieldAnomalyJob bgJobs
  forM_ anomalyJobs \job -> do
    bgJob <- BackgroundJobs.throwParsePayload job
    void $ runTestBg tr $ BackgroundJobs.processBackgroundJob trATCtx job bgJob
  where
    isShapeOrFieldAnomalyJob (Job{jobPayload}) = case jobPayload of
      AE.Object obj ->
        case (AEKM.lookup "tag" obj, AEKM.lookup "anomalyType" obj) of
          (Just (AE.String "NewAnomaly"), Just (AE.String aType)) ->
            aType == "shape" || aType == "field"
          _ -> False
      _ -> False


-- Helper function to process format anomaly jobs
processFormatAnomalyJobs :: TestResources -> IO ()
processFormatAnomalyJobs tr@TestResources{..} = do
  bgJobs <- runAllBackgroundJobs trATCtx
  let formatJobs = V.filter isFormatAnomalyJob bgJobs
  forM_ formatJobs \job -> do
    bgJob <- BackgroundJobs.throwParsePayload job
    void $ runTestBg tr $ BackgroundJobs.processBackgroundJob trATCtx job bgJob
  where
    isFormatAnomalyJob (Job{jobPayload}) = case jobPayload of
      AE.Object obj ->
        case (AEKM.lookup "tag" obj, AEKM.lookup "anomalyType" obj) of
          (Just (AE.String "NewAnomaly"), Just (AE.String "format")) -> True
          _ -> False
      _ -> False
