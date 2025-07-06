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
  runAllBackgroundJobs,
  refreshMaterializedView,
  setBjRunAtInThePast,
  toServantResponse,
)
where

import BackgroundJobs qualified
import Control.Concurrent (threadDelay)
import Control.Exception (bracket_, finally, mask, throwIO)
import Control.Monad (void, when)
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Cache (Cache (..), newCache)
import Data.Default (Default (..))
import Data.Effectful.Notify qualified
import Data.Effectful.UUID (runStaticUUID, runUUID)
import Data.Effectful.Wreq (runHTTPGolden, runHTTPWreq)
import Data.Either.Extra
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
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
import Effectful.Concurrent.Async (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (runLabeled)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Reader.Static qualified
import Effectful.Time (runTime)
import Log qualified
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import OddJobs.Job (Job)
import Relude
import RequestMessages qualified
import Servant qualified
import Servant.Server qualified as ServantS
import System.Clock (TimeSpec (TimeSpec))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Config qualified as Config
import System.Directory (getFileSize, listDirectory)
import System.Logging qualified as Logging
import System.Types (ATAuthCtx, ATBackgroundCtx, RespHeaders, atAuthToBase, effToServantHandlerTest, runBackground)
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


-- Clean all data from the database while preserving schema
-- This function uses TRUNCATE CASCADE to efficiently remove all data
cleanDatabase :: Connection -> IO ()
cleanDatabase conn = do
  -- Get all user-created tables from all schemas
  tables <- query conn getAllTablesQuery ()

  -- Truncate all tables with CASCADE to handle foreign keys
  -- We do this in a transaction to ensure atomicity
  unless (null tables) $ do
    -- First, drop any materialized views that might prevent truncation
    _ <- execute_ conn "BEGIN"

    -- Get and drop all materialized views
    matViews <- query conn getMatViewsQuery () :: IO [Only Text]
    forM_ matViews $ \(Only viewName) -> do
      let dropQuery = Query $ encodeUtf8 $ "DROP MATERIALIZED VIEW IF EXISTS " <> viewName <> " CASCADE"
      execute_ conn dropQuery

    -- Build TRUNCATE statement for all tables at once
    let tableNames = map (\(schema, table) -> schema <> "." <> table) tables
    let truncateQuery = Query $ encodeUtf8 $ "TRUNCATE TABLE " <> T.intercalate ", " tableNames <> " RESTART IDENTITY CASCADE"

    _ <- execute_ conn truncateQuery

    -- Also clean up the background_jobs table if it exists
    _ <- execute_ conn "TRUNCATE TABLE IF EXISTS background_jobs RESTART IDENTITY CASCADE"

    void $ execute_ conn "COMMIT"
  where
    getAllTablesQuery =
      [sql|
        SELECT table_schema, table_name 
        FROM information_schema.tables 
        WHERE table_schema IN ('users', 'projects', 'apis', 'monitors', 'tests')
          AND table_type = 'BASE TABLE'
        ORDER BY table_schema, table_name
      |]

    getMatViewsQuery =
      [sql|
        SELECT schemaname || '.' || matviewname
        FROM pg_matviews
        WHERE schemaname IN ('users', 'projects', 'apis', 'monitors', 'tests')
      |]


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
      _ <-
        execute
          masterConn
          [sql| SELECT pg_terminate_backend(pg_stat_activity.pid)
              FROM pg_stat_activity
              WHERE pg_stat_activity.datname = ?
                AND pid <> pg_backend_pid() |]
          (Only templateDbName)

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
  Auth.sessionByID (Just pSessId) "requestID" False Nothing
    & runErrorNoCallStack @Servant.ServerError
    & DB.runDB pool
    & runTime
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
  (notifications, result) <- process
    & Data.Effectful.Notify.runNotifyTest
    & Effectful.Reader.Static.runReader appCtx
    & DB.runDB appCtx.pool
    & runLabeled @"timefusion" (DB.runDB appCtx.timefusionPgPool)
    & runTime
    & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
    & runUUID
    & runHTTPWreq
    & Ki.runStructuredConcurrency
    & Effectful.runEff
  -- Log the notifications that would have been sent
  forM_ notifications $ \notification ->
    Log.logInfo "Test: Notification captured" notification
      & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
      & Effectful.runEff
  pure result


-- New type to hold all our resources
data TestResources = TestResources
  { trPool :: Pool Connection
  , trProjectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , trSessAndHeader :: Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  , trATCtx :: AuthContext
  , trLogger :: Log.Logger
  }


-- Compose withSetup with additional IO actions
withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources f = withSetup $ \pool -> LogBulk.withBulkStdOutLogger \logger -> do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  projectKeyCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  sessAndHeader <- testSessionHeader pool
  let atAuthCtx =
        AuthContext
          (def @EnvConfig)
          pool
          pool
          pool
          projectCache
          projectKeyCache
          ( (def :: EnvConfig)
              { apiKeyEncryptionSecretKey = "apitoolkit123456123456apitoolkit"
              , convertkitApiKey = ""
              , convertkitApiSecret = ""
              , requestPubsubTopics = ["apitoolkit-prod-default"]
              , enableBackgroundJobs = False
              }
          )
  f
    TestResources
      { trPool = pool
      , trProjectCache = projectCache
      , trSessAndHeader = sessAndHeader
      , trATCtx = atAuthCtx
      , trLogger = logger
      }


toServantResponse
  :: AuthContext
  -> Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  -> Log.Logger
  -> ATAuthCtx (RespHeaders a)
  -> IO a
toServantResponse trATCtx trSessAndHeader trLogger k = do
  ( atAuthToBase trSessAndHeader k
      & effToServantHandlerTest trATCtx trLogger
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


runAllBackgroundJobs :: AuthContext -> IO (V.Vector Job)
runAllBackgroundJobs authCtx = do
  jobs <- withPool authCtx.pool getBackgroundJobs
  LogBulk.withBulkStdOutLogger \logger ->
    V.forM_ jobs $ testJobsRunner logger authCtx
  pure jobs

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
