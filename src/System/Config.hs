module System.Config (EnvConfig (..), AuthContext (..), getAppContext, configToEnv, DeploymentEnv (..)) where

import Colourista.IO (blueMessage)
import Control.Exception.Safe qualified as Safe
import Data.Base64.Types qualified as B64
import Data.Cache (Cache, newCache)
import Data.Default (Default (..))
import Data.Map.Strict qualified as M
import Data.Pool as Pool (Pool, defaultPoolConfig, newPool, setNumStripes)
import Data.Pool qualified as Pool
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Migration qualified as Migrations
import Effectful
import Effectful.Fail (Fail)
import Effectful.Ki qualified as Ki
import Log (LogLevel (..))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Pkg.DeriveUtils qualified as DeriveUtils
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.Parser.Expr qualified as ParserExpr
import Pkg.TraceSessionCache qualified as TraceSessionCache
import Relude
import System.Clock (TimeSpec (TimeSpec))
import System.Directory (getDirectoryContents)
import System.Envy (DefConfig (..), FromEnv (..), ReadShowVar (..), Var (..), decodeWithDefaults, fromVar, toVar)
import System.Logging qualified as Logging
import "base64" Data.ByteString.Base64 qualified as B64
import "cryptohash-md5" Crypto.Hash.MD5 qualified as MD5


-- Default log level is Info (used by the Default deriving for EnvConfig)
instance Default LogLevel where
  def = LogInfo


data EnvConfig = EnvConfig
  { databaseUrl :: Text -- "DATABASE_URL"
  , timefusionPgUrl :: Text -- TIMEFUSION_PG_URL
  , port :: Int
  , grpcPort :: Int -- "GRPC_PORT"
  , migrationsDir :: Text -- "MIGRATIONS_DIR"
  , auth0ClientId :: Text
  , auth0Secret :: Text
  , auth0Domain :: Text
  , auth0LogoutRedirect :: Text
  , auth0Callback :: Text
  , testEmail :: Maybe Text
  , testPhoneNumber :: Maybe Text
  , apiKeyEncryptionSecretKey :: Text
  , messagesPerPubsubPullBatch :: Int
  , migrateAndInitializeOnStart :: Bool
  , requestPubsubTopics :: [Text]
  , enablePubsubService :: Bool
  , kafkaBrokers :: [Text]
  , kafkaGroupId :: Text
  , kafkaTopics :: [Text]
  , kafkaUsername :: Text
  , kafkaPassword :: Text
  , enableKafkaService :: Bool
  , consumerOnly :: Bool
  -- ^ CONSUMER_ONLY=True runs the message-queue consumers (Kafka/PubSub) plus
  -- the extraction / schema-learning pipeline (it flushes the spans this
  -- instance ingested, and powers stats/facets — so it must run wherever
  -- ingestion runs). Skips only the Warp HTTP server, gRPC OTLP server,
  -- odd-jobs runner and the global periodic-job timers. Lets you spin up an
  -- ingest-only instance without binding PORT/GRPC_PORT or clashing with a
  -- running server.
  , kafkaGroupConcurrency :: Int
  , smtpHost :: Text
  , smtpPort :: Int
  , smtpTls :: Bool
  , smtpUsername :: Text
  , smtpPassword :: Text
  , smtpSender :: Text
  , googleServiceAccountB64 :: TL.Text
  , convertkitApiKey :: Text
  , convertkitApiSecret :: Text
  , enableBackgroundJobs :: Bool
  , slackClientId :: Text
  , slackClientSecret :: Text
  , slackRedirectUri :: Text
  , slackBotToken :: Text
  , discordRedirectUri :: Text
  , discordClientId :: Text
  , discordClientSecret :: Text
  , discordBotToken :: Text
  , discordPublicKey :: Text
  , discordWebhookUrl :: Text
  , githubAppId :: Text
  , githubAppPrivateKey :: Text
  , githubAppName :: Text
  , githubClientId :: Text
  , githubClientSecret :: Text
  , courierClientKey :: Text
  , courierApiKey :: Text
  , environment :: Text
  , loggingDestination :: Logging.LoggingDestination
  , logLevel :: LogLevel
  , lemonSqueezyApiKey :: Text
  , lemonSqueezyUrl :: Text
  , lemonSqueezyCriticalUrl :: Text
  , postmarkToken :: Text
  , postmarkFromEmail :: Text
  , lemonSqueezyWebhookSecret :: Text
  , lsWebhookSigEnforce :: Bool
  , openaiApiKey :: Text
  , openaiModel :: Text
  , openaiSmallModel :: Text
  , openaiBaseUrl :: Text
  , hostUrl :: Text
  , monoscopePusherServiceAccountB64 :: Text
  , twilioAccountSid :: Text
  , twilioAuthToken :: Text
  , whatsappFromNumber :: Text
  , whatsappErrorTemplate :: Text
  , whatsappEndpointTemplate :: Text
  , whatsappAllReportTemplate :: Text
  , whatsappErrorReportTemplate :: Text
  , whatsappBotChart :: Text
  , whatsappBotText :: Text
  , whatsappDashboardList :: Text
  , whatsappShareWidget :: Text
  , rrwebTopics :: [Text]
  , replayBatchSize :: Int
  , s3Endpoint :: Text
  , s3Bucket :: Text
  , s3AccessKey :: Text
  , s3SecretKey :: Text
  , s3Region :: Text
  , enableReplayService :: Bool
  , enableTimefusionReads :: Bool
  , enableTimefusionWrites :: Bool
  , kafkaDeadLetterTopic :: Text
  , enableKafkaDeadLetterService :: Bool
  -- ^ Consume 'kafkaDeadLetterTopic' back through processList under its own
  -- consumer group. Failures requeue to the DLQ tail with a bumped
  -- attempt-count header — messages loop until they succeed or an engineer
  -- prunes them manually.
  , enableFreetier :: Bool
  , enableBrowserMonitoring :: Bool
  , enableSessionReplay :: Bool
  , -- External scripts configuration
    googleTagManagerId :: Maybe Text
  , googleAdsConversionId :: Maybe Text
  , facebookPixelId1 :: Maybe Text
  , facebookPixelId2 :: Maybe Text
  , linkedInPartnerId :: Maybe Text
  , postHogApiKey :: Maybe Text
  , postHogApiHost :: Maybe Text
  , crispWebsiteId :: Maybe Text
  , basicAuthEnabled :: Bool
  , basicAuthUsername :: Text
  , basicAuthPassword :: Text
  , telemetryApiKey :: Text
  , telemetryServiceName :: Text
  , enableEventsTableUpdates :: Bool
  , enableDailyJobScheduling :: Bool
  , maxConcurrentJobs :: Int
  , showDemoProject :: Bool
  , pauseNotifications :: Bool
  , liveReloadDashboards :: Bool
  , stripeSecretKey :: Text
  , stripeWebhookSecret :: Text
  , stripePublishableKey :: Text
  , stripePriceIdGraduated :: Text
  , stripePriceIdGraduatedOverage :: Text
  , stripePriceIdByos :: Text
  , extractionWorkerShards :: Int
  , extractionQueueCapacity :: Int
  , drainFlushBatchSize :: Int
  , drainFlushMaxAgeSecs :: Int
  , drainRehydrateIntervalSecs :: Int
  , maxBufferedSpans :: Int
  , maxDrainTrees :: Int
  , enableHashUpdates :: Bool
  -- ^ Kill switch for UPDATE-1 (eager hash merge) and UPDATE-2 (drain `pat:*`
  -- tag append) against `otel_logs_and_spans`. Set False to pause both writes
  -- when the table is under decompression pressure from compressed-chunk
  -- updates. The rest of the pipeline (schema learning, error extraction,
  -- pattern persistence) keeps running.
  , hashUpdateMaxAgeSecs :: Int
  -- ^ Skip UPDATE-1/UPDATE-2 for rows whose `timestamp` is older than this
  -- many seconds. Updates to already-compressed chunks force TimescaleDB to
  -- decompress them; capping at ~2h keeps writes on the hot, uncompressed
  -- tail.
  , -- Schema-learning knobs (see "Pkg.SchemaLearning.Hot").
    enableSchemaLearning :: Bool
  -- ^ Kill switch for the in-process schema-learning pipeline. When False,
  -- 'observeSpans' is skipped on the hot path and the flush fiber doesn't
  -- start. Use this if the catalog needs to be disabled in prod without a
  -- redeploy.
  , schemaFlushIntervalSecs :: Int
  , schemaCatalogExamples :: Int
  , schemaCatalogMaxKeysPerProject :: Int
  , schemaCatalogMaxBytesPerShard :: Int
  , schemaCatalogMaxFieldsPerEntry :: Int
  -- ^ Hard cap on @template.fields@ size for a single catalog entry. Past
  -- this, new field paths from the walk are silently dropped. Prevents one
  -- pathological span (deeply-nested vendor payload, high-cardinality JSON
  -- key) from blowing the shard.
  , schemaLearnFullThreshold :: Int
  , schemaLearnSampleEveryN :: Int
  , processedAtCutoff :: UTCTime
  -- ^ Must match the `timestamp >=` literal in migration 0064's partial index.
  -- The safety-net query and the partial-index WHERE clause both filter
  -- rows by this cutoff so pre-deploy rows stay invisible to the worker
  -- and the post-deploy rows get stamped via UPDATE-1.
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromEnv)


instance DefConfig EnvConfig where
  defConfig =
    (def :: EnvConfig)
      { port = 8080
      , environment = "DEV"
      , migrationsDir = "./static/migrations/"
      , messagesPerPubsubPullBatch = 1000
      , rrwebTopics = ["rrweb-client"]
      , replayBatchSize = 0 -- 0 = derive at runtime as messagesPerPubsubPullBatch `div` 2
      , loggingDestination = Logging.StdOut
      , logLevel = LogInfo -- Default to Info level
      , smtpPort = 465
      , smtpTls = True
      , maxConcurrentJobs = 4 -- Sane default, can be increased based on CPU cores
      , showDemoProject = False -- Default to hidden
      , postmarkFromEmail = "hello@monoscope.tech"
      , openaiModel = "gpt-5.4-mini"
      , openaiSmallModel = "gpt-5.4-nano"
      , kafkaGroupConcurrency = 4
      , enableKafkaDeadLetterService = True
      , extractionWorkerShards = 4
      , extractionQueueCapacity = 64
      , drainFlushBatchSize = 1000
      , drainFlushMaxAgeSecs = 60
      , drainRehydrateIntervalSecs = 300
      , maxBufferedSpans = 100000
      , maxDrainTrees = 200
      , enableHashUpdates = True
      , hashUpdateMaxAgeSecs = 7200
      , enableSchemaLearning = True
      , schemaFlushIntervalSecs = 60
      , schemaCatalogExamples = 20
      , schemaCatalogMaxKeysPerProject = 500
      , schemaCatalogMaxBytesPerShard = 67108864
      , schemaCatalogMaxFieldsPerEntry = 2000
      , schemaLearnFullThreshold = 200
      , schemaLearnSampleEveryN = 200
      , -- MUST match the literal in static/migrations/0064_processed_at_safety_net.sql.
        -- The partial index `idx_otel_unprocessed` filters on `timestamp >= this`,
        -- so a mismatched default would silently orphan post-cutoff rows from the
        -- safety-net query. `def :: UTCTime` is 1858-11-17 (ModifiedJulianDay 0),
        -- which would make every historical row eligible — the exact failure mode
        -- the migration H1 rationale rules out.
        processedAtCutoff = UTCTime (fromGregorian 2026 4 15) 0
      }


-- Support unmarshalling a coma separated text into a text list
instance Var [Text] where
  fromVar = Just . T.splitOn "," . toText
  toVar = toString . T.intercalate ","


-- Support unmarshalling LogLevel from environment variable
-- Accepts: "trace", "info", or "attention" (case-insensitive)
instance Var LogLevel where
  fromVar str = case T.toLower (toText str) of
    "trace" -> Just LogTrace
    "info" -> Just LogInfo
    "attention" -> Just LogAttention
    _ -> Nothing
  toVar LogTrace = "trace"
  toVar LogInfo = "info"
  toVar LogAttention = "attention"


-- Rename to AppContext
data AuthContext = AuthContext
  { env :: EnvConfig
  , pool :: Pool.Pool Connection
  , jobsPool :: Pool.Pool Connection
  , timefusionPgPool :: Pool.Pool Connection
  , hasqlPool :: OHasql.TracedPool
  , hasqlJobsPool :: OHasql.TracedPool
  , hasqlTimefusionPool :: OHasql.TracedPool
  , projectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , logsPatternCache :: Cache Projects.ProjectId (V.Vector Text)
  , projectKeyCache :: Cache Text (Maybe Projects.ProjectId)
  , extractionWorker :: ExtractionWorker.WorkerState Telemetry.OtelLogsAndSpans
  , traceSessionCache :: TraceSessionCache.TraceSessionCache
  , tfCircuit :: ExtractionWorker.CircuitBreaker
  , config :: EnvConfig
  , -- App-lifetime ki scope for fire-and-forget work that must outlive the request
    -- (Slack/Twilio handlers ACK fast, then process in the background). Nothing in
    -- non-server contexts (tests); see 'System.Tracing.forkBackground'.
    backgroundScope :: Maybe Ki.Scope
  }
  deriving stock (Generic)


-- ===============

data DeploymentEnv = Prod | Staging
  deriving stock (Generic, Read, Show)
  deriving (Var) via (ReadShowVar DeploymentEnv)


instance Default DeploymentEnv where
  def = Staging


configToEnv :: IOE :> es => EnvConfig -> Eff es AuthContext
configToEnv config = do
  let createPgConnIO = PG.connectPostgreSQL $ DeriveUtils.addKeepaliveParams $ encodeUtf8 config.databaseUrl
      -- Raise TimescaleDB DML decompression limit for UPDATE queries on compressed hypertables
      tfParams =
        DeriveUtils.appendConnParams
          [("options", "-c%20timescaledb.max_tuples_decompressed_per_dml_transaction%3D0")]
          (DeriveUtils.addKeepaliveParams (encodeUtf8 config.timefusionPgUrl))
  let createTimefusionPgConnIO = DeriveUtils.connectPostgreSQL tfParams
  when config.migrateAndInitializeOnStart $ liftIO do
    conn <- createPgConnIO
    initializationRes <- Migrations.runMigration conn Migrations.defaultOptions Migrations.MigrationInitialization
    blueMessage ("migration initialized " <> show initializationRes)
    migrationRes <- runPendingMigrations conn (toString config.migrationsDir :: FilePath)
    blueMessage ("migration result " <> show migrationRes)
    -- Fail-fast on MigrationError. postgresql-simple-migration stops at the
    -- first failure (e.g. a checksum mismatch from an edited migration file)
    -- and previously we logged + continued, so every later migration was
    -- silently skipped and the app booted with a broken schema. Caused the
    -- 2026-05-11 schema-flusher crash loop when an edit to 0091 blocked 0092.
    case migrationRes of
      Migrations.MigrationError msg ->
        Relude.error
          $ "migration failed: "
          <> toText msg
          <> " — refusing to start with a half-applied schema. \
             \If you edited an already-applied migration, add a new \
             \follow-up file instead of editing in place."
      _ -> pass
  pool <- liftIO $ Pool.newPool (Pool.defaultPoolConfig createPgConnIO PG.close 30 20 & setNumStripes (Just 4))
  jobsPool <- liftIO $ Pool.newPool (Pool.defaultPoolConfig createPgConnIO PG.close 30 10 & setNumStripes (Just 2))
  timefusionPgPool <- liftIO $ Pool.newPool (Pool.defaultPoolConfig createTimefusionPgConnIO PG.close 30 10 & setNumStripes (Just 2))
  let mainHasqlSettings = DeriveUtils.addKeepaliveParams $ encodeUtf8 config.databaseUrl
      tfHasqlSettings = tfParams
  hasqlPool <- liftIO $ DeriveUtils.mkHasqlPool 20 mainHasqlSettings
  hasqlJobsPool <- liftIO $ DeriveUtils.mkHasqlPool 10 mainHasqlSettings
  hasqlTimefusionPool <- liftIO $ DeriveUtils.mkHasqlPool 30 tfHasqlSettings
  projectCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  projectKeyCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  logsPatternCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  extractionWorker <- liftIO $ ExtractionWorker.initWorkerState config.extractionWorkerShards config.extractionQueueCapacity
  traceSessionCache <- liftIO TraceSessionCache.newTraceSessionCache
  tfCircuit <- liftIO ExtractionWorker.newCircuitBreaker
  -- Seed the parser whitelist + /api/v1/schema handler from a live
  -- introspection of @otel_logs_and_spans@. Non-fatal: a missing table
  -- during partial migration falls back to 'flattenedOtelAttributesBuiltin'
  -- so the server still boots.
  liftIO $ introspectAndCacheOtelColumns pool
  pure
    AuthContext
      { pool
      , jobsPool
      , timefusionPgPool
      , hasqlPool
      , hasqlJobsPool
      , hasqlTimefusionPool
      , env = config
      , projectCache
      , projectKeyCache
      , logsPatternCache
      , extractionWorker
      , traceSessionCache
      , tfCircuit
      , config
      , backgroundScope = Nothing
      }


-- | Same effect as @Migrations.runMigration conn defaultOptions (MigrationDirectory dir)@,
-- but checks all already-applied checksums in a single round trip instead of one @SELECT@
-- per file. On a remote/high-latency dev DB, 100+ migration files each costing a round trip
-- turned every ghcid reload into a ~30s stall. Files that are new or checksum-mismatched still
-- go through the library's own 'Migrations.MigrationFile' path, so failure detection (incl. the
-- 2026-05-11-style edited-migration case) is unchanged.
runPendingMigrations :: Connection -> FilePath -> IO (Migrations.MigrationResult String)
runPendingMigrations conn dir = do
  files <- sort . filter (not . isPrefixOf ".") <$> getDirectoryContents dir
  applied <- M.fromList . map (\m -> (decodeUtf8 m.schemaMigrationName, m.schemaMigrationChecksum)) <$> Migrations.getMigrations conn
  let checksum = encodeUtf8 . B64.extractBase64 . B64.encodeBase64 . MD5.hash
      isPending f = (\c -> M.lookup (toText f) applied /= Just (checksum c)) <$> readFileBS (dir <> "/" <> f)
  pending <- filterM isPending files
  Migrations.runMigration conn Migrations.defaultOptions $ Migrations.MigrationCommands [Migrations.MigrationFile f (dir <> "/" <> f) | f <- pending]


getAppContext :: Eff '[Fail, IOE] AuthContext
getAppContext = do
  config <- liftIO (decodeWithDefaults defConfig)
  configToEnv config


-- | Query 'information_schema.columns' for @otel_logs_and_spans@ once at
-- startup and seed 'ParserExpr.setFlattenedOtelColumns' with the dotted
-- form (@___@ → @.@). Best-effort: any exception (missing table during
-- migration, lost pg conn) logs a warning and falls back to the hand-coded
-- builtin so the server still boots.
introspectAndCacheOtelColumns :: Pool.Pool Connection -> IO ()
introspectAndCacheOtelColumns pool = do
  result <- Safe.try $ Pool.withResource pool $ \conn -> do
    rows <-
      PG.query_
        conn
        "SELECT column_name::text FROM information_schema.columns \
        \WHERE table_schema = 'public' AND table_name = 'otel_logs_and_spans'"
        :: IO [PG.Only Text]
    pure [c | PG.Only c <- rows]
  case result of
    Right cols ->
      let dotted = S.fromList [T.replace "___" "." c | c <- cols, "___" `T.isInfixOf` c]
       in ParserExpr.setFlattenedOtelColumns dotted
    Left (e :: SomeException) ->
      blueMessage $ "C1: otel_logs_and_spans introspection failed, using builtin attribute set: " <> show e
