module System.Config (EnvConfig (..), AuthContext (..), getAppContext, configToEnv, DeploymentEnv (..)) where

import Colourista.IO (blueMessage)
import Data.Cache (Cache, newCache)
import Data.Default (Default (..))
import Data.Pool as Pool (Pool, defaultPoolConfig, newPool, setNumStripes)
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
import Hasql.Pool qualified as HPool
import Log (LogLevel (..))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils qualified as DeriveUtils
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.TraceSessionCache qualified as TraceSessionCache
import Relude
import System.Clock (TimeSpec (TimeSpec))
import System.Envy (DefConfig (..), FromEnv (..), ReadShowVar (..), Var (..), decodeWithDefaults, fromVar, toVar)
import System.Logging qualified as Logging


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
  , s3Endpoint :: Text
  , s3Bucket :: Text
  , s3AccessKey :: Text
  , s3SecretKey :: Text
  , s3Region :: Text
  , enableReplayService :: Bool
  , enableTimefusionReads :: Bool
  , enableTimefusionWrites :: Bool
  , kafkaDeadLetterTopic :: Text
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
      , messagesPerPubsubPullBatch = 200
      , rrwebTopics = ["rrweb-client"]
      , loggingDestination = Logging.StdOut
      , logLevel = LogInfo -- Default to Info level
      , smtpPort = 465
      , smtpTls = True
      , maxConcurrentJobs = 4 -- Sane default, can be increased based on CPU cores
      , showDemoProject = False -- Default to hidden
      , postmarkFromEmail = "hello@monoscope.tech"
      , openaiModel = "gpt-5.4-mini"
      , openaiSmallModel = "gpt-5.4-nano"
      , extractionWorkerShards = 4
      , extractionQueueCapacity = 64
      , drainFlushBatchSize = 1000
      , drainFlushMaxAgeSecs = 60
      , drainRehydrateIntervalSecs = 300
      , maxBufferedSpans = 100000
      , maxDrainTrees = 200
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
  , hasqlPool :: HPool.Pool
  , hasqlJobsPool :: HPool.Pool
  , hasqlTimefusionPool :: HPool.Pool
  , projectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , logsPatternCache :: Cache Projects.ProjectId (V.Vector Text)
  , projectKeyCache :: Cache Text (Maybe Projects.ProjectId)
  , extractionWorker :: ExtractionWorker.WorkerState Telemetry.OtelLogsAndSpans
  , traceSessionCache :: TraceSessionCache.TraceSessionCache
  , tfCircuit :: ExtractionWorker.CircuitBreaker
  , config :: EnvConfig
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
    migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ Migrations.MigrationDirectory (toString config.migrationsDir :: FilePath)
    blueMessage ("migration result " <> show migrationRes)
    pass
  pool <- liftIO $ Pool.newPool (Pool.defaultPoolConfig createPgConnIO PG.close 30 20 & setNumStripes (Just 4))
  jobsPool <- liftIO $ Pool.newPool (Pool.defaultPoolConfig createPgConnIO PG.close 30 10 & setNumStripes (Just 2))
  timefusionPgPool <- liftIO $ Pool.newPool (Pool.defaultPoolConfig createTimefusionPgConnIO PG.close 30 10 & setNumStripes (Just 2))
  let mainHasqlSettings = DeriveUtils.addKeepaliveParams $ encodeUtf8 config.databaseUrl
      tfHasqlSettings = tfParams
  hasqlPool <- liftIO $ DeriveUtils.mkHasqlPool 20 mainHasqlSettings
  hasqlJobsPool <- liftIO $ DeriveUtils.mkHasqlPool 10 mainHasqlSettings
  hasqlTimefusionPool <- liftIO $ DeriveUtils.mkHasqlPool 10 tfHasqlSettings
  projectCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  projectKeyCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  logsPatternCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  extractionWorker <- liftIO $ ExtractionWorker.initWorkerState config.extractionWorkerShards config.extractionQueueCapacity
  traceSessionCache <- liftIO TraceSessionCache.newTraceSessionCache
  tfCircuit <- liftIO ExtractionWorker.newCircuitBreaker
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
      }


getAppContext :: Eff '[Fail, IOE] AuthContext
getAppContext = do
  config <- liftIO (decodeWithDefaults defConfig)
  configToEnv config
