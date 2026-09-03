module System.Config (EnvConfig (..), TwilioContentSid, mkTwilioContentSid, twilioContentSidText, AuthContext (..), CodeBlobKey, getAppContext, configToEnv, DeploymentEnv (..), runPendingMigrations) where

import Colourista.IO (blueMessage)
import Control.Exception.Safe qualified as Safe
import Data.Base64.Types qualified as B64
import Data.Cache (Cache, newCache)
import Data.Char (isHexDigit)
import Data.Default (Default (..))
import Data.Map.Strict qualified as M
import Data.Pool as Pool (Pool, defaultPoolConfig, newPool, setNumStripes)
import Data.Pool qualified as Pool
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
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.ContainerTypes qualified as Containers
import Models.Telemetry.RUM qualified as RUM
import Models.Telemetry.Telemetry qualified as Telemetry
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Pkg.DeriveUtils qualified as DeriveUtils
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.Git qualified as Git
import Pkg.LiveTail qualified as LiveTail
import Pkg.Parser.Expr qualified as ParserExpr
import Pkg.TraceSessionCache qualified as TraceSessionCache
import Relude
import System.Clock (TimeSpec (TimeSpec))
import System.Directory (getDirectoryContents)
import System.Envy (DefConfig (..), FromEnv (..), Var (..), decodeWithDefaults, fromVar, toVar)
import System.Logging qualified as Logging
import Text.Show (showString, showsPrec)
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
  , enableOtlpGrpcService :: Bool -- "ENABLE_OTLP_GRPC_SERVICE": bind the gRPC OTLP listener (GRPC_PORT). Off in dev avoids port-4317 rebind clashes on ghcid reload.
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
  , environment :: DeploymentEnv
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
  , -- Kill switch for the LLM review of endpoint groups the classifier could
    -- not decide. Report-only regardless; this turns off the spend.
    enableEndpointGroupReview :: Bool
  , -- Whether a verdict that has cleared 'mergeEvidenceMet' may merge rows, as
    -- opposed to only being recorded. Off turns the whole thing into a report.
    enableEndpointGroupAutoApply :: Bool
  , -- The same pair for the error-group review. Kill switch for the spend, and
    -- the gate between "recorded a verdict" and "merged rows on it".
    enableErrorGroupReview :: Bool
  , -- Defaults OFF. The first deploy records verdicts and refutations without
    -- acting on them, so a day of them can be read before anything merges — the
    -- same rollout the endpoint review used.
    enableErrorGroupAutoApply :: Bool
  , -- Whether a merged group may also become a deterministic mask applied at
    -- ingest. Downstream of auto-apply: with nothing merging, nothing is
    -- promotable. Off until merges have been read.
    enableErrorMaskPromotion :: Bool
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
  , whatsappMonitorTemplate :: Maybe TwilioContentSid
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
  , enableServiceMapRollup :: Bool
  -- ^ Runs the 5-minute service-dependency rollup. Off by default: the rollup is the only
  -- place a span self-join runs against TimeFusion, which is the query shape that has
  -- OOM-killed it before, so it gets a staged rollout rather than shipping on.
  , enableTimefusionWrites :: Bool
  , enablePostgresTelemetryWrites :: Bool
  -- ^ Dual-write to the legacy Postgres @otel_logs_and_spans@ store. Defaults
  -- to True; set ENABLE_POSTGRES_TELEMETRY_WRITES=False once TimeFusion is the source of
  -- truth so ingestion writes TF only and PG is phased out.
  , kafkaDeadLetterTopic :: Text
  , rrwebDeadLetterTopic :: Text
  -- ^ Separate dead-letter topic for rrweb session-replay poison. rrweb is NOT
  -- an OTLP ce-type, so the DLQ-replay carousel can't re-process it (it would
  -- loop as "unsupported ce-type"); replay poison lands here and simply sits —
  -- no consumer, a quarantine for inspection. Kept off 'kafkaDeadLetterTopic'
  -- so real OTLP DLQ tracking isn't drowned in rrweb noise.
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
  , traceViewTimeoutSecs :: Int
  -- ^ Budget for the trace fetch behind a page that only *embeds* a trace (the
  -- issue detail page). A pathological trace — thousands of spans, cold TF
  -- read — has blown past the 60s gateway timeout and 504'd the whole page;
  -- past this budget the page renders without the trace pane instead.
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
  deriving stock (Generic)
  deriving anyclass (Default, FromEnv)


-- | Deliberately NOT derived. A third of these fields are secrets (@auth0Secret@,
-- @apiKeyEncryptionSecretKey@, @stripeSecretKey@, @smtpPassword@, @basicAuthPassword@,
-- every bot token…), and 'EnvConfig' is embedded in types that DO derive 'Show'
-- ('Pages.BodyWrapper.BWConfig'), so a single @show@ of a page config in a log line or an
-- exception message would print the deployment's entire credential set. Deriving cannot
-- express redaction, which is the one case that outranks the derive-everything rule; a
-- field-by-field allowlist would silently leak the next secret someone adds.
instance Show EnvConfig where
  showsPrec _ _ = showString "EnvConfig{redacted}"


instance DefConfig EnvConfig where
  defConfig =
    (def :: EnvConfig)
      { port = 8080
      , environment = Dev
      , migrationsDir = "./static/migrations/"
      , messagesPerPubsubPullBatch = 1000
      , rrwebTopics = ["rrweb-client"]
      , rrwebDeadLetterTopic = "rrweb_deadletter"
      , replayBatchSize = 0 -- 0 = derive at runtime as messagesPerPubsubPullBatch `div` 2
      , loggingDestination = Logging.StdOut
      , logLevel = LogInfo -- Default to Info level
      , smtpPort = 465
      , smtpTls = True
      , maxConcurrentJobs = 4 -- Sane default, can be increased based on CPU cores
      , showDemoProject = False -- Default to hidden
      , postmarkFromEmail = "hello@monoscope.tech"
      , openaiModel = "gpt-5.6-luna#high"
      , -- "minimal" is not a reasoning effort gpt-5.6-luna accepts; the API 400s
        -- on it, which silently killed every judge call (log- and error-pattern
        -- merge) with "LLM judge failed". "low" is the cheapest one it takes.
        enableEndpointGroupReview = True
      , enableEndpointGroupAutoApply = True
      , enableErrorGroupReview = True
      , enableErrorGroupAutoApply = False
      , enableErrorMaskPromotion = False
      , openaiSmallModel = "gpt-5.6-luna#low"
      , kafkaGroupConcurrency = 4
      , enableKafkaDeadLetterService = True
      , enableOtlpGrpcService = True
      , enablePostgresTelemetryWrites = True
      , extractionWorkerShards = 4
      , extractionQueueCapacity = 64
      , drainFlushBatchSize = 1000
      , drainFlushMaxAgeSecs = 60
      , drainRehydrateIntervalSecs = 300
      , maxBufferedSpans = 100000
      , maxDrainTrees = 200
      , traceViewTimeoutSecs = 20
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


-- | A validated Twilio Content Template SID: "HX" followed by 32 hexadecimal digits.
newtype TwilioContentSid = TwilioContentSid Text
  deriving stock (Eq, Generic, Show)


mkTwilioContentSid :: Text -> Maybe TwilioContentSid
mkTwilioContentSid sid =
  TwilioContentSid sid <$ guard (T.length sid == 34 && T.take 2 sid == "HX" && T.all isHexDigit (T.drop 2 sid))


twilioContentSidText :: TwilioContentSid -> Text
twilioContentSidText (TwilioContentSid sid) = sid


instance Var TwilioContentSid where
  fromVar = mkTwilioContentSid . toText
  toVar = toString . twilioContentSidText


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

-- | Everything that changes the api_catalog stats result: project, tab, sort, window,
-- period, page offset. A tuple rather than a joined string so a new dimension is a type
-- error at every construction site instead of a silently colliding cache key.
type HostStatsKey = (Projects.ProjectId, Text, Endpoints.EndpointSort, Endpoints.Since, Endpoints.Period, Int)


-- | Everything that changes the endpoints-list stats result: (project, direction tab,
-- host, sort) and (search, page, per_page, period). Nested pairs because Hashable
-- stops at 7-tuples. Same rationale as 'HostStatsKey'.
type EndpointStatsKey = ((Projects.ProjectId, Text, Text, Text), (Text, Int, Int, Endpoints.Period))


-- | Identifies a source blob: @(owner, repo, ref, path)@. Deliberately NOT project-scoped —
-- the same public repo linked by two projects is the same bytes, and the credential that
-- fetched it is not part of the file's identity. Access is still checked per request before
-- a lookup happens; this caches the fetch, never the authorization.
type CodeBlobKey = (Text, Text, Text, Text)


data AuthContext = AuthContext
  { env :: EnvConfig
  , pool :: Pool.Pool Connection
  , jobsPool :: Pool.Pool Connection
  , timefusionPgPool :: Pool.Pool Connection
  , hasqlPool :: OHasql.TracedPool
  , hasqlJobsPool :: OHasql.TracedPool
  , hasqlTimefusionPool :: OHasql.TracedPool
  , hasqlTimefusionUsesPgTypes :: Bool
  -- ^ Whether the "timefusion" pool actually points at a plain Postgres (tests),
  -- which — like the pg leg — needs id/JSON cast to native uuid/jsonb. Real
  -- TimeFusion rejects those OIDs and coerces bare text→Variant, so prod is False.
  -- This describes pool wiring, hence it lives here and not on env-decoded EnvConfig.
  , projectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , logsPatternCache :: Cache Projects.ProjectId (V.Vector Text)
  , hostStatsCache :: Cache HostStatsKey [Endpoints.HostEvents]
  -- ^ api_catalog per-host traffic stats. The underlying telemetry aggregate scans a
  -- full window of spans (tens of seconds), so tab and period toggles must not re-run
  -- it; a few minutes of staleness is invisible on a rolling 24h count.
  , endpointStatsCache :: Cache EndpointStatsKey (V.Vector Endpoints.EndpointRequestStats)
  -- ^ endpoints-list per-endpoint traffic stats; same deal as 'hostStatsCache'.
  , infrastructureCache :: Cache Containers.ContainerSnapshotKey (V.Vector Containers.ContainerRow)
  -- ^ One expensive metrics pivot feeds every infrastructure tab and detail drawer.
  , rumCache :: Cache RUM.RumCacheKey RUM.RumQueryResult
  -- ^ Briefly reuses RUM panel reads across tab navigation and preloaded requests.
  , codeBlobCache :: Cache CodeBlobKey ByteString
  -- ^ Source blobs for stack-trace code context, keyed @(owner, repo, ref, path)@. One git-host
  -- API call per frame opened otherwise, and a hot issue viewed repeatedly re-fetches every
  -- time — a rate-limit stall then presents as the panel silently not filling in. Only
  -- successful fetches are stored, so a 404 or a revoked token is never cached.
  , repoListCache :: Cache (DeriveUtils.UUIDId "github_credential") [Git.GitRepo]
  -- ^ The repository picker on the code-mappings settings page. Listing them mints an
  -- installation token and calls the git host, which put a 1.5s round trip in front of a
  -- settings page that renders in 0.2s otherwise. Keyed by credential, so re-granting or
  -- revoking one is not masked by another's entry, and only successes are stored.
  -- Spelled structurally rather than as @GitSync.GitHubCredentialId@: importing that module
  -- here closes a cycle through 'Models.Projects.ProjectApiKeys'.
  , projectKeyCache :: Cache Text (Maybe Projects.ProjectId)
  , extractionWorker :: ExtractionWorker.WorkerState Telemetry.OtelLogsAndSpans
  , traceSessionCache :: TraceSessionCache.TraceSessionCache
  , tfCircuit :: ExtractionWorker.CircuitBreaker
  , metricCatalogBuffer :: Telemetry.MetricCatalogBuffer
  , liveTail :: LiveTail.Runtime
  -- ^ Live Tail's subscription cache, local hub and emit callback. Assembled at startup
  -- because the emit callback depends on the chosen transport, which depends on config that
  -- is only complete here.
  , config :: EnvConfig
  , -- App-lifetime ki scope for fire-and-forget work that must outlive the request
    -- (Slack/Twilio handlers ACK fast, then process in the background). Nothing in
    -- non-server contexts (tests); see 'System.Tracing.forkBackground'.
    backgroundScope :: Maybe Ki.Scope
  }
  deriving stock (Generic)


-- ===============

-- | @Var@ derived via 'WrappedEnumSC' encodes to "prod"/"staging"/"dev" and
-- decodes case-insensitively (via @toPascal . fromSnake@), so existing
-- @ENVIRONMENT=DEV@/@PROD@/@STAGING@ deployments keep working.
data DeploymentEnv = Prod | Staging | Dev
  deriving stock (Eq, Generic, Read, Show)
  deriving (Var) via DeriveUtils.WrappedEnumSC 'Nothing "" DeploymentEnv


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
  -- 1800s idle, matching the hasql pools: at 30s a chart-heavy page reopened
  -- TF connections continuously, and connect() eventually returned
  -- EADDRNOTAVAIL ("Can't assign requested address") under widgetGetH. TF
  -- queries are slow and bursty, so recycling an idle connection every 30s
  -- costs a full TCP+startup round trip for no benefit.
  timefusionPgPool <- liftIO $ Pool.newPool (Pool.defaultPoolConfig createTimefusionPgConnIO PG.close 1800 10 & setNumStripes (Just 2))
  let mainHasqlSettings = DeriveUtils.addKeepaliveParams $ encodeUtf8 config.databaseUrl
      tfHasqlSettings = tfParams
  hasqlPool <- liftIO $ DeriveUtils.mkHasqlPool 20 mainHasqlSettings
  hasqlJobsPool <- liftIO $ DeriveUtils.mkHasqlPool 10 mainHasqlSettings
  hasqlTimefusionPool <- liftIO $ DeriveUtils.mkHasqlPool 30 tfHasqlSettings
  projectCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  projectKeyCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  logsPatternCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0)
  hostStatsCache <- liftIO $ newCache (Just $ TimeSpec 300 0)
  endpointStatsCache <- liftIO $ newCache (Just $ TimeSpec 300 0)
  infrastructureCache <- liftIO $ newCache (Just $ TimeSpec 15 0)
  -- 15s was shorter than the queries it caches: a RUM panel set takes ~28s over a 24h
  -- window, so every entry expired before the next request could reach it and the cache
  -- never hit. Matches the other telemetry stat caches above.
  rumCache <- liftIO $ newCache (Just $ TimeSpec 300 0)
  -- 15 min: a mutable ref (a branch name) must not pin a stale blob for long, and the value
  -- here is collapsing the burst of frames opened while reading ONE issue, not long-term
  -- storage. A commit-sha ref is immutable and would tolerate far longer, but the key cannot
  -- tell the two apart, so the shorter bound wins.
  codeBlobCache <- liftIO $ newCache (Just $ TimeSpec (15 * 60) 0)
  -- 10 min: the picker only has to be right by the time someone links a repo, and a repo
  -- added to the installation while the page is open is one reload away either way.
  repoListCache <- liftIO $ newCache (Just $ TimeSpec (10 * 60) 0)
  extractionWorker <- liftIO $ ExtractionWorker.initWorkerState config.extractionWorkerShards config.extractionQueueCapacity
  traceSessionCache <- liftIO TraceSessionCache.newTraceSessionCache
  tfCircuit <- liftIO ExtractionWorker.newCircuitBreaker
  metricCatalogBuffer <- liftIO Telemetry.newMetricCatalogBuffer
  -- The emit callback is the local hub by default. Server startup replaces it with the Kafka
  -- producer when the deployment is split; wiring it here means a context built outside the
  -- server (tests, CLI, jobs) still has a working single-process Live Tail rather than a
  -- partially-initialised field.
  liveTail <- liftIO do
    cache <- LiveTail.newSubCache
    hub <- LiveTail.newHub
    -- Brokers, not 'enableKafkaService': that flag differs between web and ingest pods in a
    -- split deployment, so reading it would have the two roles pick different transports.
    -- Everything else Live Tail needs is a constant in "Pkg.LiveTail" — none of it is a
    -- deployment decision, and an operator asked to pick a lease length has no way to answer.
    relayBuffer <- LiveTail.newRelayBuffer
    pure
      LiveTail.Runtime
        { transport = LiveTail.transportFor config.kafkaBrokers
        , cache
        , hub
        , relayBuffer
        , emit = LiveTail.deliver hub
        }
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
      , hasqlTimefusionUsesPgTypes = False -- prod TF is a real TimeFusion: bare text→Variant
      , env = config
      , projectCache
      , projectKeyCache
      , logsPatternCache
      , hostStatsCache
      , endpointStatsCache
      , infrastructureCache
      , rumCache
      , codeBlobCache
      , repoListCache
      , extractionWorker
      , traceSessionCache
      , tfCircuit
      , metricCatalogBuffer
      , liveTail
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


-- | Query 'information_schema.columns' for each queryable telemetry table once
-- at startup and seed its parser column cache (which splits the flattened
-- @___@ columns from the bare ones). Best-effort per table: any exception
-- (missing table during migration, lost pg conn) logs a warning and falls back
-- to the hand-coded builtin so the server still boots.
--
-- Both tables are seeded because KQL validates against whichever one the query
-- reads; before @otel_metrics@ had a column set of its own, metrics queries
-- skipped validation entirely and shipped spans-only fields to TimeFusion.
introspectAndCacheOtelColumns :: Pool.Pool Connection -> IO ()
introspectAndCacheOtelColumns pool =
  forM_ ([("otel_logs_and_spans", ParserExpr.setOtelColumns), ("otel_metrics", ParserExpr.setMetricsColumns)] :: [(Text, [Text] -> IO ())]) \(table, seed) -> do
    result <- Safe.try $ Pool.withResource pool $ \conn ->
      map PG.fromOnly
        <$> ( PG.query
                conn
                "SELECT column_name::text FROM information_schema.columns \
                \WHERE table_schema = 'public' AND table_name = ?"
                (PG.Only table)
                :: IO [PG.Only Text]
            )
    case result of
      Right cols -> seed cols
      Left (e :: SomeException) ->
        blueMessage $ "C1: " <> table <> " introspection failed, using builtin attribute set: " <> show e
