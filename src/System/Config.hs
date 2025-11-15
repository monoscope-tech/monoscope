{-# LANGUAGE FlexibleContexts #-}

module System.Config (EnvConfig (..), AuthContext (..), DashboardM, ctxToHandler, getAppContext, configToEnv, DeploymentEnv (..)) where

import Colourista.IO (blueMessage)
import Data.Cache (Cache, newCache)
import Data.Default (Default (..))
import Data.Default.Instances ()
import Data.Pool as Pool (Pool, defaultPoolConfig, newPool, setNumStripes)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Migration qualified as Migrations
import Effectful
import Effectful.Fail (Fail)
import Log (LogLevel (..))
import Models.Projects.Projects qualified as Projects
import Pkg.DBUtils qualified as DBUtils
import Relude
import Servant.Server (Handler)
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
  , courierClientKey :: Text
  , courierApiKey :: Text
  , environment :: Text
  , loggingDestination :: Logging.LoggingDestination
  , logLevel :: LogLevel
  , lemonSqueezyApiKey :: Text
  , lemonSqueezyUrl :: Text
  , lemonSqueezyCriticalUrl :: Text
  , postmarkToken :: Text
  , lemonSqueezyWebhookSecret :: Text
  , openaiApiKey :: Text
  , openaiBaseUrl :: Text
  , chartShotUrl :: Text
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
  , telemetryProjectId :: Text
  , telemetryServiceName :: Text
  , enableEventsTableUpdates :: Bool
  , enableDailyJobScheduling :: Bool
  , maxConcurrentJobs :: Int
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
      , maxConcurrentJobs = 4 -- Sane default, can be increased based on CPU cores
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
  , projectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , logsPatternCache :: Cache Projects.ProjectId (V.Vector Text)
  , projectKeyCache :: Cache Text (Maybe Projects.ProjectId)
  , config :: EnvConfig
  }


-- TODO: remove
type DashboardM = ReaderT AuthContext Handler


-- TODO: remove
ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s


-- ===============

data DeploymentEnv = Prod | Staging
  deriving stock (Generic, Read, Show)
  deriving (Var) via (ReadShowVar DeploymentEnv)


instance Default DeploymentEnv where
  def = Staging


configToEnv :: IOE :> es => EnvConfig -> Eff es AuthContext
configToEnv config = do
  let createPgConnIO = PG.connectPostgreSQL $ encodeUtf8 config.databaseUrl
  let createTimefusionPgConnIO = DBUtils.connectPostgreSQL $ encodeUtf8 config.timefusionPgUrl
  when config.migrateAndInitializeOnStart $ liftIO do
    conn <- createPgConnIO
    initializationRes <- Migrations.runMigration conn Migrations.defaultOptions Migrations.MigrationInitialization
    blueMessage ("migration initialized " <> show initializationRes)
    migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ Migrations.MigrationDirectory (toString config.migrationsDir :: FilePath)
    blueMessage ("migration result " <> show migrationRes)
    pass
  pool <- liftIO $ Pool.newPool $ (Pool.defaultPoolConfig createPgConnIO PG.close (60 * 2) 50 & setNumStripes (Just 5))
  jobsPool <- liftIO $ Pool.newPool $ (Pool.defaultPoolConfig createPgConnIO PG.close (60 * 2) 50 & setNumStripes (Just 4))
  timefusionPgPool <- liftIO $ Pool.newPool $ (Pool.defaultPoolConfig createTimefusionPgConnIO PG.close (60 * 2) 5 & setNumStripes (Just 4))
  projectCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0) -- :: m (Cache Projects.ProjectId Projects.ProjectCache) -- 30*60secs or 30 minutes TTL
  projectKeyCache <- liftIO $ newCache Nothing
  logsPatternCache <- liftIO $ newCache (Just $ TimeSpec (30 * 60) 0) -- Cache for log patterns, 30 minutes TTL
  pure
    AuthContext
      { pool
      , jobsPool
      , timefusionPgPool
      , env = config
      , projectCache
      , projectKeyCache
      , logsPatternCache
      , config
      }


getAppContext :: Eff '[Fail, IOE] AuthContext
getAppContext = do
  config <- liftIO (decodeWithDefaults defConfig)
  configToEnv config
