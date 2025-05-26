{-# LANGUAGE FlexibleContexts #-}

module System.Config (EnvConfig (..), AuthContext (..), DashboardM, ctxToHandler, getAppContext, configToEnv, DeploymentEnv (..)) where

import Colourista.IO (blueMessage)
import Data.Cache (Cache, newCache)
import Data.Default (Default (..))
import Data.Default.Instances ()
import Data.Pool as Pool (Pool, defaultPoolConfig, newPool)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Migration qualified as Migrations
import Effectful
import Effectful.Fail (Fail)
import Models.Projects.Projects qualified as Projects
import Pkg.DBUtils qualified as DBUtils
import Relude
import Servant.Server (Handler)
import System.Clock (TimeSpec (TimeSpec))
import System.Envy (FromEnv (..), ReadShowVar (..), Var (..), decodeEnv, fromVar, toVar)
import System.Logging qualified as Logging


data EnvConfig = EnvConfig
  { databaseUrl :: Text -- "DATABASE_URL"
  , timefusionPgUrl :: Text -- TIMEFUSION_PG_URL
  , port :: Int
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
  , otlpStreamTopics :: [Text]
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
  , courierClientKey :: Text
  , courierApiKey :: Text
  , environment :: Text
  , loggingDestination :: Logging.LoggingDestination
  , lemonSqueezyApiKey :: Text
  , lemonSqueezyUrl :: Text
  , lemonSqueezyCriticalUrl :: Text
  , postmarkToken :: Text
  , lemonSqueezyWebhookSecret :: Text
  , openaiApiKey :: Text
  , openaiBaseUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromEnv)


-- Support unmarshalling a coma separated text into a text list
instance Var [Text] where
  fromVar = Just . T.splitOn "," . toText
  toVar = toString . T.intercalate ","


-- Rename to AppContext
data AuthContext = AuthContext
  { env :: EnvConfig
  , pool :: Pool.Pool Connection
  , jobsPool :: Pool.Pool Connection
  , timefusionPgPool :: Pool.Pool Connection
  , projectCache :: Cache Projects.ProjectId Projects.ProjectCache
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
  pool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig createPgConnIO PG.close (60 * 2) 100
  jobsPool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig createPgConnIO PG.close (60 * 2) 50
  timefusionPgPool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig createTimefusionPgConnIO PG.close (60 * 2) 50
  projectCache <- liftIO $ newCache (Just $ TimeSpec (60 * 60) 0) -- :: m (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
  projectKeyCache <- liftIO $ newCache Nothing
  pure
    AuthContext
      { pool
      , jobsPool
      , timefusionPgPool
      , env = config
      , projectCache
      , projectKeyCache
      , config
      }


getAppContext :: Eff '[Fail, IOE] AuthContext
getAppContext = do
  configE <- liftIO (decodeEnv :: IO (Either String EnvConfig))
  case configE of
    Left errMsg -> error $ toText errMsg
    Right config -> configToEnv config
