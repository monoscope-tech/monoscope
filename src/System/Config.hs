{-# LANGUAGE FlexibleContexts #-}

module System.Config (EnvConfig (..), AuthContext (..), DashboardM, ctxToHandler, getAppContext, configToEnv, DeploymentEnv (..)) where

import Colourista.IO (blueMessage)
import Configuration.Dotenv qualified as Dotenv
import Control.Exception (try)
import Data.Cache (Cache, newCache)
import Data.Default (Default (..))
import Data.Default.Instances ()
import Data.Pool as Pool (Pool, defaultPoolConfig, newPool)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Migration qualified as Migrations
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Fail (Fail)
import Models.Projects.Projects qualified as Projects
import Relude (
  Applicative (pure),
  Bool,
  ConvertUtf8 (encodeUtf8),
  Either (..),
  FilePath,
  Generic,
  IO,
  Int,
  Maybe (Just),
  Num ((*)),
  Read,
  ReaderT (runReaderT),
  Semigroup ((<>)),
  Show,
  SomeException,
  String,
  Text,
  ToString (toString),
  ToText (toText),
  error,
  pass,
  show,
  when,
  ($),
  (.),
 )
import Servant.Server (Handler)
import System.Clock (TimeSpec (TimeSpec))
import System.Envy (FromEnv (..), ReadShowVar (..), Var (..), decodeEnv, fromVar, toVar)
import System.Logging qualified as Logging


data EnvConfig = EnvConfig
  { databaseUrl :: Text -- "DATABASE_URL"
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
  , smtpHost :: Text
  , smtpPort :: Int
  , smtpUsername :: Text
  , smtpPassword :: Text
  , smtpSender :: Text
  , paddleSandbox :: Bool
  , paddleSandboxVendorId :: Text
  , paddleSandboxApiKey :: Text
  , paddleVendorId :: Text
  , paddleApiKey :: Text
  , paddleSandboxStartup :: Text
  , paddleStartup :: Text
  , paddleSandboxGrowth :: Text
  , paddleGrowth :: Text
  , paddleSandboxHobby :: Text
  , paddleHobby :: Text
  , orttoApiKey :: Text
  , googleServiceAccountB64 :: LT.Text
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
  , enablePubsubService :: Bool
  , lemonSqueezyApiKey :: Text
  , lemonSqueezyUrl :: Text
  , postmarkToken :: Text
  , lemonSqueezyGraduatedUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromEnv, Default)


-- Support unmarshalling a coma separated text into a text list
instance Var [Text] where
  fromVar = Just . T.splitOn "," . toText
  toVar = toString . T.intercalate ","


-- Rename to AppContext
data AuthContext = AuthContext
  { env :: EnvConfig
  , pool :: Pool.Pool Connection
  , jobsPool :: Pool.Pool Connection
  , projectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , config :: EnvConfig
  }


-- TODO: remove
type DashboardM = ReaderT AuthContext Handler


-- TODO: remove
ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s


-- ===============

data DeploymentEnv = Prod | Staging
  deriving stock (Read, Show, Generic)
  deriving (Var) via (ReadShowVar DeploymentEnv)


instance Default DeploymentEnv where
  def = Staging


configToEnv :: IOE :> es => EnvConfig -> Eff es AuthContext
configToEnv config = do
  let createPgConnIO = PG.connectPostgreSQL $ encodeUtf8 config.databaseUrl
  when config.migrateAndInitializeOnStart $ liftIO do
    conn <- createPgConnIO
    initializationRes <- Migrations.runMigration conn Migrations.defaultOptions Migrations.MigrationInitialization
    blueMessage ("migration initialized " <> show initializationRes)
    migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ Migrations.MigrationDirectory (toString config.migrationsDir :: FilePath)
    blueMessage ("migration result " <> show migrationRes)
    pass
  pool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig createPgConnIO PG.close (60 * 6) 100
  jobsPool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig createPgConnIO PG.close (60 * 6) 150
  projectCache <- liftIO $ newCache (Just $ TimeSpec (60 * 60) 0) -- :: m (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
  pure
    AuthContext
      { pool = pool
      , jobsPool = jobsPool
      , env = config
      , projectCache
      , config
      }


getAppContext :: Eff '[Fail, IOE] AuthContext
getAppContext = do
  _ <- liftIO (try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ()))
  configE <- liftIO (decodeEnv :: IO (Either String EnvConfig))
  case configE of
    Left errMsg -> error $ toText errMsg
    Right config -> do
      configToEnv config
