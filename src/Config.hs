{-# LANGUAGE TemplateHaskell #-}

module Config (EnvConfig (..), AuthContext (..), DashboardM, ctxToHandler) where

import Colog (LogAction)
import Data.Cache (Cache)
import Data.Pool as Pool
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Models.Projects.Projects qualified as Projects
import Optics.TH
import Relude
import Servant.Server (Handler)
import System.Envy (FromEnv, Var, fromVar, toVar)

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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromEnv)

-- Support unmarshalling a coma separated text into a text list
instance Var [Text] where
  fromVar = Just . T.splitOn "," . toText
  toVar = toString . T.intercalate ","

makeFieldLabelsNoPrefix ''EnvConfig

data AuthContext = AuthContext
  { env :: EnvConfig
  , pool :: Pool.Pool Connection
  , logger :: LogAction IO String
  , projectCache :: Cache Projects.ProjectId Projects.ProjectCache
  }

type DashboardM = ReaderT AuthContext Handler

ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s
