{-# LANGUAGE TemplateHaskell #-}

module Config (EnvConfig (..), AuthContext (AuthContext), DashboardM, HeadersTriggerRedirect, HeadersTrigger, ctxToHandler) where

import Colog (LogAction)
import Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection)
import Optics.TH
import Relude
import Servant (Header, Headers)
import Servant.Server (Handler)
import System.Envy (FromEnv)

data EnvConfig = EnvConfig
  { databaseUrl :: Text, -- "DATABASE_URL"
    port :: Int,
    migrationsDir :: Text, -- "MIGRATIONS_DIR"
    auth0ClientId :: Text,
    auth0Secret :: Text,
    auth0Domain :: Text,
    auth0LogoutRedirect :: Text,
    auth0Callback :: Text,
    testEmail :: Maybe Text,
    apiKeyEncryptionSecretKey :: Text,
    messagesPerPubsubPullBatch :: Int
  }
  deriving (Show, Generic)
  deriving anyclass (FromEnv)

makeFieldLabelsNoPrefix ''EnvConfig

data AuthContext = AuthContext
  { env :: EnvConfig,
    pool :: Pool.Pool Connection,
    logger :: LogAction IO String
  }

type DashboardM = ReaderT AuthContext Handler

-- | HeadersTriggerRedirect is a type alias for th hx-trigger header and the hx-redirect.
-- These headers will be primarily used on form submissions.
type HeadersTriggerRedirect = Headers '[Header "HX-Trigger" String, Header "HX-Redirect" String]

type HeadersTrigger = Headers '[Header "HX-Trigger" String]

ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s
