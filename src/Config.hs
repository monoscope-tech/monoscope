{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Config where

import Relude

import System.Envy (FromEnv)
import Servant.Server (Handler)
import Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection)
import Servant (Headers, Header)

data EnvConfig = EnvConfig
  { databaseUrl :: String, -- "DATABASE_URL"
    port :: Int,
    migrationsDir :: String -- "MIGRATIONS_DIR"
  }
  deriving (Show, Generic)
  deriving anyclass (FromEnv)


data AuthContext = AuthContext
  { env :: EnvConfig,
    pool :: Pool.Pool Connection
  } deriving stock (Show, Generic)


type DashboardM = ReaderT AuthContext Handler

-- | HeadersTriggerRedirect is a type alias for th hx-trigger header and the hx-redirect. 
-- These headers will be primarily used on form submissions.
type HeadersTriggerRedirect = Headers '[Header "HX-Trigger" String, Header "HX-Redirect" String]

ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s
