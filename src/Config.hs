{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Config
  ( EnvConfig (..),
  )
where
import Relude

import System.Envy (FromEnv)
import Servant.Server (Handler)

data EnvConfig = EnvConfig
  { databaseUrl :: String, -- "DATABASE_URL"
    port :: Int,
    migrationsDir :: String -- "MIGRATIONS_DIR"
  }
  deriving (Show, Generic)
  deriving anyclass (FromEnv)


data AuthContext = AuthedUser
  { -- userInfo :: User
  env :: EnvConfig 
  } deriving stock (Show, Generic)


type DashboardM = ReaderT AuthContext Handler
