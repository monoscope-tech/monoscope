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

data EnvConfig = EnvConfig
  { databaseUrl :: String, -- "DATABASE_URL"
    port :: Int,
    migrationsDir :: String -- "MIGRATIONS_DIR"
  }
  deriving (Show, Generic)
  deriving anyclass (FromEnv)
