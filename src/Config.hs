{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Config where

import Data.Default
import Data.Pool as Pool
import qualified Data.UUID as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import qualified Models.Users.Sessions as Sessions
import Network.Wai (Request, requestHeaders)
import Optics.TH
import Relude
import Servant (Header, Headers, ServerError (errBody, errHeaders), err301, err401, throwError)
import Servant.Server (Handler)
import Servant.Server.Experimental.Auth
import System.Envy (FromEnv)
import Web.Cookie
import Prelude (lookup)

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
    apiKeyEncryptionSecretKey :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromEnv)

makeFieldLabelsNoPrefix ''EnvConfig

data AuthContext = AuthContext
  { env :: EnvConfig,
    pool :: Pool.Pool Connection
  }
  deriving stock (Show, Generic)

type DashboardM = ReaderT AuthContext Handler

-- | HeadersTriggerRedirect is a type alias for th hx-trigger header and the hx-redirect.
-- These headers will be primarily used on form submissions.
type HeadersTriggerRedirect = Headers '[Header "HX-Trigger" String, Header "HX-Redirect" String]

type HeadersTrigger = Headers '[Header "HX-Trigger" String]

ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s
