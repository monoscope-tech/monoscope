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
    testEmail :: Maybe Text
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

ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s

--- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: Pool Connection -> AuthHandler Request Sessions.PersistentSession
authHandler conn = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw301 _ = throwError $ err301 {errHeaders = [("Location", "/login")]}

    handler :: Request -> Handler Sessions.PersistentSession
    handler req = either throw301 (lookupAccount conn) $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "apitoolkit_session" $ parseCookies cookie

-- We need to handle errors for the persistent session better and redirect if there's an error
lookupAccount :: Pool Connection -> ByteString -> Handler Sessions.PersistentSession
lookupAccount conn key = do
  resp <- runMaybeT $ do
    pid <- hoistMaybe (UUID.fromASCIIBytes key)
    presistentID <- liftIO $ withPool conn $ Sessions.getPersistentSession $ Sessions.PersistentSessionId pid
    hoistMaybe presistentID
  case resp of
    Nothing -> do
      liftIO $ putStrLn $ "Auth: Unable to unmarshal auth cookie value into PersistentSessionId. Value: " <> show key
      throwError $ err301 {errHeaders = [("Location", "/login")]}
    Just session -> pure session
