{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Config where

import Data.Default
import Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection)
import qualified Models.Users.Sessions as Sessions
import Network.Wai (Request, requestHeaders)
import Relude
import Servant (Header, Headers, ServerError (errBody), err401, throwError)
import Servant.Server (Handler)
import Servant.Server.Experimental.Auth
import System.Envy (FromEnv)
import Web.Cookie
import Prelude (lookup)

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
  }
  deriving stock (Show, Generic)

type DashboardM = ReaderT AuthContext Handler

-- | HeadersTriggerRedirect is a type alias for th hx-trigger header and the hx-redirect.
-- These headers will be primarily used on form submissions.
type HeadersTriggerRedirect = Headers '[Header "HX-Trigger" String, Header "HX-Redirect" String]

ctxToHandler :: AuthContext -> DashboardM a -> Handler a
ctxToHandler s x = runReaderT x s

-- type FloraAuthContext = AuthHandler Request Sessions.PersistentSession


--- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: AuthHandler Request Sessions.PersistentSession
authHandler = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = either throw401 lookupAccount $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie

lookupAccount :: ByteString -> Handler Sessions.PersistentSession
lookupAccount key = pure $ (def @Sessions.PersistentSession)
