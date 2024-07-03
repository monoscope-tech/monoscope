module Pkg.TestUtils (
  withSetup,
  abort,
  rollback,
  withTestResources,
  fromRightShow,
  TestResources (..),
  testSessionHeader,
) where

import Data.Default (Default (..))
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Time (runTime)

import Control.Exception (bracket_, finally, mask, throwIO)
import Data.Cache (Cache (..), newCache)
import Data.Effectful.UUID (runStaticUUID)
import Data.Effectful.Wreq (runHTTPGolden)
import Data.Either.Extra
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute, execute_)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization))
import Database.PostgreSQL.Simple.Migration qualified as Migration
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Transaction (newSavepoint, rollbackToAndReleaseSavepoint)
import Database.Postgres.Temp (cacheAction, cacheConfig, toConnectionString, withConfig, withDbCache)
import Database.Postgres.Temp qualified as TmpPostgres
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Log qualified
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Relude
import Servant qualified
import System.Clock (TimeSpec (TimeSpec))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Directory (getFileSize, listDirectory)
import Web.Auth qualified as Auth

import Web.Cookie (SetCookie)


migrationsDirr :: FilePath
migrationsDirr = "./static/migrations/"


migrate :: TmpPostgres.DB -> IO ()
migrate db = do
  conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
  initializationRes <- Migration.runMigration conn Migration.defaultOptions MigrationInitialization

  migrationRes <- Migration.runMigration conn Migration.defaultOptions $ MigrationDirectory migrationsDirr
  -- Create a nil user and projects to make subsequent tests easier
  let q =
        [sql| INSERT into users.users (id, first_name, last_name, email) VALUES ('00000000-0000-0000-0000-000000000000', 'FN', 'LN', 'test@apitoolkit.io');
        |]
  _ <- execute conn q ()
  pass


-- Setup function that spins up a database with the db migrations already executed.
-- source: https://jfischoff.github.io/blog/keeping-database-tests-fast.html
withSetup :: (Pool Connection -> IO ()) -> IO ()
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x
  throwE $ withDbCache $ \dbCache -> do
    let combinedConfig =
          cacheConfig dbCache -- <> TmpPostgres.verboseConfig
            <> mempty
              { TmpPostgres.postgresConfigFile = [("shared_preload_libraries", "'timescaledb'")]
              }
    dirSize <- sum <$> (listDirectory migrationsDirr >>= mapM (getFileSize . (migrationsDirr <>)))
    migratedConfig <- throwE $ cacheAction ("./.tmp/postgres/" <> show dirSize) migrate combinedConfig
    withConfig migratedConfig $ \db ->
      f =<< newPool (defaultPoolConfig (connectPostgreSQL $ toConnectionString db) close 60 10)


-- throw away all db changes that happened within this abort block
abort :: (Connection -> IO a) -> Connection -> IO a
abort f conn =
  bracket_
    (execute_ conn "BEGIN")
    (execute_ conn "ROLLBACK")
    (f conn)


-- can be a nested transaction, and creates a savepoint which then gets auto rolled back,
-- while reusing all the setup and db operations that happened up until that savepoint
rollback :: Connection -> IO a -> IO a
rollback conn actionToRollback = mask $ \restore -> do
  sp <- newSavepoint conn
  restore actionToRollback `finally` rollbackToAndReleaseSavepoint conn sp


-- | `testSessionHeader` would log a user in and automatically generate a session header
-- which can be reused in subsequent tests
testSessionHeader :: MonadIO m => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session)
testSessionHeader pool = do
  pSessId <-
    Auth.authorizeUserAndPersist Nothing "firstName" "lastName" "https://placehold.it/500x500" "test@apitoolkit.io"
      & (runStaticUUID $ map (UUID.fromWords 0 0 0) [1 .. 10])
      & runHTTPGolden "./golden/"
      & DB.runDB pool
      & runTime
      & runEff
      & liftIO
  Auth.sessionByID (Just pSessId) "requestID" False
    & runErrorNoCallStack @Servant.ServerError
    & DB.runDB pool
    & runEff
    & liftIO
    <&> fromRightShow


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


-- New type to hold all our resources
data TestResources = TestResources
  { trPool :: Pool Connection
  , trProjectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , trSessAndHeader :: Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  , trATCtx :: AuthContext
  , trLogger :: Log.Logger
  }


-- Compose withSetup with additional IO actions
withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources f = withSetup $ \pool -> LogBulk.withBulkStdOutLogger \logger -> do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  sessAndHeader <- testSessionHeader pool
  let atAuthCtx =
        AuthContext (def @EnvConfig) pool pool projectCache
          $ ( (def :: EnvConfig)
                { apiKeyEncryptionSecretKey = "apitoolkit123456123456apitoolkit"
                , convertkitApiKey = ""
                , convertkitApiSecret = ""
                }
            )
  f
    TestResources
      { trPool = pool
      , trProjectCache = projectCache
      , trSessAndHeader = sessAndHeader
      , trATCtx = atAuthCtx
      , trLogger = logger
      }
