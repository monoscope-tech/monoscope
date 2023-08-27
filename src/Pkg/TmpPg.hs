module Pkg.TmpPg (withSetup) where

import Relude
import Database.Postgres.Temp qualified as TmpPostgres
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple 
import Database.PostgreSQL.Simple.Migration qualified as Migration
import Database.PostgreSQL.Simple.Migration (MigrationCommand(MigrationInitialization, MigrationDirectory))
import Control.Exception (throwIO)
import Database.Postgres.Temp (withDbCache, cacheConfig, cacheAction, withConfig, toConnectionString)
import System.Directory (listDirectory, getFileSize)
import Database.PostgreSQL.Simple.SqlQQ (sql)

migrationsDir :: FilePath
migrationsDir = "./static/migrations/" 

migrate :: TmpPostgres.DB -> IO ()
migrate db = do
  conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
  initializationRes <- Migration.runMigration conn Migration.defaultOptions MigrationInitialization
  migrationRes <- Migration.runMigration conn Migration.defaultOptions $ MigrationDirectory (migrationsDir)
  -- Create a nil user and projects to make subsequent tests easier 
  let q = 
        [sql| INSERT into users.users (id, first_name, last_name, email) VALUES ('00000000-0000-0000-0000-000000000000', 'FN', 'LN', 'test@apitoolkit.io');
              INSERT into projects.projects (id, title) VALUES ('00000000-0000-0000-0000-000000000000', 'test-title')
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
    let combinedConfig = cacheConfig dbCache -- <> TmpPostgres.verboseConfig 
            <> mempty { TmpPostgres.postgresConfigFile = [("shared_preload_libraries", "'timescaledb'")]
                      }
    dirSize <- sum <$> (listDirectory migrationsDir >>= mapM (getFileSize . (migrationsDir <>)))
    migratedConfig <- throwE $ cacheAction ("./.tmp/postgres/" <> show dirSize) migrate combinedConfig
    withConfig migratedConfig $ \db ->
      f =<< createPool (connectPostgreSQL $ toConnectionString db) close 2 60 10


