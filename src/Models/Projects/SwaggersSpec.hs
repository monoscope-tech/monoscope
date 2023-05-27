{-# LANGUAGE OverloadedStrings #-}

module Models.Projects.SwaggersSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Time.LocalTime (getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Transaction qualified as DBT
import Database.PostgreSQL.Transact qualified as DBT
import Models.Projects.Projects (ProjectId (..))
import Models.Projects.Swaggers
import Models.Users.Users (UserId (..))
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck
import Database.Postgres.Temp qualified as TmpPostgres
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close)
import Data.Pool (Pool, createPool)
import Control.Exception (throwIO)
import Database.Postgres.Temp (withDbCache, defaultConfig, cacheConfig, cacheAction, withConfig, toConnectionString, withSnapshot, DirectoryType (Temporary), snapshotConfig)
import Database.PostgreSQL.Simple.Migration qualified as Migrations
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Entity.DBT qualified as DBT

-- Helper function to run DBT actions within tests
-- runDBT :: TmpPostgres.DB -> DBT.DBT IO a -> IO a
-- runDBT db action = do
--   let connSettings = TmpPostgres.toConnectionString db
--   conn <- PG.connectPostgreSQL connSettings
--   result <- DBT.runDBT action DBT.DefaultIsolationLevel conn
--   PG.close conn
--   pure result

-- Helper function to create a Swagger value for testing
createSwagger :: Pool Connection ->  ProjectId -> UserId -> Value -> IO Swagger
createSwagger pool projectId createdBy swaggerJson = do
  currentTime <- liftIO getZonedTime
  randUUID <- liftIO UUIDV4.nextRandom
  let swagger =
        Swagger
          { Models.Projects.Swaggers.id = SwaggerId  randUUID
          , projectId = projectId
          , createdBy = createdBy
          , createdAt = currentTime
          , updatedAt = currentTime
          , swaggerJson = swaggerJson
          }
  DBT.withPool pool $ addSwagger swagger
  pure swagger

-- resetDatabase :: IO ()
-- resetDatabase = do
--   -- Establish a connection to the database
--   let connSettings = TmpPostgres.toConnectionString db
--   conn <- PG.connectPostgreSQL connSettings

--   -- Execute necessary SQL statements to reset the database tables
--   PG.execute_ conn "TRUNCATE TABLE apis.swagger_jsons RESTART IDENTITY CASCADE"

--   -- Close the database connection
--   PG.close conn

-- Setup function that spins up a database with the db migrations already executed.
-- source: https://jfischoff.github.io/blog/keeping-database-tests-fast.html
withSetup :: (Pool Connection -> IO ()) -> IO ()
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ withDbCache $ \dbCache -> withConfig (cacheConfig dbCache) $ \db -> do
    conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
    migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ MigrationDirectory ("static/migrations" :: FilePath)
    putStrLn $ "migration result: " <> show migrationRes
    withSnapshot db $ \snapshot -> do
      withConfig (snapshotConfig snapshot) $ \migratedDb -> f =<< createPool (connectPostgreSQL $ toConnectionString migratedDb) close 2 60 10
    pass


-- migrate :: TmpPostgres.DB -> IO ()
-- migrate db = do
--     initializationRes <- Migrations.runMigration conn Migrations.defaultOptions MigrationInitialization
--     putStrLn $ "migration initialized " <> show initializationRes
--     migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ MigrationDirectory ((toString $ envConfig ^. #migrationsDir) :: FilePath)
--     putStrLn $ "migration result: " <> show migrationRes

spec :: Spec
-- spec = beforeAll_ resetDatabase $ describe "Models.Projects.Swaggers" $ do
spec = aroundAll withSetup $ describe "Models.Projects.Swaggers" $ do
  -- describe "addSwagger" $
  --   it "should insert a new Swagger into the database" $ \pool -> do
  --     projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
  --     createdBy <- UserId <$> liftIO UUIDV4.nextRandom
  --     swaggerId <- SwaggerId <$> liftIO UUIDV4.nextRandom
  --     let swaggerJson = object ["info" .= object ["title" .= "API"]]
  --     let swagger =
  --           Swagger
  --             { Models.Projects.Swaggers.id = swaggerId
  --             , projectId = projectId
  --             , createdBy = createdBy
  --             , createdAt = undefined
  --             , updatedAt = undefined
  --             , swaggerJson = swaggerJson
  --             }
  --     addSwagger pool swagger
  --     result <- DBT.runDBT pool $ getSwaggerById (toText swaggerId)
  --     result `shouldBe` Just swagger

  describe "getSwaggerById" $
    it "should retrieve a Swagger by its ID" $ \pool -> do
      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      let swaggerJson' = object ["info" .= object ["title" .= ("API" :: String)]]
      swagger <- liftIO $ createSwagger pool projectId createdBy swaggerJson'
      result <- DBT.withPool pool $ getSwaggerById ( swagger.id.toText )
      let (Just r) = result
      (r.swaggerJson) `shouldBe` swagger.swaggerJson

  -- describe "swaggersByProject" $
  --   it "should retrieve all Swaggers for a given Project" $ do
  --     projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
  --     createdBy <- UserId <$> liftIO UUIDV4.nextRandom
  --     let swaggerJson1 = object ["info" .= object ["title" .= "API 1"]]
  --     let swaggerJson2 = object ["info" .= object ["title" .= "API 2"]]
  --     swagger1 <- liftIO $ createSwagger projectId createdBy swaggerJson1
  --     swagger2 <- liftIO $ createSwagger projectId createdBy swaggerJson2
  --     result <- runDBT $ swaggersByProject projectId
  --     result `shouldBe` [swagger1, swagger2]

  -- describe "updateSwagger" $
  --   prop "should update the Swagger JSON of a Swagger" $ \(swaggerJson1, swaggerJson2) -> do
  --     projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
  --     createdBy <- UserId <$> liftIO UUIDV4.nextRandom
  --     swagger <- liftIO $ createSwagger projectId createdBy swaggerJson1
  --     _ <- runDBT $ updateSwagger (toText $ Models.Projects.Swaggers.id swagger) swaggerJson2
  --     result <- runDBT $ getSwaggerById (toText $ Models.Projects.Swaggers.id swagger)
  --     fmap Models.Projects.Swaggers.swaggerJson result `shouldBe` Just swaggerJson2
