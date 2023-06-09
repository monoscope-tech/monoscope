{-# LANGUAGE OverloadedStrings #-}

module Models.Projects.SwaggersSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Time.LocalTime (getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Simple qualified as PG
import Optics.Core ((^.))
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
import Debug.Pretty.Simple (pTraceShowM)
import Data.Maybe
import System.Directory

-- Helper function to create a Swagger value for testing
createSwagger ::  ProjectId -> UserId -> Value -> DBT.DBT IO Swagger
createSwagger projectId createdBy swaggerJson = do
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
  addSwagger swagger
  pure swagger

-- Setup function that spins up a database with the db migrations already executed.
-- source: https://jfischoff.github.io/blog/keeping-database-tests-fast.html
withSetup :: (Pool Connection -> IO ()) -> IO ()
withSetup f = do
  putStrLn "In setup 🔥 bb"
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ withDbCache $ \dbCache -> traceShowM "In with dbcache 🔥" >> withConfig (cacheConfig dbCache) $ \db -> do
    pt <- liftIO $ getCurrentDirectory 
    putStrLn "In with config 🔥"
    putStrLn $ show pt
    conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
    initializationRes <- Migrations.runMigration conn Migrations.defaultOptions MigrationInitialization
    putStrLn $ show initializationRes
    putStrLn $ "after conn"  
    -- putStrLn $ TmpPostgres.toConnectionString db
    migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ MigrationDirectory ("./static/migrations" :: FilePath)
    putStrLn $ "migration result: " <> show migrationRes
    x <- withSnapshot db $ \snapshot -> do
      withConfig (snapshotConfig snapshot) $ \migratedDb -> f =<< createPool (connectPostgreSQL $ toConnectionString migratedDb) close 2 60 10
    pTraceShowM x 
    pass


spec :: Spec
-- spec = beforeAll_ resetDatabase $ describe "Models.Projects.Swaggers" $ do
spec = aroundAll withSetup $ describe "Models.Projects.Swaggers" $ do
  describe "addSwagger" $
    it "should insert a new Swagger into the database" $ \pool -> do
      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      swaggerId <- SwaggerId <$> liftIO UUIDV4.nextRandom
      let swaggerJson = object ["info" .= object ["title" .= "API"]]
      let swagger =
            Swagger
              { Models.Projects.Swaggers.id = swaggerId
              , projectId = projectId
              , createdBy = createdBy
              , createdAt = undefined
              , updatedAt = undefined
              , swaggerJson = swaggerJson
              }
      
      result <- DBT.withPool pool $ do
        addSwagger swagger
        getSwaggerById (swaggerId.toText)
      (fromJust result).swaggerJson `shouldBe` swaggerJson 

  describe "getSwaggerById" $
    it "should retrieve a Swagger by its ID" $ \pool -> do
      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      let swaggerJson' = object ["info" .= object ["title" .= ("API" :: String)]]
      swagger <- DBT.withPool pool $ createSwagger projectId createdBy swaggerJson'
      result <- DBT.withPool pool $ getSwaggerById ( swagger.id.toText )
      (fromJust result).swaggerJson `shouldBe` swagger.swaggerJson

  describe "swaggersByProject" $
    it "should retrieve all Swaggers for a given Project" $ \pool -> do
      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      let swaggerJson1 = object ["info" .= object ["title" .= "API 1"]]
      let swaggerJson2 = object ["info" .= object ["title" .= "API 2"]]
      result <- DBT.withPool pool $ do
        createSwagger projectId createdBy swaggerJson1
        createSwagger projectId createdBy swaggerJson2
        swaggersByProject projectId
      (map (^. #swaggerJson) (toList result)) `shouldBe` [swaggerJson1, swaggerJson2]

  -- describe "updateSwagger" $
  --   prop "should update the Swagger JSON of a Swagger" $ \pool -> \(swaggerJson1, swaggerJson2) -> do
  --     projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
  --     createdBy <- UserId <$> liftIO UUIDV4.nextRandom
  --     swagger <- liftIO $ createSwagger projectId createdBy swaggerJson1
  --     result <- DBT.withPool pool $ do
  --       updateSwagger (toText $ Models.Projects.Swaggers.id swagger) swaggerJson2
  --       getSwaggerById (toText $ Models.Projects.Swaggers.id swagger)
  --     fmap Models.Projects.Swaggers.swaggerJson result `shouldBe` Just swaggerJson2
