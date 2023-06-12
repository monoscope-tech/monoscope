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
import Database.PostgreSQL.Entity.DBT (withPool)
import Data.Maybe
import Pkg.TmpPg qualified as TmpPg
import Database.PostgreSQL.Transact (DBT)

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
  putStrLn "In setup 🔥"
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ withDbCache $ \dbCache -> traceShowM "In with dbcache 🔥" >> withConfig (cacheConfig dbCache) $ \db -> do
    putStrLn "In with config 🔥"
    conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
    putStrLn $ "after conn"  
    -- putStrLn $ TmpPostgres.toConnectionString db
    migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ MigrationDirectory ("./static/migrations" :: FilePath)
    putStrLn $ "migration result: " <> show migrationRes
    x <- withSnapshot db $ \snapshot -> do
      withConfig (snapshotConfig snapshot) $ \migratedDb -> f =<< createPool (connectPostgreSQL $ toConnectionString migratedDb) close 2 60 10
    pTraceShowM x 
    pass


spec :: Spec
spec = aroundAll TmpPg.withSetup $ describe "Models.Projects.Swaggers" $ do
  let swaggerId = SwaggerId UUID.nil 
  let swaggerJson' = object ["info" .= object ["title" .= "API"]]
  let swaggerJson1 = object ["info" .= object ["title" .= "API 1"]]
  let swaggerJson2 = object ["info" .= object ["title" .= "API 2"]]
  describe "addSwagger" $
    it "should insert a new Swagger into the database" $ \pool -> do
      currentTime <- liftIO getZonedTime
      let swagger =
            Swagger
              { Models.Projects.Swaggers.id = swaggerId
              , projectId = ProjectId UUID.nil 
              , createdBy =  UserId UUID.nil
              , createdAt = currentTime 
              , updatedAt = currentTime 
              , swaggerJson = swaggerJson'
              }
      result <- withPool pool $ do
        _ <- addSwagger swagger
        getSwaggerById (swaggerId.toText)
      (fromJust result).swaggerJson `shouldBe` swaggerJson' 

  describe "getSwaggerById" $
    it "should retrieve a Swagger by its ID" $ \pool -> do
      swagger <- withPool pool $ createSwagger (ProjectId UUID.nil) (UserId UUID.nil) swaggerJson'
      result <- withPool pool $ getSwaggerById ( swagger.id.toText )
      (fromJust result).swaggerJson `shouldBe` swagger.swaggerJson

  describe "swaggersByProject" $
    it "should retrieve all Swaggers for a given Project" $ \pool -> do
      result <- withPool pool $ do
        _ <- createSwagger (ProjectId UUID.nil) (UserId UUID.nil)  swaggerJson1
        _ <- createSwagger (ProjectId UUID.nil) (UserId UUID.nil)  swaggerJson2
        swaggersByProject (ProjectId UUID.nil) 
      (map (^. #swaggerJson) (toList result)) `shouldBe` [swaggerJson',swaggerJson', swaggerJson1, swaggerJson2]

  describe "updateSwagger" $
    it "should update the Swagger JSON of a Swagger" $ \pool -> do
      result <- withPool pool $ do
        _ <- updateSwagger (swaggerId.toText) swaggerJson2
        getSwaggerById (swaggerId.toText)
      (fromJust result).swaggerJson `shouldBe` swaggerJson2
