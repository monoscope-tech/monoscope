{-# LANGUAGE OverloadedStrings #-}

module Models.Projects.SwaggersSpec (spec) where

import Control.Monad.IO.Class (MonadIO (liftIO), liftIO)
import Data.Aeson (Value (..), object, (.=))
import Data.Time (getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import Data.UUID (nil)
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity qualified as DB
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Transaction qualified as DBT
import Database.PostgreSQL.Transact qualified as DBT
import Database.Postgres.Temp
import Models.Projects.Projects (ProjectId (..))
import Models.Projects.Swaggers
import Models.Users.Users (UserId (..))
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck

-- Helper function to run DBT actions within tests
runDBT :: TmpPostgres.Db -> DBT.DBT IO a -> IO a
runDBT db action = do
  let connSettings = TmpPostgres.toConnectionString db
  conn <- PG.connectPostgreSQL connSettings
  result <- DBT.runDBT action DBT.DefaultIsolationLevel conn
  PG.close conn
  pure result

-- Helper function to create a Swagger value for testing
createSwagger :: ProjectId -> UserId -> Value -> IO Swagger
createSwagger projectId createdBy swaggerJson = do
  currentTime <- liftIO getZonedTime
  let swagger =
        Swagger
          { Models.Projects.Swaggers.id = SwaggerId <$> liftIO UUIDV4.nextRandom
          , projectId = projectId
          , createdBy = createdBy
          , createdAt = currentTime
          , updatedAt = currentTime
          , swaggerJson = swaggerJson
          }
  runDBT $ addSwagger swagger
  pure swagger

resetDatabase :: IO ()
resetDatabase = do
  -- Establish a connection to the database
  let connSettings = TmpPostgres.toConnectionString db
  conn <- PG.connectPostgreSQL connSettings

  -- Execute necessary SQL statements to reset the database tables
  PG.execute_ conn "TRUNCATE TABLE projects.swagger_jsons RESTART IDENTITY CASCADE"

  -- Close the database connection
  PG.close conn

spec :: Spec
spec = beforeAll_ resetDatabase $ describe "Models.Projects.Swaggers" $ do
  describe "addSwagger" $
    it "should insert a new Swagger into the database" $ do
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
      runDBT $ addSwagger swagger
      result <- runDBT $ getSwaggerById (toText swaggerId)
      result `shouldBe` Just swagger

  describe "getSwaggerById" $
    it "should retrieve a Swagger by its ID" $ do
      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      let swaggerJson = object ["info" .= object ["title" .= "API"]]
      swagger <- liftIO $ createSwagger projectId createdBy swaggerJson
      result <- runDBT $ getSwaggerById (toText $ Models.Projects.Swaggers.id swagger)
      result `shouldBe` Just swagger

  describe "swaggersByProject" $
    it "should retrieve all Swaggers for a given Project" $ do
      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      let swaggerJson1 = object ["info" .= object ["title" .= "API 1"]]
      let swaggerJson2 = object ["info" .= object ["title" .= "API 2"]]
      swagger1 <- liftIO $ createSwagger projectId createdBy swaggerJson1
      swagger2 <- liftIO $ createSwagger projectId createdBy swaggerJson2
      result <- runDBT $ swaggersByProject projectId
      result `shouldBe` [swagger1, swagger2]

  describe "updateSwagger" $
    prop "should update the Swagger JSON of a Swagger" $ \(swaggerJson1, swaggerJson2) -> do
      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      swagger <- liftIO $ createSwagger projectId createdBy swaggerJson1
      _ <- runDBT $ updateSwagger (toText $ Models.Projects.Swaggers.id swagger) swaggerJson2
      result <- runDBT $ getSwaggerById (toText $ Models.Projects.Swaggers.id swagger)
      fmap Models.Projects.Swaggers.swaggerJson result `shouldBe` Just swaggerJson2
