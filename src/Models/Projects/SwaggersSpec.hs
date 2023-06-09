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
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Data.Maybe
import Pkg.TmpPg qualified as TmpPg

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

spec :: Spec
spec = aroundAll TmpPg.withSetup $ describe "Models.Projects.Swaggers" $ do
  describe "addSwagger" $
    it "should insert a new Swagger into the database" $ \pool -> do
      

      projectId <- ProjectId <$> liftIO UUIDV4.nextRandom
      createdBy <- UserId <$> liftIO UUIDV4.nextRandom
      swaggerId <- SwaggerId <$> liftIO UUIDV4.nextRandom
      currentTime <- liftIO getZonedTime
      let swaggerJson = object ["info" .= object ["title" .= "API"]]
      let swagger =
            Swagger
              { Models.Projects.Swaggers.id = swaggerId
              , projectId = projectId
              , createdBy = createdBy
              , createdAt = currentTime 
              , updatedAt = currentTime 
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
