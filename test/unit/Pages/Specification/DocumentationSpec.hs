module Pages.Specification.DocumentationSpec (spec) where

import Data.Aeson qualified as AE
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Pages.BodyWrapper (PageCtx (..))
import Pages.Specification.Documentation qualified as Documentation
import Pkg.TestUtils
import Relude
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Documentation" do
    it "should add swagger" \TestResources{..} -> do
      let swagForm = Documentation.SwaggerForm swg1 "Swagger"
      (Documentation.DocumentationMut msg) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Documentation.documentationPostH testPid swagForm
      msg `shouldBe` "Swagger added successfully"

    it "should get swagger" \TestResources{..} -> do
      (PageCtx _ (Documentation.DocumentationGet pid swaggers swaggerID jsonString)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Documentation.documentationGetH testPid Nothing Nothing
      let jsonVal = AE.decodeStrict (encodeUtf8 jsonString)
      let swagVal = AE.decodeStrict (encodeUtf8 swg1)
      swagVal `shouldBe` jsonVal
      pid `shouldBe` testPid
      length swaggers `shouldBe` 1


swg1 :: Text
swg1 =
  [text|{
    "swagger": "2.0",
    "info": {
      "title": "feature.proto",
      "version": "version not set"
    },
    "schemes": [
      "http",
      "https"
    ],
    "consumes": [
      "application/json"
    ],
    "produces": [
      "application/json"
    ],
    "paths": {
      "/ops/features": {
        "get": {
          "summary": "GET /ops/features",
          "operationId": "ListFeatures",
          "responses": {
            "200": {
              "description": "",
              "schema": {
                "type": "object",
                "properties": {"name":"string"}
              }
            }
          },
          "tags": [
            "Generic"
          ]
        }
      }
    }
  }|]
