module Pages.Specification.DocumentationSpec (spec) where

import Data.Aeson (
  decodeStrict,
 )
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Pages.BodyWrapper (PageCtx (..))
import Pages.Specification.Documentation qualified as Documentation
import Pkg.TestUtils
import Relude
import Servant qualified
import Servant.Server qualified as ServantS
import System.Types (atAuthToBase, effToServantHandlerTest)
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Documentation" do
    it "should add swagger" \TestResources{..} -> do
      let swagForm = Documentation.SwaggerForm swg1 "Swagger"
      (Documentation.DocumentationMut msg) <-
        Documentation.documentationPostH testPid swagForm
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      msg `shouldBe` "Swagger added successfully"

    it "should get swagger" \TestResources{..} -> do
      (PageCtx _ (Documentation.DocumentationGet pid swaggers swaggerID jsonString)) <-
        Documentation.documentationGetH testPid Nothing
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      let jsonVal = decodeStrict (encodeUtf8 jsonString)
      let swagVal = decodeStrict (encodeUtf8 swg1)
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
