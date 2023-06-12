{-# LANGUAGE OverloadedStrings #-}

module Pages.GenerateSwaggerSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Aeson qualified as AE
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding as TE
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Pages.GenerateSwagger

import Relude
import Test.Hspec

-- Define a sample input for the generateSwagger function
sampleProject :: Maybe Projects.Project
sampleProject =
  Just
    ( Projects.Project
        { id = Projects.ProjectId UUID.nil
        , title = "Sample Project"
        , description = "Sample description"
        , active = True
        , paymentPlan = "pro"
        , createdAt = undefined
        , updatedAt = undefined
        , deletedAt = undefined
        }
    )

sampleEndpoints :: V.Vector Endpoints.SwEndpoint
sampleEndpoints =
  V.fromList
    [ Endpoints.SwEndpoint
        { Endpoints.urlPath = "/users"
        , Endpoints.urlParams = AE.Null
        , Endpoints.method = "GET"
        , Endpoints.hosts = V.fromList ["localhost"]
        , Endpoints.hash = "endpoint1"
        }
    ]

sampleShapes :: V.Vector Shapes.SwShape
sampleShapes =
  V.fromList
    [ Shapes.SwShape
        { swEndpointHash = "endpoint1"
        , swFieldHashes = V.fromList ["field1", "field2"]
        , swRequestBodyKeypaths = V.fromList ["users.field1", "users.field2"]
        , swResponseBodyKeypaths = V.fromList ["users.field1", "users.field2"]
        , swResponseHeadersKeypaths = V.fromList []
        , swRequestHeadersKeypaths = V.fromList []
        , swQueryParamsKeypaths = V.fromList []
        , swHash = "shap1"
        , swStatusCode = 200
        }
    ]

sampleFields :: V.Vector Fields.SwField
sampleFields =
  V.fromList
    [ Fields.SwField
        { fHash = "field1"
        , fFieldCategory = Fields.FCRequestBody
        , fKeyPath = "users.field1"
        , fDescription = "Sample field 1"
        , fEndpointHash = "endpoint1"
        , fKey = "key"
        , fFieldType = Fields.FTString
        , fFormat = "text"
        }
    ]

sampleFormats :: V.Vector Formats.SwFormat
sampleFormats =
  V.fromList
    [ Formats.SwFormat
        { Formats.swFieldHash = "field1"
        , Formats.swFieldFormat = "Text"
        , Formats.swFieldType = Fields.FTString
        , Formats.swHash = ""
        }
    ]

expectedSwaggerJSON = T.pack "{ \"openapi\": \"3.0.0\", \"info\": { \"description\": \"Sample Project\", \"title\": \"Sample description\", \"version\": \"1.0.0\", \"termsOfService\": \"'https://apitoolkit.io/terms-and-conditions/'\" }, \"servers\": [ { \"url\": \"localhost\" } ], \"paths\": { \"/users\": { \"get\": { \"responses\": { \"200\": { \"description\": \"\", \"headers\": {}, \"content\": { \"*/*\": { \"users.field1\": { \"description\": \"\", \"type\": \"Text\" }, \"users.field2\": { \"description\": \"\", \"type\": \"Text\" } } } } } } } }"

spec :: Spec
spec = describe "generateSwagger" $ do
  it "generates Swagger JSON matching the expected output" $ do
    let generatedSwaggerJSON = generateSwagger sampleProject sampleEndpoints sampleShapes sampleFields sampleFormats
    case (decode expectedSwaggerJSON, decode (encode generatedSwaggerJSON)) of
      (Just expected, Just generated) -> expected `shouldBe` generated
      _ -> expectationFailure "Failed to decode JSON"
