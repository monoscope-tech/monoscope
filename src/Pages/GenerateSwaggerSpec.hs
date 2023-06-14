{-# LANGUAGE OverloadedStrings #-}

module Pages.GenerateSwaggerSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Aeson qualified as AE
import Data.Aeson.QQ
import Data.Vector qualified as V
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Pages.GenerateSwagger
import RequestMessages qualified

import Relude
import Test.Hspec

projectTitle :: Text
projectTitle = "Sample Project"
projectDescription :: Text
projectDescription = "Sample description"

sampleEndpoints :: V.Vector Endpoints.SwEndpoint
sampleEndpoints =
  V.fromList
    [ Endpoints.SwEndpoint
        { Endpoints.urlPath = "/users"
        , Endpoints.urlParams = AE.Null
        , Endpoints.method = "GET"
        , Endpoints.hosts = V.fromList ["localhost"]
        , Endpoints.hash = "endpoint1_GET"
        }
    , Endpoints.SwEndpoint
        { Endpoints.urlPath = "/users"
        , Endpoints.urlParams = AE.Null
        , Endpoints.method = "POST"
        , Endpoints.hosts = V.fromList ["localhost"]
        , Endpoints.hash = "endpoint1_POST"
        }
    ]

sampleShapes :: V.Vector Shapes.SwShape
sampleShapes =
  V.fromList
    [ Shapes.SwShape
        { swEndpointHash = "endpoint1_GET"
        , swFieldHashes = V.fromList ["field1", "field2", "field8"]
        , swRequestBodyKeypaths = V.fromList []
        , swResponseBodyKeypaths = V.fromList [".users[*].name", ".users[*].age", ".key.end"]
        , swResponseHeadersKeypaths = V.fromList []
        , swRequestHeadersKeypaths = V.fromList []
        , swQueryParamsKeypaths = V.fromList []
        , swHash = "shap1"
        , swStatusCode = 200
        }
    , Shapes.SwShape
        { swEndpointHash = "endpoint1_POST"
        , swFieldHashes = V.fromList ["field3", "field4", "field5", "field6", "field7"]
        , swRequestBodyKeypaths = V.fromList [".name", ".age", ".weight"]
        , swResponseBodyKeypaths = V.fromList [".message", ".type"]
        , swResponseHeadersKeypaths = V.fromList []
        , swRequestHeadersKeypaths = V.fromList []
        , swQueryParamsKeypaths = V.fromList []
        , swHash = "shap2"
        , swStatusCode = 401
        }
    , Shapes.SwShape
        { swEndpointHash = "endpoint1_POST"
        , swFieldHashes = V.fromList ["field3", "field4", "field5", "field6", "field7"]
        , swRequestBodyKeypaths = V.fromList [".name", ".age", ".weight"]
        , swResponseBodyKeypaths = V.fromList [".message", ".type"]
        , swResponseHeadersKeypaths = V.fromList []
        , swRequestHeadersKeypaths = V.fromList []
        , swQueryParamsKeypaths = V.fromList []
        , swHash = "shap3"
        , swStatusCode = 201
        }
    ]

sampleFields :: V.Vector Fields.SwField
sampleFields =
  V.fromList
    [ Fields.SwField
        { fHash = "field1"
        , fFieldCategory = Fields.FCResponseBody
        , fKeyPath = ".users[*].name"
        , fDescription = ""
        , fEndpointHash = "endpoint1_GET"
        , fKey = "key"
        , fFieldType = Fields.FTString
        , fFormat = "text"
        }
    , Fields.SwField
        { fHash = "field2"
        , fFieldCategory = Fields.FCResponseBody
        , fKeyPath = ".users[*].age"
        , fDescription = ""
        , fEndpointHash = "endpoint1_GET"
        , fKey = "key"
        , fFieldType = Fields.FTNumber
        , fFormat = "Integer"
        }
    , Fields.SwField
        { fHash = "field3"
        , fFieldCategory = Fields.FCRequestBody
        , fKeyPath = ".name"
        , fDescription = ""
        , fEndpointHash = "endpoint1_POST"
        , fKey = "key"
        , fFieldType = Fields.FTString
        , fFormat = "text"
        }
    , Fields.SwField
        { fHash = "field4"
        , fFieldCategory = Fields.FCRequestBody
        , fKeyPath = ".age"
        , fDescription = ""
        , fEndpointHash = "endpoint1_POST"
        , fKey = "key"
        , fFieldType = Fields.FTNumber
        , fFormat = "integer"
        }
    , Fields.SwField
        { fHash = "field5"
        , fFieldCategory = Fields.FCRequestBody
        , fKeyPath = ".weight"
        , fDescription = ""
        , fEndpointHash = "endpoint1_POST"
        , fKey = "key"
        , fFieldType = Fields.FTNumber
        , fFormat = "integer"
        }
    , Fields.SwField
        { fHash = "field6"
        , fFieldCategory = Fields.FCResponseBody
        , fKeyPath = ".message"
        , fDescription = ""
        , fEndpointHash = "endpoint1_POST"
        , fKey = "key"
        , fFieldType = Fields.FTString
        , fFormat = "text"
        }
    , Fields.SwField
        { fHash = "field7"
        , fFieldCategory = Fields.FCResponseBody
        , fKeyPath = ".type"
        , fDescription = ""
        , fEndpointHash = "endpoint1_POST"
        , fKey = "key"
        , fFieldType = Fields.FTString
        , fFormat = "text"
        }
    ,
        Fields.SwField
        { fHash = "field8"
        , fFieldCategory = Fields.FCResponseBody
        , fKeyPath = ".key.end"
        , fDescription = ""
        , fEndpointHash = "endpoint1_GET"
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
    , Formats.SwFormat
        { Formats.swFieldHash = "field2"
        , Formats.swFieldFormat = "Integer"
        , Formats.swFieldType = Fields.FTNumber
        , Formats.swHash = ""
        }
    , Formats.SwFormat
        { Formats.swFieldHash = "field3"
        , Formats.swFieldFormat = "Text"
        , Formats.swFieldType = Fields.FTString
        , Formats.swHash = ""
        }
    , Formats.SwFormat
        { Formats.swFieldHash = "field4"
        , Formats.swFieldFormat = "Integer"
        , Formats.swFieldType = Fields.FTNumber
        , Formats.swHash = ""
        }
    , Formats.SwFormat
        { Formats.swFieldHash = "field5"
        , Formats.swFieldFormat = "Integer"
        , Formats.swFieldType = Fields.FTNumber
        , Formats.swHash = ""
        }
    , Formats.SwFormat
        { Formats.swFieldHash = "field6"
        , Formats.swFieldFormat = "Text"
        , Formats.swFieldType = Fields.FTString
        , Formats.swHash = ""
        }
    , Formats.SwFormat
        { Formats.swFieldHash = "field7"
        , Formats.swFieldFormat = "Text"
        , Formats.swFieldType = Fields.FTString
        , Formats.swHash = ""
        }
    , Formats.SwFormat
      { Formats.swFieldHash = "field8"
      , Formats.swFieldFormat = "Text"
      , Formats.swFieldType = Fields.FTString
      , Formats.swHash = ""
      }
    ]

expectedSwaggerJSON :: AE.Value
expectedSwaggerJSON =
  [aesonQQ|
    {
      "openapi": "3.0.0",
      "info": {
        "title":  "Sample Project",
        "termsOfService" : "https://apitoolkit.io/terms-and-conditions/",
        "description" : "Sample description",
        "version": "1.0.0"
      },
      "servers": [{"url":"localhost"}],
      "paths": {
        "/users": {
          "get": {
            "responses": {
              "200": {
                "description": "",
                "content": {
                  "*/*": {
                    "schema": {
                      "type": "object",
                      "properties": {
                        "users": {
                          "type": "array",
                          "items": {
                            "properties": {
                              "name": {
                                "description": "",
                                "type": "string"
                              },
                              "age": {
                                "description": "",
                                "type": "number"
                              }
                            },
                            "type": "object"
                          }
                        },
                        "key": {
                          "type": "object",
                          "properties": {
                            "end": {
                              "description": "",
                              "type": "string"
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          },
          "post": {
            "responses": {
              "401": { 
                "description": "",
                "content": {
                  "*/*":  {
                    "schema": {
                      "type": "object",
                      "properties": {
                        "message": {
                          "description": "",
                          "type": "string"
                        },
                        "type": {
                          "description": "",
                          "type": "string"
                        }
                      }
                    }
                  }
                }
              },
              "201": {
                "description": "",
                "content": {
                  "*/*": {
                    "schema": {
                      "type": "object",
                      "properties": {
                        "message": {
                          "description": "",
                          "type": "string"
                        },
                        "type": {
                          "description": "",
                          "type": "string"
                        }
                      }
                    }
                  }
                }
              }
            },
             "requestBody": {
               "content": {
                 "*/*": {
                   "schema": {
                     "type": "object",
                      "properties": {
                        "name": {
                          "description": "",
                          "type": "string"
                        },
                        "age": {
                          "description": "",
                          "type": "number"
                        },
                        "weight": {
                          "description": "",
                          "type": "number"
                        }
                      }
                   }
                 }
               }
             }
          }
        }
      }
    }
  |]

spec :: Spec
spec = describe "generateSwagger" $ do
  it "generates Swagger JSON matching the expected output" $ do
    let generatedSwaggerJSON = generateSwagger projectTitle projectDescription sampleEndpoints sampleShapes sampleFields sampleFormats
    print $ encode generatedSwaggerJSON
    RequestMessages.valueToFields generatedSwaggerJSON `shouldBe` RequestMessages.valueToFields expectedSwaggerJSON