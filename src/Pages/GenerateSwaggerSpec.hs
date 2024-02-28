{-# LANGUAGE OverloadedStrings #-}

module Pages.GenerateSwaggerSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.QQ
import Data.Vector qualified as V
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Pages.GenerateSwagger
import Relude
import RequestMessages qualified
import Test.Hspec
import Test.Hspec.Expectations.Json

projectTitle :: Text
projectTitle = "Sample Project"

projectDescription :: Text
projectDescription = "Sample description"

----
--- GENERAL STRUCTURE TEST
----

sampleEndpoints :: V.Vector Endpoints.SwEndpoint
sampleEndpoints =
  V.fromList
    [ Endpoints.SwEndpoint
        { urlPath = "/users",
          urlParams = AE.Null,
          method = "GET",
          host = "localhost",
          hash = "endpoint1_GET",
          description = ""
        },
      Endpoints.SwEndpoint
        { urlPath = "/users",
          urlParams = AE.Null,
          method = "POST",
          host = "localhost",
          hash = "endpoint1_POST",
          description = ""
        }
    ]

sampleShapes :: V.Vector Shapes.SwShape
sampleShapes =
  V.fromList
    [ Shapes.SwShape
        { swEndpointHash = "endpoint1_GET",
          swFieldHashes = V.fromList ["field1", "field2", "field8"],
          swRequestBodyKeypaths = V.fromList [],
          swResponseBodyKeypaths = V.fromList [".users[*].name", ".users[*].age", ".key.end"],
          swResponseHeadersKeypaths = V.fromList [],
          swRequestHeadersKeypaths = V.fromList [],
          swQueryParamsKeypaths = V.fromList [],
          swHash = "shap1",
          swStatusCode = 200,
          swRequestDescription = "",
          swResponseDescription = ""
        },
      Shapes.SwShape
        { swEndpointHash = "endpoint1_POST",
          swFieldHashes = V.fromList ["field3", "field4", "field5", "field6", "field7"],
          swRequestBodyKeypaths = V.fromList [".name", ".age", ".weight"],
          swResponseBodyKeypaths = V.fromList [".message", ".type"],
          swResponseHeadersKeypaths = V.fromList [],
          swRequestHeadersKeypaths = V.fromList [],
          swQueryParamsKeypaths = V.fromList [],
          swHash = "shap2",
          swStatusCode = 401,
          swRequestDescription = "",
          swResponseDescription = ""
        },
      Shapes.SwShape
        { swEndpointHash = "endpoint1_POST",
          swFieldHashes = V.fromList ["field3", "field4", "field5", "field6", "field7"],
          swRequestBodyKeypaths = V.fromList [".name", ".age", ".weight"],
          swResponseBodyKeypaths = V.fromList [".message", ".type"],
          swResponseHeadersKeypaths = V.fromList [],
          swRequestHeadersKeypaths = V.fromList [],
          swQueryParamsKeypaths = V.fromList [],
          swHash = "shap3",
          swStatusCode = 201,
          swRequestDescription = "",
          swResponseDescription = ""
        }
    ]

sampleFields :: V.Vector Fields.SwField
sampleFields =
  V.fromList
    [ Fields.SwField
        { fHash = "field1",
          fFieldCategory = Fields.FCResponseBody,
          fKeyPath = ".users[*].name",
          fDescription = "",
          fEndpointHash = "endpoint1_GET",
          fKey = "key",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field2",
          fFieldCategory = Fields.FCResponseBody,
          fKeyPath = ".users[*].age",
          fDescription = "",
          fEndpointHash = "endpoint1_GET",
          fKey = "key",
          fFieldType = Fields.FTNumber,
          fFormat = "integer",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field3",
          fFieldCategory = Fields.FCRequestBody,
          fKeyPath = ".name",
          fDescription = "",
          fEndpointHash = "endpoint1_POST",
          fKey = "key",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field4",
          fFieldCategory = Fields.FCRequestBody,
          fKeyPath = ".age",
          fDescription = "",
          fEndpointHash = "endpoint1_POST",
          fKey = "key",
          fFieldType = Fields.FTNumber,
          fFormat = "integer",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field5",
          fFieldCategory = Fields.FCRequestBody,
          fKeyPath = ".weight",
          fDescription = "",
          fEndpointHash = "endpoint1_POST",
          fKey = "key",
          fFieldType = Fields.FTNumber,
          fFormat = "integer",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field6",
          fFieldCategory = Fields.FCResponseBody,
          fKeyPath = ".message",
          fDescription = "",
          fEndpointHash = "endpoint1_POST",
          fKey = "key",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field7",
          fFieldCategory = Fields.FCResponseBody,
          fKeyPath = ".type",
          fDescription = "",
          fEndpointHash = "endpoint1_POST",
          fKey = "key",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field8",
          fFieldCategory = Fields.FCResponseBody,
          fKeyPath = ".key.end",
          fDescription = "",
          fEndpointHash = "endpoint1_GET",
          fKey = "key",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        }
    ]

sampleFormats :: V.Vector Formats.SwFormat
sampleFormats =
  V.fromList
    [ Formats.SwFormat
        { swFieldHash = "field1",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["jon"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field2",
          swFieldFormat = "integer",
          swFieldType = Fields.FTNumber,
          swExamples = ["18"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field3",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["jon"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field4",
          swFieldFormat = "integer",
          swFieldType = Fields.FTNumber,
          swExamples = ["18"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field5",
          swFieldFormat = "integer",
          swFieldType = Fields.FTNumber,
          swExamples = ["75"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field6",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["hello"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field7",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["SUCCESS"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field8",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["no"],
          swHash = ""
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
                  "application/json": {
                    "schema": {
                      "type": "object",
                      "properties": {
                        "users": {
                          "type": "array",
                          "items": {
                            "properties": {
                              "name": {
                                "description": "",
                                "type": "string",
                                "format": "text",
                                "example": "jon"
                              },
                              "age": {
                                "description": "",
                                "type": "number",
                                "format": "integer",
                                "example": "18"
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
                              "type": "string",
                              "format": "text",
                              "example": "no"
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
                  "application/json":  {
                    "schema": {
                      "type": "object",
                      "properties": {
                        "message": {
                          "description": "",
                          "type": "string",
                          "format": "text",
                          "example": "hello"
                        },
                        "type": {
                          "description": "",
                          "type": "string",
                          "format": "text",
                          "example": "SUCCESS"
                        }
                      }
                    }
                  }
                }
              },
              "201": {
                "description": "",
                "content": {
                  "application/json": {
                    "schema": {
                      "type": "object",
                      "properties": {
                        "message": {
                          "description": "",
                          "type": "string",
                          "format": "text",
                          "example": "hello"
                        },
                        "type": {
                          "description": "",
                          "type": "string",
                          "format": "text",
                          "example": "SUCCESS"
                        }
                      }
                    }
                  }
                }
              }
            },
             "requestBody": {
               "content": {
                 "application/json": {
                   "schema": {
                     "type": "object",
                      "properties": {
                        "name": {
                          "description": "",
                          "type": "string",
                          "format": "text",
                          "example":"jon"
                        },
                        "age": {
                          "description": "",
                          "type": "number",
                          "format": "integer",
                          "example": "18"
                        },
                        "weight": {
                          "description": "",
                          "type": "number",
                          "format": "integer",
                          "example": "75"
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

----
--- HEADERS TEST
----
hSampleEndpoints :: V.Vector Endpoints.SwEndpoint
hSampleEndpoints =
  V.fromList
    [ Endpoints.SwEndpoint
        { urlPath = "/headers",
          urlParams = AE.Null,
          method = "GET",
          host = "localhost",
          hash = "endpoint1_GET",
          description = ""
        }
    ]

hSampleShapes :: V.Vector Shapes.SwShape
hSampleShapes =
  V.fromList
    [ Shapes.SwShape
        { swEndpointHash = "endpoint1_GET",
          swFieldHashes = V.fromList ["field1", "field2", "field3"],
          swRequestBodyKeypaths = V.fromList [],
          swResponseBodyKeypaths = V.fromList [],
          swResponseHeadersKeypaths = V.fromList [".Access-Control-Allow-Credentials.[]", ".Access-Control-Allow-Methods.[]", ".Content-Length"],
          swRequestHeadersKeypaths = V.fromList [],
          swQueryParamsKeypaths = V.fromList [],
          swHash = "shape1",
          swStatusCode = 200,
          swRequestDescription = "",
          swResponseDescription = ""
        }
    ]

hSampleFields :: V.Vector Fields.SwField
hSampleFields =
  V.fromList
    [ Fields.SwField
        { fHash = "field1",
          fFieldCategory = Fields.FCResponseHeader,
          fKeyPath = ".Access-Control-Allow-Credentials.[]",
          fDescription = "Sample header 1",
          fEndpointHash = "endpoint1_GET",
          fKey = "header1",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field2",
          fFieldCategory = Fields.FCResponseHeader,
          fKeyPath = ".Access-Control-Allow-Methods.[]",
          fDescription = "Sample header 2",
          fEndpointHash = "endpoint1_GET",
          fKey = "header2",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field3",
          fFieldCategory = Fields.FCResponseHeader,
          fKeyPath = ".Content-Length",
          fDescription = "Sample header 3",
          fEndpointHash = "endpoint1_GET",
          fKey = "header3",
          fFieldType = Fields.FTNumber,
          fFormat = "integer",
          fIsEnum = False,
          fIsRequired = False
        }
    ]

hSampleFormats :: V.Vector Formats.SwFormat
hSampleFormats =
  V.fromList
    [ Formats.SwFormat
        { swFieldHash = "field1",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["header 1"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field2",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["header 2"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field3",
          swFieldFormat = "integer",
          swFieldType = Fields.FTNumber,
          swExamples = ["header 3"],
          swHash = ""
        }
    ]

hExpectedSwaggerJSON :: AE.Value
hExpectedSwaggerJSON =
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
        "/headers": {
          "get": {
            "responses": {
              "200": {
                content: {
                  "application/json": {}
                },
                "description": "",
                "headers": {
                   "content": {
                      "schema": {
                        "properties": {
                          "Access-Control-Allow-Credentials": {
                            "description": "Sample header 1",
                            "items": {
                              "type": "string",
                              "format": "text",
                              "example": "header 1"
                            },
                            "type": "array"
                          },
                          "Access-Control-Allow-Methods": {
                            "description": "Sample header 2",
                            "items": {
                              "type": "string",
                              "format": "text",
                              "example": "header 2"
                            },
                            "type": "array"
                          },
                          "Content-Length": {
                            "description": "Sample header 3",
                             "type": "number",
                             "format": "integer",
                             "example": "header 3"
                          }
                        },
                        "type": "object"
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

-----
--- ParametersTests
----

pSampleEndpoints :: V.Vector Endpoints.SwEndpoint
pSampleEndpoints =
  V.fromList
    [ Endpoints.SwEndpoint
        { urlPath = "/headers",
          urlParams = AE.Null,
          method = "GET",
          host = "localhost",
          hash = "endpoint1_GET",
          description = ""
        }
    ]

pSampleShapes :: V.Vector Shapes.SwShape
pSampleShapes =
  V.fromList
    [ Shapes.SwShape
        { swEndpointHash = "endpoint1_GET",
          swFieldHashes = V.fromList ["field1", "field2", "field3"],
          swRequestBodyKeypaths = V.fromList [],
          swResponseBodyKeypaths = V.fromList [],
          swResponseHeadersKeypaths = V.fromList [],
          swRequestHeadersKeypaths = V.fromList [],
          swQueryParamsKeypaths = V.fromList ["from.[]", "page.[]", "ref.[]"],
          swHash = "shape1",
          swStatusCode = 200,
          swRequestDescription = "",
          swResponseDescription = ""
        }
    ]

pSampleFields :: V.Vector Fields.SwField
pSampleFields =
  V.fromList
    [ Fields.SwField
        { fHash = "field1",
          fFieldCategory = Fields.FCQueryParam,
          fKeyPath = "from.[]",
          fDescription = "Sample param 1",
          fEndpointHash = "endpoint1_GET",
          fKey = "header1",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field2",
          fFieldCategory = Fields.FCQueryParam,
          fKeyPath = "page.[]",
          fDescription = "Sample param 2",
          fEndpointHash = "endpoint1_GET",
          fKey = "header2",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        },
      Fields.SwField
        { fHash = "field3",
          fFieldCategory = Fields.FCQueryParam,
          fKeyPath = "ref.[]",
          fDescription = "Sample param 3",
          fEndpointHash = "endpoint1_GET",
          fKey = "header3",
          fFieldType = Fields.FTString,
          fFormat = "text",
          fIsEnum = False,
          fIsRequired = False
        }
    ]

pSampleFormats :: V.Vector Formats.SwFormat
pSampleFormats =
  V.fromList
    [ Formats.SwFormat
        { swFieldHash = "field1",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["/home"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field2",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["2"],
          swHash = ""
        },
      Formats.SwFormat
        { swFieldHash = "field3",
          swFieldFormat = "text",
          swFieldType = Fields.FTString,
          swExamples = ["me"],
          swHash = ""
        }
    ]

pExpectedSwaggerJSON :: AE.Value
pExpectedSwaggerJSON =
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
        "/headers": {
          "get": {
            "parameters": [
              {
                "in": "query",
                "name": "from",
                "description": "Sample param 1",
                "schema": {
                  "type": "string",
                   "format": "text",
                   "example": "/home"
                }
              },
              {
                "in": "query",
                "name": "page",
                "description": "Sample param 2",
                "schema": {
                  "type": "string",
                  "format": "text",
                  "example": "2"
                }
              },
              {
                "in": "query",
                "name": "ref",
                "description": "Sample param 3",
                "schema": {
                  "type": "string",
                  "format": "text",
                  "example": "me"
                }
              }
            ],
            "responses": {
              "200": {
                "description": ""
              }
            }
          }
        }
      }
    }
  |]

spec :: Spec
spec = describe "generateSwagger" do
  it "GENERAL: generates Swagger JSON matching the expected output" do
    let generatedSwaggerJSON = generateSwagger projectTitle projectDescription sampleEndpoints sampleShapes sampleFields sampleFormats
    RequestMessages.valueToFields generatedSwaggerJSON `shouldBe` RequestMessages.valueToFields expectedSwaggerJSON
  --- headers
  it "HEADERS: generates swagger JSON matching expected output" do
    let hGeneratedSwaggerJSON = generateSwagger projectTitle projectDescription hSampleEndpoints hSampleShapes hSampleFields hSampleFormats
    hGeneratedSwaggerJSON `shouldBeJson` hExpectedSwaggerJSON
  -- parameters
  it "PARAMETERS: generates swagger JSON matching expected output" do
    let pGeneratedSwaggerJSON = generateSwagger projectTitle projectDescription pSampleEndpoints pSampleShapes pSampleFields pSampleFormats
    RequestMessages.valueToFields pGeneratedSwaggerJSON `shouldBe` RequestMessages.valueToFields pExpectedSwaggerJSON
