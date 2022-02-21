{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RequestMessagesSpec where

import Data.Aeson (Object, Value (Array, Bool, Null, Number, Object, String), decodeStrict, eitherDecode, eitherDecodeStrict)
import Data.Aeson as AE
import Data.Aeson.QQ
import Data.Aeson.Types as AET
import Data.Text.Encoding.Base64 as B64
import Data.Time (secondsToNominalDiffTime)
import qualified Data.Time as Time
import Data.Time.LocalTime (calendarTimeTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import qualified Models.Apis.Endpoints as Endpoints
import qualified Models.Apis.Fields as Fields
import qualified Models.Apis.RequestDumps as RequestDumps
import qualified Models.Projects.Projects as Projects
import Relude
import Relude.Unsafe (read)
import qualified RequestMessages
import Test.Hspec
import Test.Hspec.DB (TestDB, describeDB, itDB)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "Process Messages" $ do
    it "value to fields" $ do
      let exJSON =
            [aesonQQ| {
              "menu": {
                "id": "file",
                "value": "File",
                "popup": {
                  "menuitem": [
                    {"value": "New", "onclick": "CreateNewDoc()"},
                    {"value": "Open", "onclick": "OpenDoc()"},
                    {"value": "Close", "onclick": "CloseDoc()"}
                  ]
                }
              }
            }|]
      let expectedResp =
            [ (".menu.id", AE.String "file"),
              (".menu.value", AE.String "File"),
              (".menu.popup.menuitem.[].value", AE.String "Close"),
              (".menu.popup.menuitem.[].onclick", AE.String "CloseDoc()"),
              (".menu.popup.menuitem.[].value", AE.String "Open"),
              (".menu.popup.menuitem.[].onclick", AE.String "OpenDoc()"),
              (".menu.popup.menuitem.[].value", AE.String "New"),
              (".menu.popup.menuitem.[].onclick", AE.String "CreateNewDoc()")
            ]
      RequestMessages.valueToFields exJSON `shouldBe` expectedResp

    it "should should process request messages" $ do
      recId <- UUIDV4.nextRandom
      timestamp <- Time.getZonedTime
      let requestMsg =
            RequestMessages.RequestMessage
              { timestamp = timestamp,
                projectId = UUID.nil,
                host = "http://apitoolkit.io",
                method = "POST",
                referer = "https://referer",
                urlPath = "/path/to/data",
                protoMajor = 1,
                protoMinor = 1,
                durationMicroSecs = 50000,
                duration = 500,
                requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
                responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
                requestBody = B64.encodeBase64 [r|{"key": "value"}|],
                responseBody = B64.encodeBase64 [r|{"key": "value"}|],
                statusCode = 203
              }

      let resp = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId
      putStrLn $ show resp

      let respDump =
            RequestDumps.RequestDump
              { id = recId,
                createdAt = timestamp,
                updatedAt = timestamp,
                projectId = UUID.nil,
                host = "http://apitoolkit.io",
                urlPath = "/path/to/data",
                method = "POST",
                referer = "https://referer",
                protoMajor = 1,
                protoMinor = 1,
                duration = calendarTimeTime $ secondsToNominalDiffTime 500,
                requestHeaders = Array [],
                responseHeaders = Array [],
                requestBody = Object (fromList [("key", String "value")]),
                responseBody = Object (fromList [("key", String "value")]),
                statusCode = 203
              }
      let respEndpoint =
            Endpoints.Endpoint
              { id = Endpoints.EndpointId {unEndpointId = recId},
                createdAt = timestamp,
                updatedAt = timestamp,
                projectId = Projects.ProjectId {unProjectId = UUID.nil},
                urlPath = "/path/to/data",
                urlParams = Object (fromList []),
                method = "POST",
                hosts = ["http://apitoolkit.io"],
                requestHashes = [",.key"],
                responseHashes = [",.key"],
                queryparamHashes = []
              }
      let fields =
            [ ( Fields.Field
                  { id =
                      Fields.FieldId
                        { Fields.unFieldId = recId
                        },
                    createdAt = timestamp,
                    updatedAt = timestamp,
                    projectId = Projects.ProjectId {unProjectId = UUID.nil},
                    endpoint = Endpoints.EndpointId {unEndpointId = UUID.nil},
                    key = "key",
                    fieldType = Fields.FTString,
                    fieldTypeOverride = Just "",
                    format = "text",
                    formatOverride = Just "",
                    description = "",
                    keyPath = ["", "key"],
                    keyPathStr = ".key",
                    fieldCategory = Fields.FCRequestBody
                  },
                []
              ),
              ( Fields.Field
                  { id =
                      Fields.FieldId
                        { Fields.unFieldId = UUID.nil
                        },
                    createdAt = timestamp,
                    updatedAt = timestamp,
                    projectId = Projects.ProjectId {Projects.unProjectId = UUID.nil},
                    endpoint = Endpoints.EndpointId {Endpoints.unEndpointId = UUID.nil},
                    key = "key",
                    fieldType = Fields.FTString,
                    fieldTypeOverride = Just "",
                    format = "text",
                    formatOverride = Just "",
                    description = "",
                    keyPath = ["", "key"],
                    keyPathStr = ".key",
                    fieldCategory = Fields.FCResponseBody
                  },
                []
              )
            ]

      resp `shouldBe` Right (respDump, respEndpoint, fields)

    it "should should process request messages even with get request and empty request body" $ do
      recId <- UUIDV4.nextRandom
      timestamp <- Time.getZonedTime
      let requestMsg =
            RequestMessages.RequestMessage
              { timestamp = timestamp,
                projectId = UUID.nil,
                host = "http://apitoolkit.io",
                method = "POST",
                referer = "https://referer",
                urlPath = "/path/to/data",
                protoMajor = 1,
                protoMinor = 1,
                durationMicroSecs = 50000,
                duration = 500,
                requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
                responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
                requestBody = B64.encodeBase64 "",
                responseBody = B64.encodeBase64 [r|{"key": "value"}|],
                statusCode = 203
              }

      let resp = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId
      putStrLn $ show resp

      let respDump =
            RequestDumps.RequestDump
              { id = recId,
                createdAt = timestamp,
                updatedAt = timestamp,
                projectId = UUID.nil,
                host = "http://apitoolkit.io",
                urlPath = "/path/to/data",
                method = "POST",
                referer = "https://referer",
                protoMajor = 1,
                protoMinor = 1,
                duration = calendarTimeTime $ secondsToNominalDiffTime 500,
                requestHeaders = Array [],
                responseHeaders = Array [],
                requestBody = Object (fromList []),
                responseBody = Object (fromList [("key", String "value")]),
                statusCode = 203
              }
      let respEndpoint =
            Endpoints.Endpoint
              { id = Endpoints.EndpointId {unEndpointId = recId},
                createdAt = timestamp,
                updatedAt = timestamp,
                projectId = Projects.ProjectId {unProjectId = UUID.nil},
                urlPath = "/path/to/data",
                urlParams = Object (fromList []),
                method = "POST",
                hosts = ["http://apitoolkit.io"],
                requestHashes = [""],
                responseHashes = [",.key"],
                queryparamHashes = []
              }
      let fields =
            [ ( Fields.Field
                  { id =
                      Fields.FieldId
                        { Fields.unFieldId = UUID.nil
                        },
                    createdAt = timestamp,
                    updatedAt = timestamp,
                    projectId = Projects.ProjectId {Projects.unProjectId = UUID.nil},
                    endpoint = Endpoints.EndpointId {Endpoints.unEndpointId = UUID.nil},
                    key = "key",
                    fieldType = Fields.FTString,
                    fieldTypeOverride = Just "",
                    format = "text",
                    formatOverride = Just "",
                    description = "",
                    keyPath = ["", "key"],
                    keyPathStr = ".key",
                    fieldCategory = Fields.FCResponseBody
                  },
                []
              )
            ]

      resp `shouldBe` Right (respDump, respEndpoint, fields)

  describe "Regex Formats Gen" $ do
    it "should get support string types" $ do
      RequestMessages.valueToFormatStr "123" `shouldBe` "integer"
      RequestMessages.valueToFormatStr "abc" `shouldBe` "text"
