module RequestMessagesSpec (spec) where

import Data.Aeson as AE
import Data.Aeson.QQ
import Data.Text.Encoding.Base64 as B64
import Data.Time (secondsToNominalDiffTime)
import Data.Time qualified as Time
import Data.Time.LocalTime (calendarTimeTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Relude
import RequestMessages qualified
import Test.Hspec
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
                duration = 50000,
                -- requestHeaders = fromList [],
                -- responseHeaders = fromList [],
                requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
                responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
                requestBody = B64.encodeBase64 [r|{"key": "value"}|],
                responseBody = B64.encodeBase64 [r|{"key": "value"}|],
                statusCode = 203
              }

      let resp = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId
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
                duration = 500,
                -- requestHeaders = fromList [],
                -- responseHeaders = fromList [],
                requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
                responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
                requestBody = B64.encodeBase64 "",
                responseBody = B64.encodeBase64 [r|{"key": "value"}|],
                statusCode = 203
              }

      let resp = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId

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
