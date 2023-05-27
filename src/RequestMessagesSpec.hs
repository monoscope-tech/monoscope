module RequestMessagesSpec (spec) where

import Data.Aeson as AE
import Data.Aeson.QQ
import Data.ByteString.Base64 qualified as B64
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Models.Projects.Projects qualified as Projects
import Relude
import Relude.Unsafe qualified as Unsafe
import RequestMessages (SDKTypes (..))
import RequestMessages qualified
import Test.Hspec
import NeatInterpolation

spec :: Spec
spec = do
  describe "Process Messages" $ do
    it "value to fields" $ do
      let exJSON =
            [aesonQQ| {
              "menu": {
                "id": "file",
                "popup": {
                  "menuitem": [
                    {"value": "v1", "onclick": "oc1"},
                    {"value": "v2", "onclick": "oc2"}
                  ]
                }
              }
            }|]
      let expectedResp =
            [ (".menu.id", [AE.String "file"])
            , -- FIXME: We can correctly handle objects in arrays.
              (".menu.popup.menuitem[*].onclick", [AE.String "oc1", AE.String "oc2"])
            , (".menu.popup.menuitem[*].value", [AE.String "v1", AE.String "v2"])
            ]
      RequestMessages.valueToFields exJSON `shouldBe` expectedResp

  -- Commented out until we fix and standardize durations handling.
  -- it "should should process request messages" $ do
  --   recId <- UUIDV4.nextRandom
  --   let timestamp = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  --   let requestMsg =
  --         RequestMessages.RequestMessage
  --           { timestamp = timestamp,
  --             projectId = UUID.nil,
  --             host = "http://apitoolkit.io",
  --             method = "POST",
  --             referer = "https://referer",
  --             urlPath = "/path/to/data",
  --             protoMajor = 1,
  --             protoMinor = 1,
  --             duration = 50000,
  --             requestHeaders = [aesonQQ|{}|],
  --             responseHeaders = [aesonQQ|{}|],
  --             -- requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
  --             -- responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
  --             requestBody = B64.encodeBase64 [r|{"key": "value"}|],
  --             responseBody = B64.encodeBase64 [r|{"key": "value"}|],
  --             statusCode = 203
  --           }

  --   let resp = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId
  --   let respDump =
  --         RequestDumps.RequestDump
  --           { id = recId,
  --             createdAt = timestamp,
  --             updatedAt = timestamp,
  --             projectId = UUID.nil,
  --             host = "http://apitoolkit.io",
  --             urlPath = "/path/to/data",
  --             method = "POST",
  --             referer = "https://referer",
  --             protoMajor = 1,
  --             protoMinor = 1,
  --             duration = calendarTimeTime $ secondsToNominalDiffTime 500,
  --             requestHeaders = Object (fromList []),
  --             responseHeaders = Object (fromList []),
  --             requestBody = Object (fromList [("key", String "value")]),
  --             responseBody = Object (fromList [("key", String "value")]),
  --             statusCode = 203
  --           }
  --   let respEndpoint =
  --         Endpoints.Endpoint
  --           { id = Endpoints.EndpointId {unEndpointId = recId},
  --             createdAt = timestamp,
  --             updatedAt = timestamp,
  --             projectId = Projects.ProjectId {unProjectId = UUID.nil},
  --             urlPath = "/path/to/data",
  --             urlParams = Object (fromList []),
  --             method = "POST",
  --             hosts = ["http://apitoolkit.io"],
  --             requestHashes = [",.key"],
  --             responseHashes = [",.key"],
  --             queryparamHashes = []
  --           }
  --   let fields =
  --         [ ( Fields.Field
  --               { id =
  --                   Fields.FieldId
  --                     { Fields.unFieldId = UUID.nil
  --                     },
  --                 createdAt = timestamp,
  --                 updatedAt = timestamp,
  --                 projectId = Projects.ProjectId {unProjectId = UUID.nil},
  --                 endpoint = Endpoints.EndpointId {unEndpointId = UUID.nil},
  --                 key = "key",
  --                 fieldType = Fields.FTString,
  --                 fieldTypeOverride = Just "",
  --                 format = "text",
  --                 formatOverride = Just "",
  --                 description = "",
  --                 keyPath = ["", "key"],
  --                 keyPathStr = ".key",
  --                 fieldCategory = Fields.FCRequestBody
  --               },
  --             []
  --           ),
  --           ( Fields.Field
  --               { id =
  --                   Fields.FieldId
  --                     { Fields.unFieldId = UUID.nil
  --                     },
  --                 createdAt = timestamp,
  --                 updatedAt = timestamp,
  --                 projectId = Projects.ProjectId {Projects.unProjectId = UUID.nil},
  --                 endpoint = Endpoints.EndpointId {Endpoints.unEndpointId = UUID.nil},
  --                 key = "key",
  --                 fieldType = Fields.FTString,
  --                 fieldTypeOverride = Just "",
  --                 format = "text",
  --                 formatOverride = Just "",
  --                 description = "",
  --                 keyPath = ["", "key"],
  --                 keyPathStr = ".key",
  --                 fieldCategory = Fields.FCResponseBody
  --               },
  --             []
  --           )
  --         ]

  --   resp `shouldBe` Right (respDump, respEndpoint, fields)

  -- it "should should process request messages even with get request and empty request body" $ do
  --   recId <- UUIDV4.nextRandom
  --   -- timestamp <- Time.getZonedTime
  --   let timestamp = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  --   let requestMsg =
  --         RequestMessages.RequestMessage
  --           { timestamp = timestamp,
  --             projectId = UUID.nil,
  --             host = "http://apitoolkit.io",
  --             method = "POST",
  --             referer = "https://referer",
  --             urlPath = "/path/to/data",
  --             protoMajor = 1,
  --             protoMinor = 1,
  --             duration = 50000,
  --             requestHeaders = [aesonQQ|{}|],
  --             responseHeaders = [aesonQQ|{}|],
  --             -- requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
  --             -- responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
  --             requestBody = B64.encodeBase64 "",
  --             responseBody = B64.encodeBase64 [r|{"key": "value"}|],
  --             statusCode = 203
  --           }

  --   let resp = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId

  --   let respDump =
  --         RequestDumps.RequestDump
  --           { id = recId,
  --             createdAt = timestamp,
  --             updatedAt = timestamp,
  --             projectId = UUID.nil,
  --             host = "http://apitoolkit.io",
  --             urlPath = "/path/to/data",
  --             method = "POST",
  --             referer = "https://referer",
  --             protoMajor = 1,
  --             protoMinor = 1,
  --             duration = calendarTimeTime $ secondsToNominalDiffTime 500,
  --             requestHeaders = Object (fromList []),
  --             responseHeaders = Object (fromList []),
  --             requestBody = Object (fromList []),
  --             responseBody = Object (fromList [("key", String "value")]),
  --             statusCode = 203
  --           }
  --   let respEndpoint =
  --         Endpoints.Endpoint
  --           { id = Endpoints.EndpointId {unEndpointId = recId},
  --             createdAt = timestamp,
  --             updatedAt = timestamp,
  --             projectId = Projects.ProjectId {unProjectId = UUID.nil},
  --             urlPath = "/path/to/data",
  --             urlParams = Object (fromList []),
  --             method = "POST",
  --             hosts = ["http://apitoolkit.io"],
  --             requestHashes = [""],
  --             responseHashes = [",.key"],
  --             queryparamHashes = []
  --           }
  --   let fields =
  --         [ ( Fields.Field
  --               { id =
  --                   Fields.FieldId
  --                     { Fields.unFieldId = UUID.nil
  --                     },
  --                 createdAt = timestamp,
  --                 updatedAt = timestamp,
  --                 projectId = Projects.ProjectId {Projects.unProjectId = UUID.nil},
  --                 endpoint = Endpoints.EndpointId {Endpoints.unEndpointId = UUID.nil},
  --                 key = "key",
  --                 fieldType = Fields.FTString,
  --                 fieldTypeOverride = Just "",
  --                 format = "text",
  --                 formatOverride = Just "",
  --                 description = "",
  --                 keyPath = ["", "key"],
  --                 keyPathStr = ".key",
  --                 fieldCategory = Fields.FCResponseBody
  --               },
  --             []
  --           )
  --         ]

  --   resp `shouldBe` Right (respDump, respEndpoint, fields)

  describe "Regex Formats Gen" $ do
    it "should get support string types" $ do
      RequestMessages.valueToFormatStr "123" `shouldBe` "integer"
      RequestMessages.valueToFormatStr "abc" `shouldBe` "text"

  describe "requestMessageEndpoint" $ do
    it "should be able to convert simple request message to series on insert db commands" $ do
      recId <- UUIDV4.nextRandom
      -- timestamp <- Time.getZonedTime
      let timestamp = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      let requestMsg =
            RequestMessages.RequestMessage
              { timestamp = timestamp
              , projectId = UUID.nil
              , sdkType = GoGin
              , host = "http://apitoolkit.io"
              , method = "POST"
              , referer = "https://referer"
              , urlPath = "/path/to/data"
              , rawUrl = "/path/to/data"
              , pathParams = [aesonQQ|{}|]
              , queryParams = [aesonQQ|{}|]
              , protoMajor = 1
              , protoMinor = 1
              , duration = 50000
              , requestHeaders = [aesonQQ|{}|]
              , responseHeaders = [aesonQQ|{}|]
              , -- requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
                -- responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
                requestBody = B64.encodeBase64 ""
              , responseBody = B64.encodeBase64 $ encodeUtf8 [text|{"key": "value"}|]
              , statusCode = 203
              }
      let projectCache = Projects.ProjectCache{hosts = [], endpointHashes = ["abc"], shapeHashes = [], redactFieldslist = []}
      let Right (query, params, _) = RequestMessages.requestMsgToDumpAndEndpoint projectCache requestMsg timestamp recId
      traceShowM "In request Message Endpoint test ===BEGIN==="
      traceShowM query
      traceShowM "===END==="
      pass
