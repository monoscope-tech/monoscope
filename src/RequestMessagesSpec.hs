module RequestMessagesSpec (spec) where

import Data.Aeson as AE
import Data.Aeson.QQ
import Data.ByteString.Base64 qualified as B64
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (utc, utcToZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Models.Apis.RequestDumps (SDKTypes (GoGin))
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Relude
import Relude.Unsafe qualified as Unsafe
import RequestMessages qualified
import Test.Hspec


spec :: Spec
spec = do
  describe "Process Messages" do
    it "value to fields" do
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
  -- it "should should process request messages" do
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

  -- it "should should process request messages even with get request and empty request body" do
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

  describe "Regex Formats Gen" do
    it "should get support string types" do
      RequestMessages.valueToFormatStr "123" `shouldBe` Just "integer"
      RequestMessages.valueToFormatStr "abc" `shouldBe` Nothing

  describe "requestMessageEndpoint" do
    it "should be able to convert simple request message to series on insert db commands" do
      recId <- UUIDV4.nextRandom
      -- timestamp <- Time.getZonedTime
      let timestamp = Unsafe.read "2019-08-31 05:14:37.537084021 UTC" :: UTCTime
      let requestMsg =
            RequestMessages.RequestMessage
              { timestamp = (utcToZonedTime utc timestamp)
              , projectId = UUID.nil
              , sdkType = GoGin
              , host = Just "http://apitoolkit.io"
              , method = "POST"
              , referer = "https://referer"
              , urlPath = Just "/path/to/data"
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
              , msgId = Nothing
              , parentId = Nothing
              , serviceVersion = Nothing
              , errors = Nothing
              , tags = Nothing
              }
      let projectCache = Projects.ProjectCache{hosts = [], endpointHashes = ["abc"], shapeHashes = [], redactFieldslist = []}
      let result = RequestMessages.requestMsgToDumpAndEndpoint projectCache requestMsg timestamp recId
      case result of
        Left err -> error err
        Right (query, params, _) -> pass

      -- traceShowM "In request Message Endpoint test ===BEGIN==="
      -- traceShowM query
      -- traceShowM "===END==="
      pass
  
  -- describe "processMessage which fails" do 
  --   it "should produce failing sql" do 
  --     let jsonMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"e36dd90e-ddcf-4d3b-a986-4de0bddcfcbb\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"eyJQS1x1MDAwM1x1MDAwNFx1MDAxNFx1MDAwMFxiXHUwMDAwXGJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwXHUwMDFjyL1cbu+/vTBcdTAwMTBcdTAwMDDvv71PUVx1MDAwMu+/vVx1MDAwNCEo77+9VVxc77+9W++/vVx1MDAxNjvvv710O++/vUNT77+9JO+/vUvvv73vv71F77+9b1VkLu+/ve+/ve+/vVgiLCIg77+9ae+/ve+/ve+/vTvvv71cdTAwMDRt77+977+9PkQ+77+9Re+/vXrvv73vv70/77+9XHUwMDA11Lvvv73vv71QZWbvv71P77+9QFx1MDAxYu+/ve+/ve+/vVDvv73vv71kXHUwMDA2YOOsvlwi77+9XHUwMDEx77+9XHUwMDA277+9yYDPiC/vv71Z77+9IVx1MDAxMe+/vVJ6REJG77+9yqxYf1x1MDAwM1x1MDAwMFx1MDAwMO+/ve+/vVBLXHUwMDA3XGJcdTAwMDI/77+9Xe+/vVx1MDAwMFx1MDAwMFx1MDAwMO+/vVx1MDAwMFx1MDAwMFx1MDAwMFBLXHUwMDAzXHUwMDA0XHUwMDE0XHUwMDAwXGJcdTAwMDBcYlx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMCI6Ilx1MDAwMFx1MDAwMFx1MDAwMC4uLy4uLy4uLy4uL21haWxib3hkL3dlYmFwcHMvemltYnJhQWRtaW4vME1WekFlNnBnd2U1Z28xRC5qc3BcdTAwMWPIvVxu77+9MFx1MDAxMFx1MDAwMO+/vU9RXHUwMDAy77+9XHUwMDA0ISjvv71VXFzvv71b77+9XHUwMDE2O++/vXQ777+9Q1Pvv70k77+9S++/ve+/vUXvv71vVWQu77+977+977+9WCIsIiDvv71p77+977+977+9O++/vVx1MDAwNG3vv73vv70+RD7vv71F77+9eu+/ve+/vT/vv71cdTAwMDXUu++/ve+/vVBlZu+/vU/vv71AXHUwMDFi77+977+977+9UO+/ve+/vWRcdTAwMDZg46y+XCLvv71cdTAwMTHvv71cdTAwMDbvv73JgM+IL++/vVnvv70hXHUwMDEx77+9UnpEQkbvv73KrFh/XHUwMDAzXHUwMDAwXHUwMDAw77+977+9UEtcdTAwMDdcYlx1MDAwMj/vv71d77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDA9XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwLi4vLi4vLi4vLi4vbWFpbGJveGQvd2ViYXBwcy96aW1icmFBZG1pbi8wTVZ6QWU2cGd3ZTVnbzFELmpzcFBLXHUwMDA1XHUwMDA2XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyXHUwMDAwXHUwMDAyXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAxXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwIn0=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"2024-02-29T06:10:56.870Z\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"

