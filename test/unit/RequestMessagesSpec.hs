{-# LANGUAGE QuasiQuotes #-}
module RequestMessagesSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.QQ
import Relude
import RequestMessages qualified
import Test.Hspec
import NeatInterpolation (text)


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
            [ -- FIXME: We can correctly handle objects in arrays.
              ("menu.popup.menuitem[*].value", [AE.String "v1", AE.String "v2"])
            , ("menu.id", [AE.String "file"])
            , ("menu.popup.menuitem[*].onclick", [AE.String "oc1", AE.String "oc2"])
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

  describe "valueToFormatStr" do
    it "should recognize date formats" do
      RequestMessages.valueToFormatStr "22/02/2022" `shouldBe` Just "{dd/mm/yyyy}"
      RequestMessages.valueToFormatStr "20-02-2022" `shouldBe` Just "{dd-mm-yyyy}"
      RequestMessages.valueToFormatStr "22.02.2022" `shouldBe` Just "{dd.mm.yyyy}"
      RequestMessages.valueToFormatStr "2023-10-14" `shouldBe` Just "{YYYY-MM-DD}"
      RequestMessages.valueToFormatStr "2023/10/14" `shouldBe` Just "{YYYY/MM/DD}"
      RequestMessages.valueToFormatStr "20231014" `shouldBe` Just "{YYYYMMDD}"
      
    it "should recognize named date formats" do
      RequestMessages.valueToFormatStr "Oct 14, 2023" `shouldBe` Just "{Mon DD, YYYY}"
      RequestMessages.valueToFormatStr "14-Oct-2023" `shouldBe` Just "{DD-Mon-YYYY}"
      RequestMessages.valueToFormatStr "2023-10-14T10:29:38.64522Z" `shouldBe` Just "{YYYY-MM-DDThh:mm:ss.sTZD}"
      RequestMessages.valueToFormatStr "2023-10-14 10:29:38" `shouldBe` Just "{YYYY-MM-DD HH:MM:SS}"
      
    it "should recognize time formats" do
      RequestMessages.valueToFormatStr "15:30:45" `shouldBe` Just "{HH:MM:SS}"
      RequestMessages.valueToFormatStr "15:30:45.123" `shouldBe` Just "{HH:MM:SS.mmm}"
      RequestMessages.valueToFormatStr "3:30 PM" `shouldBe` Just "{H:MM AM/PM}"
      RequestMessages.valueToFormatStr "Mon, 14 Oct 2023 10:30:00 +0000" `shouldBe` Just "{rfc2822}"
      
    it "should recognize UUIDs and hashes" do
      RequestMessages.valueToFormatStr "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd" `shouldBe` Just "{uuid}"
      RequestMessages.valueToFormatStr "507f1f77bcf86cd799439011" `shouldBe` Just "{uuid}"
      RequestMessages.valueToFormatStr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" `shouldBe` Just "{sha256}"
      RequestMessages.valueToFormatStr "356a192b7913b04c54574d18c28d46e6395428ab" `shouldBe` Just "{sha1}"
      RequestMessages.valueToFormatStr "5d41402abc4b2a76b9719d911017c592" `shouldBe` Just "{md5}"
      
    it "should recognize numbers" do
      RequestMessages.valueToFormatStr "123" `shouldBe` Just "{http_status}"
      RequestMessages.valueToFormatStr "123.456" `shouldBe` Just "{float}"
      RequestMessages.valueToFormatStr "-123.456" `shouldBe` Just "{float}"
      RequestMessages.valueToFormatStr "0xDEADBEEF" `shouldBe` Just "{hex}"
      RequestMessages.valueToFormatStr "1634567890" `shouldBe` Just "{epoch_s}"
      RequestMessages.valueToFormatStr "1634567890123" `shouldBe` Just "{epoch_ms}"
      
    it "should recognize network addresses" do
      RequestMessages.valueToFormatStr "192.168.1.1" `shouldBe` Just "{ipv4}"
      RequestMessages.valueToFormatStr "192.168.1.0/24" `shouldBe` Just "{cidr}"
      RequestMessages.valueToFormatStr "2001:0db8:85a3:0000:0000:8a2e:0370:7334" `shouldBe` Just "{ipv6}"
      RequestMessages.valueToFormatStr "00:1B:44:11:3A:B7" `shouldBe` Just "{mac}"
      
    it "should recognize ports and URLs" do
      RequestMessages.valueToFormatStr ":8080" `shouldBe` Just "{port}"
      RequestMessages.valueToFormatStr "user@example.com" `shouldBe` Just "{email}"
      RequestMessages.valueToFormatStr "https://example.com" `shouldBe` Just "{url}"
      RequestMessages.valueToFormatStr "http://localhost:3000" `shouldBe` Nothing -- URLs with ports not supported
      RequestMessages.valueToFormatStr "https://api.example.com/v1/users" `shouldBe` Nothing -- Complex URLs not supported
      RequestMessages.valueToFormatStr "api.example.com" `shouldBe` Just "{hostname}"
      
    it "should recognize tokens and encoding" do
      RequestMessages.valueToFormatStr "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c" `shouldBe` Just "{jwt}"
      RequestMessages.valueToFormatStr "dGVzdCBiYXNlNjQgZW5jb2Rpbmc=" `shouldBe` Just "{base64}"
      
    it "should recognize system identifiers" do
      RequestMessages.valueToFormatStr "/usr/local/bin/script.sh" `shouldBe` Just "{file_path}"
      RequestMessages.valueToFormatStr "C:\\Users\\Admin\\Documents" `shouldBe` Just "{file_path}"
      RequestMessages.valueToFormatStr "pid:12345" `shouldBe` Just "{pid}"
      RequestMessages.valueToFormatStr "tid:67890" `shouldBe` Just "{tid}"
      
    it "should recognize other identifiers" do
      RequestMessages.valueToFormatStr "Thread-42" `shouldBe` Just "{thread}"
      RequestMessages.valueToFormatStr "session_abc123def456" `shouldBe` Just "{session_id}"
      RequestMessages.valueToFormatStr "4111111111111111" `shouldBe` Just "{credit_card}"
      RequestMessages.valueToFormatStr "GB82WEST12345698765432" `shouldBe` Just "{iban}"
      
    it "should recognize personal identifiers" do
      RequestMessages.valueToFormatStr "123-45-6789" `shouldBe` Just "{ssn}"
      RequestMessages.valueToFormatStr "+1 (555) 123-4567" `shouldBe` Just "{phone}"
      RequestMessages.valueToFormatStr "200" `shouldBe` Just "{http_status}"
      RequestMessages.valueToFormatStr "404" `shouldBe` Just "{http_status}"
      RequestMessages.valueToFormatStr "500" `shouldBe` Just "{http_status}"
      
    it "should return Nothing for plain text" do
      RequestMessages.valueToFormatStr "plain text" `shouldBe` Nothing
      RequestMessages.valueToFormatStr "" `shouldBe` Nothing
      RequestMessages.valueToFormatStr "hello world" `shouldBe` Nothing
      RequestMessages.valueToFormatStr "test123test" `shouldBe` Nothing
      
    it "should handle edge cases for numbers" do
      RequestMessages.valueToFormatStr "0" `shouldBe` Just "{integer}"
      RequestMessages.valueToFormatStr "-42" `shouldBe` Nothing -- Negative integers not supported with sign
      RequestMessages.valueToFormatStr "+123" `shouldBe` Nothing -- Positive sign not supported
      RequestMessages.valueToFormatStr "0.0" `shouldBe` Just "{float}"
      RequestMessages.valueToFormatStr "-3.14159" `shouldBe` Just "{float}"
      RequestMessages.valueToFormatStr "+1.23" `shouldBe` Just "{float}"
      RequestMessages.valueToFormatStr "1e10" `shouldBe` Nothing -- Scientific notation not supported
      RequestMessages.valueToFormatStr "NaN" `shouldBe` Nothing
      RequestMessages.valueToFormatStr "Infinity" `shouldBe` Nothing
      
    it "should handle various timestamp formats" do
      RequestMessages.valueToFormatStr "1234567890" `shouldBe` Just "{epoch_s}"
      RequestMessages.valueToFormatStr "1234567890123" `shouldBe` Just "{epoch_ms}"
      RequestMessages.valueToFormatStr "2234567890" `shouldBe` Just "{integer}" -- Future timestamp
      RequestMessages.valueToFormatStr "1234567890000" `shouldBe` Just "{epoch_ms}"
      
    it "should handle edge cases for UUIDs and hashes" do
      RequestMessages.valueToFormatStr "00000000-0000-0000-0000-000000000000" `shouldBe` Just "{uuid}"
      RequestMessages.valueToFormatStr "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF" `shouldBe` Just "{uuid}"
      RequestMessages.valueToFormatStr "g073bcdcc-2669-4bf6-81d3-e4ae73fb11fd" `shouldBe` Nothing -- Invalid UUID (g)
      RequestMessages.valueToFormatStr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85" `shouldBe` Just "{base64}" -- 63 chars matches base64 pattern
      RequestMessages.valueToFormatStr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855g" `shouldBe` Just "{base64}" -- 65 chars matches base64 pattern
      
    it "should handle different IP address formats" do
      RequestMessages.valueToFormatStr "0.0.0.0" `shouldBe` Just "{ipv4}"
      RequestMessages.valueToFormatStr "255.255.255.255" `shouldBe` Just "{ipv4}"
      RequestMessages.valueToFormatStr "256.1.1.1" `shouldBe` Nothing -- Invalid IPv4
      RequestMessages.valueToFormatStr "1.1.1.1.1" `shouldBe` Nothing -- Too many octets
      RequestMessages.valueToFormatStr "::1" `shouldBe` Nothing -- IPv6 shorthand not supported
      RequestMessages.valueToFormatStr "10.0.0.0/8" `shouldBe` Just "{cidr}"
      RequestMessages.valueToFormatStr "192.168.0.1/33" `shouldBe` Nothing -- Invalid CIDR
      
    it "should handle various date edge cases" do
      RequestMessages.valueToFormatStr "00/00/0000" `shouldBe` Nothing -- Invalid date
      RequestMessages.valueToFormatStr "32/13/2023" `shouldBe` Nothing -- Invalid date
      RequestMessages.valueToFormatStr "29/02/2020" `shouldBe` Just "{dd/mm/yyyy}" -- Leap year
      RequestMessages.valueToFormatStr "31/12/1999" `shouldBe` Just "{dd/mm/yyyy}"
      RequestMessages.valueToFormatStr "01/01/2000" `shouldBe` Just "{dd/mm/yyyy}"
      RequestMessages.valueToFormatStr "2023-13-01" `shouldBe` Just "{YYYY-MM-DD}" -- Pattern matches even if invalid date
      RequestMessages.valueToFormatStr "2023-12-32" `shouldBe` Just "{YYYY-MM-DD}" -- Pattern matches even if invalid day
      
    it "should handle special characters in patterns" do
      RequestMessages.valueToFormatStr "user+tag@example.com" `shouldBe` Just "{email}"
      RequestMessages.valueToFormatStr "test.user@sub.domain.com" `shouldBe` Just "{email}"
      RequestMessages.valueToFormatStr "@example.com" `shouldBe` Nothing -- Invalid email
      RequestMessages.valueToFormatStr "user@" `shouldBe` Nothing -- Invalid email
      RequestMessages.valueToFormatStr "C:\\Program Files\\test.exe" `shouldBe` Just "{file_path}"
      RequestMessages.valueToFormatStr "/usr/local/bin/python3.9" `shouldBe` Just "{file_path}"
      RequestMessages.valueToFormatStr "/path with spaces/" `shouldBe` Nothing -- Spaces not supported
      
    it "should handle ambiguous patterns correctly" do
      RequestMessages.valueToFormatStr "12345678" `shouldBe` Just "{YYYYMMDD}" -- Could be date or integer
      RequestMessages.valueToFormatStr "20231231" `shouldBe` Just "{YYYYMMDD}" -- Valid date format
      RequestMessages.valueToFormatStr "99999999" `shouldBe` Just "{YYYYMMDD}" -- Pattern matches even if invalid date
      RequestMessages.valueToFormatStr "123" `shouldBe` Just "{http_status}" -- HTTP status takes precedence
      RequestMessages.valueToFormatStr "999" `shouldBe` Just "{integer}" -- Not valid HTTP status
      RequestMessages.valueToFormatStr "4111111111111111" `shouldBe` Just "{credit_card}" -- Visa test card
      RequestMessages.valueToFormatStr "1111111111111111" `shouldBe` Just "{hex_id}" -- Not a valid card
      
  describe "replaceAllFormats" do
    it "should replace UUIDs and hashes" do
      RequestMessages.replaceAllFormats "123" `shouldBe` "{integer}"
      RequestMessages.replaceAllFormats "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd" `shouldBe` "{uuid}"
      RequestMessages.replaceAllFormats "550e8400-e29b-41d4-a716-446655440000" `shouldBe` "{uuid}"
      RequestMessages.replaceAllFormats "507f1f77bcf86cd799439011" `shouldBe` "{uuid}"
      
    it "should replace hash formats" do
      RequestMessages.replaceAllFormats "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" `shouldBe` "{sha256}"
      RequestMessages.replaceAllFormats "356a192b7913b04c54574d18c28d46e6395428ab" `shouldBe` "{sha1}"
      RequestMessages.replaceAllFormats "5d41402abc4b2a76b9719d911017c592" `shouldBe` "{md5}"
      
    it "should replace mixed content" do
      RequestMessages.replaceAllFormats "User 123 accessed endpoint c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd" `shouldBe` "User {integer} accessed endpoint {uuid}"
      RequestMessages.replaceAllFormats "Error at 192.168.0.1:8080 with status 404" `shouldBe` "Error at {ipv4}{port} with status {integer}"
      
    it "should replace ports" do
      RequestMessages.replaceAllFormats "Server on :8080" `shouldBe` "Server on {port}"
      RequestMessages.replaceAllFormats "localhost:3000" `shouldBe` "localhost{port}"
      RequestMessages.replaceAllFormats "api.com:443" `shouldBe` "api.com{port}"
      RequestMessages.replaceAllFormats ":22" `shouldBe` "{port}"
      
    it "should replace multiple IP addresses and ports" do
      RequestMessages.replaceAllFormats "Connected to 10.0.0.1:443 and 192.168.1.100:22" `shouldBe` "Connected to {ipv4}{port} and {ipv4}{port}"
      
    it "should replace multiple integers" do
      RequestMessages.replaceAllFormats "Responses: 200, 301, 404, 500" `shouldBe` "Responses: {integer}, {integer}, {integer}, {integer}"
      
    it "should replace complex paths" do
      RequestMessages.replaceAllFormats "GET /api/v2/users/123/orders/456 returned 200" `shouldBe` "GET /api/v{integer}/users/{integer}/orders/{integer} returned {integer}"
      
    it "should replace mixed formats in single line" do
      RequestMessages.replaceAllFormats "User 550e8400-e29b-41d4-a716-446655440000 connected from 192.168.1.50:9876" `shouldBe` "User {uuid} connected from {ipv4}{port}"
      RequestMessages.replaceAllFormats "Hash: a94a8fe5ccb19ba61c4c0873d391e987982fbbd3, Status: 403, Port: :8443" `shouldBe` "Hash: {sha1}, Status: {integer}, Port: {port}"
      
    it "should handle date with port patterns" do
      RequestMessages.replaceAllFormats "Processing request 123456 at 2023-10-15:3000" `shouldBe` "Processing request {integer} at {integer}-{integer}-{integer}{port}"
      
    it "should handle various mixed patterns" do
      RequestMessages.replaceAllFormats "Mixed: 192.168.1.1 123 :80 404" `shouldBe` "Mixed: {ipv4} {integer} {port} {integer}"
      RequestMessages.replaceAllFormats "0xDEADBEEF" `shouldBe` "{hex}"
      RequestMessages.replaceAllFormats "Values: 999, 1000, 1001" `shouldBe` "Values: {integer}, {integer}, {integer}"
      
    it "should replace IP addresses" do
      RequestMessages.replaceAllFormats "10.0.0.1" `shouldBe` "{ipv4}"
      RequestMessages.replaceAllFormats "172.16.0.1" `shouldBe` "{ipv4}"
      RequestMessages.replaceAllFormats "192.168.1.1" `shouldBe` "{ipv4}"
      RequestMessages.replaceAllFormats "255.255.255.0" `shouldBe` "{ipv4}"
      
    it "should handle multiple formats in text" do
      RequestMessages.replaceAllFormats "Multiple formats: 123 abc def456789 :9000" `shouldBe` "Multiple formats: {integer} abc def{integer} {port}"
      RequestMessages.replaceAllFormats "Log entry 404 at 10.0.0.1:8080" `shouldBe` "Log entry {integer} at {ipv4}{port}"
      
    it "should handle real-world error messages" do
      RequestMessages.replaceAllFormats "Connection timeout to 192.168.1.100:3306 after 30000ms" `shouldBe` "Connection timeout to {ipv4}{port} after {integer}ms"
      RequestMessages.replaceAllFormats "Failed to connect to database at localhost:5432" `shouldBe` "Failed to connect to database at localhost{port}"
      RequestMessages.replaceAllFormats "Error: Invalid UUID c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd in request" `shouldBe` "Error: Invalid UUID {uuid} in request"
      RequestMessages.replaceAllFormats "Authentication failed for user@example.com from IP 10.0.0.5" `shouldBe` "Authentication failed for user@example.com from IP {ipv4}"
      RequestMessages.replaceAllFormats "Request ID: 507f1f77bcf86cd799439011 failed with status 500" `shouldBe` "Request ID: {uuid} failed with status {integer}"
      
    it "should handle log lines with timestamps" do
      RequestMessages.replaceAllFormats "2023-10-14 10:29:38 ERROR: Connection refused" `shouldBe` "{integer}-{integer}-{integer} {integer}{port}{port} ERROR: Connection refused"
      RequestMessages.replaceAllFormats "[2023-10-14T10:29:38.123Z] INFO: Server started on port 8080" `shouldBe` "[{integer}-{integer}-{integer}T{integer}{port}{port}.{integer}Z] INFO: Server started on port {integer}"
      RequestMessages.replaceAllFormats "Oct 14, 2023 - User 12345 logged in from 192.168.1.50" `shouldBe` "Oct {integer}, {integer} - User {integer} logged in from {ipv4}"
      
    it "should handle file paths in error messages" do
      RequestMessages.replaceAllFormats "File not found: /usr/local/app/config.json" `shouldBe` "File not found: /usr/local/app/config.json"
      RequestMessages.replaceAllFormats "Error reading C:\\Users\\Admin\\data.txt" `shouldBe` "Error reading C:\\Users\\Admin\\data.txt"
      RequestMessages.replaceAllFormats "Module failed at /app/src/main.js:42:15" `shouldBe` "Module failed at /app/src/main.js{port}{port}"
      
    it "should handle mixed identifiers" do
      RequestMessages.replaceAllFormats "Thread-42 processing session_abc123def456 for user 789" `shouldBe` "Thread-{integer} processing session_abc{integer}def{integer} for user {integer}"
      RequestMessages.replaceAllFormats "pid:1234 tid:5678 processing request" `shouldBe` "pid{port} tid{port} processing request"
      RequestMessages.replaceAllFormats "Transaction 0xDEADBEEF failed with hash a94a8fe5ccb19ba61c4c0873d391e987982fbbd3" `shouldBe` "Transaction {hex} failed with hash {sha1}"
      
    it "should handle database connection strings" do
      RequestMessages.replaceAllFormats "postgres://user:pass@localhost:5432/mydb" `shouldBe` "postgres://user:pass@localhost{port}/mydb"
      RequestMessages.replaceAllFormats "mongodb://10.0.0.1:27017/database" `shouldBe` "mongodb://{ipv4}{port}/database"
      RequestMessages.replaceAllFormats "redis://192.168.1.10:6379/0" `shouldBe` "redis://{ipv4}{port}/{integer}"
      
    it "should handle API endpoints" do
      RequestMessages.replaceAllFormats "GET /api/v2/users/123/orders/456" `shouldBe` "GET /api/v{integer}/users/{integer}/orders/{integer}"
      RequestMessages.replaceAllFormats "POST /api/users/550e8400-e29b-41d4-a716-446655440000/profile" `shouldBe` "POST /api/users/{uuid}/profile"
      RequestMessages.replaceAllFormats "DELETE /api/sessions/session_xyz789abc123" `shouldBe` "DELETE /api/sessions/session_xyz{integer}abc{integer}"
      
    it "should handle JSON-like structures" do
      RequestMessages.replaceAllFormats "{\"user_id\": 12345, \"ip\": \"192.168.1.1\", \"timestamp\": 1634567890}" `shouldBe` "{\"user_id\": {integer}, \"ip\": \"{ipv4}\", \"timestamp\": {integer}}" -- epoch_s pattern requires exact match
      RequestMessages.replaceAllFormats "data={id:c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd,port:8080}" `shouldBe` "data={id:{uuid},port{port}}" -- :8080 matches port pattern
      
    it "should handle network errors" do
      RequestMessages.replaceAllFormats "EHOSTUNREACH: 10.0.0.1:443" `shouldBe` "EHOSTUNREACH: {ipv4}{port}"
      RequestMessages.replaceAllFormats "Connection reset by peer 192.168.100.50:22" `shouldBe` "Connection reset by peer {ipv4}{port}"
      RequestMessages.replaceAllFormats "Timeout connecting to host api.example.com on port 443" `shouldBe` "Timeout connecting to host api.example.com on port {integer}"
      
    it "should handle security-related messages" do
      RequestMessages.replaceAllFormats "Invalid JWT: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U" `shouldBe` "Invalid JWT: eyJhbGciOiJIUzI{integer}NiIsInR{integer}cCI{integer}IkpXVCJ{integer}.eyJzdWIiOiIxMjM{integer}NTY{integer}ODkwIn{integer}.dozjgNryP{integer}J{integer}jVmNHl{integer}w{integer}N_XgL{integer}n{integer}I{integer}PlFUP{integer}THsR{integer}U"
      RequestMessages.replaceAllFormats "Unauthorized access from IP 10.20.30.40 to /admin" `shouldBe` "Unauthorized access from IP {ipv4} to /admin"
      RequestMessages.replaceAllFormats "Failed login attempt for user@example.com from 192.168.1.100" `shouldBe` "Failed login attempt for user@example.com from {ipv4}"
      
    it "should preserve non-matching text" do
      RequestMessages.replaceAllFormats "This is plain text with no patterns" `shouldBe` "This is plain text with no patterns"
      RequestMessages.replaceAllFormats "Error: Something went wrong!" `shouldBe` "Error: Something went wrong!"
      RequestMessages.replaceAllFormats "Hello, world! Testing 1-2-3" `shouldBe` "Hello, world! Testing {integer}-{integer}-{integer}"
      
    it "should handle edge cases in replacement" do
      RequestMessages.replaceAllFormats "" `shouldBe` ""
      RequestMessages.replaceAllFormats "123456789012345" `shouldBe` "{integer}"
      RequestMessages.replaceAllFormats "123.456.789.012" `shouldBe` "{integer}.{integer}.{integer}.{integer}"
      RequestMessages.replaceAllFormats "::::::" `shouldBe` "::::::"
      RequestMessages.replaceAllFormats "0x0x0x0x" `shouldBe` "{hex}x{hex}x"
      
    it "should handle kubernetes and docker patterns" do
      RequestMessages.replaceAllFormats "pod-abc123def-xyz789" `shouldBe` "pod-abc{integer}def-xyz{integer}"
      RequestMessages.replaceAllFormats "container-550e8400-e29b-41d4-a716-446655440000" `shouldBe` "container-{uuid}"
      RequestMessages.replaceAllFormats "node-192.168.1.100:30000" `shouldBe` "node-{ipv4}{port}"
      RequestMessages.replaceAllFormats "deployment-v1.2.3-20231014" `shouldBe` "deployment-v{integer}.{integer}.{integer}-{integer}"
      
    it "should handle cloud provider identifiers" do
      RequestMessages.replaceAllFormats "arn:aws:s3:::my-bucket/path/to/file.txt" `shouldBe` "arn:aws:s{integer}:::my-bucket/path/to/file.txt"
      RequestMessages.replaceAllFormats "i-0a1b2c3d4e5f67890" `shouldBe` "i-{integer}a{integer}b{integer}c{integer}d{integer}e{integer}f{integer}"
      RequestMessages.replaceAllFormats "subnet-12345678" `shouldBe` "subnet-{integer}"
      RequestMessages.replaceAllFormats "vpc-abc123def456" `shouldBe` "vpc-abc{integer}def{integer}"
      
    it "should handle performance metrics" do
      RequestMessages.replaceAllFormats "Response time: 123.456ms" `shouldBe` "Response time: {integer}.{integer}ms"
      RequestMessages.replaceAllFormats "CPU usage: 85.5%" `shouldBe` "CPU usage: {integer}.{integer}%"
      RequestMessages.replaceAllFormats "Memory: 4096MB/8192MB" `shouldBe` "Memory: {integer}MB/{integer}MB"
      RequestMessages.replaceAllFormats "Requests/sec: 10000" `shouldBe` "Requests/sec: {integer}"
      
    it "should handle complex nested patterns" do
      RequestMessages.replaceAllFormats "[2023-10-14T10:29:38Z] [ERROR] Connection to db-master.region.rds.amazonaws.com:5432 failed: timeout after 30000ms" `shouldBe` "[{integer}-{integer}-{integer}T{integer}{port}{port}Z] [ERROR] Connection to db-master.region.rds.amazonaws.com{port} failed: timeout after {integer}ms"
      RequestMessages.replaceAllFormats "User c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd from 192.168.1.100 accessed /api/v2/data/12345 at 1634567890" `shouldBe` "User {uuid} from {ipv4} accessed /api/v{integer}/data/{integer} at {integer}"

-- describe "requestMessageEndpoint" do
--   it "should be able to convert simple request message to series on insert db commands" do
--     recId <- UUIDV4.nextRandom
--     -- timestamp <- Time.getZonedTime
--     let timestamp = Unsafe.read "2019-08-31 05:14:37.537084021 UTC" :: UTCTime
--     let requestMsg =
--           RequestMessages.RequestMessage
--             { timestamp = utcToZonedTime utc timestamp
--             , projectId = UUID.nil
--             , sdkType = GoGin
--             , host = Just "http://apitoolkit.io"
--             , method = "POST"
--             , referer = Just $ Left "https://referer"
--             , urlPath = Just "/path/to/data"
--             , rawUrl = "/path/to/data"
--             , pathParams = [aesonQQ|{}|]
--             , queryParams = [aesonQQ|{}|]
--             , protoMajor = 1
--             , protoMinor = 1
--             , duration = 50000
--             , requestHeaders = [aesonQQ|{}|]
--             , responseHeaders = [aesonQQ|{}|]
--             , -- requestHeaders = [aesonQQ| {"Content-Type": "application/json"} |],
--               -- responseHeaders = [aesonQQ| {"X-Rand": "random-value"} |],
--               requestBody = B64.extractBase64 $ B64.encodeBase64 ""
--             , responseBody = B64.extractBase64 $ B64.encodeBase64 $ encodeUtf8 [text|{"key": "value"}|]
--             , statusCode = 203
--             , msgId = Nothing
--             , parentId = Nothing
--             , serviceVersion = Nothing
--             , errors = Nothing
--             , tags = Nothing
--             }
--     let projectCache =
--           Projects.ProjectCache
--             { hosts = []
--             , endpointHashes = ["abc"]
--             , shapeHashes = []
--             , redactFieldslist = []
--             , weeklyRequestCount = 0
--             , paymentPlan = ""
--             }
--     let result = RequestMessages.requestMsgToDumpAndEndpoint projectCache requestMsg timestamp recId
--     case result of
--       Left err -> error err
--       Right (query, params, _) -> pass

--     -- traceShowM "In request Message Endpoint test ===BEGIN==="
--     -- traceShowM query
--     -- traceShowM "===END==="
--     pass

-- describe "processMessage which fails" do
--   it "should produce failing sql" do
--     let jsonMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"e36dd90e-ddcf-4d3b-a986-4de0bddcfcbb\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"eyJQS1x1MDAwM1x1MDAwNFx1MDAxNFx1MDAwMFxiXHUwMDAwXGJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwXHUwMDFjyL1cbu+/vTBcdTAwMTBcdTAwMDDvv71PUVx1MDAwMu+/vVx1MDAwNCEo77+9VVxc77+9W++/vVx1MDAxNjvvv710O++/vUNT77+9JO+/vUvvv73vv71F77+9b1VkLu+/ve+/ve+/vVgiLCIg77+9ae+/ve+/ve+/vTvvv71cdTAwMDRt77+977+9PkQ+77+9Re+/vXrvv73vv70/77+9XHUwMDA11Lvvv73vv71QZWbvv71P77+9QFx1MDAxYu+/ve+/ve+/vVDvv73vv71kXHUwMDA2YOOsvlwi77+9XHUwMDEx77+9XHUwMDA277+9yYDPiC/vv71Z77+9IVx1MDAxMe+/vVJ6REJG77+9yqxYf1x1MDAwM1x1MDAwMFx1MDAwMO+/ve+/vVBLXHUwMDA3XGJcdTAwMDI/77+9Xe+/vVx1MDAwMFx1MDAwMFx1MDAwMO+/vVx1MDAwMFx1MDAwMFx1MDAwMFBLXHUwMDAzXHUwMDA0XHUwMDE0XHUwMDAwXGJcdTAwMDBcYlx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMCI6Ilx1MDAwMFx1MDAwMFx1MDAwMC4uLy4uLy4uLy4uL21haWxib3hkL3dlYmFwcHMvemltYnJhQWRtaW4vME1WekFlNnBnd2U1Z28xRC5qc3BcdTAwMWPIvVxu77+9MFx1MDAxMFx1MDAwMO+/vU9RXHUwMDAy77+9XHUwMDA0ISjvv71VXFzvv71b77+9XHUwMDE2O++/vXQ777+9Q1Pvv70k77+9S++/ve+/vUXvv71vVWQu77+977+977+9WCIsIiDvv71p77+977+977+9O++/vVx1MDAwNG3vv73vv70+RD7vv71F77+9eu+/ve+/vT/vv71cdTAwMDXUu++/ve+/vVBlZu+/vU/vv71AXHUwMDFi77+977+977+9UO+/ve+/vWRcdTAwMDZg46y+XCLvv71cdTAwMTHvv71cdTAwMDbvv73JgM+IL++/vVnvv70hXHUwMDEx77+9UnpEQkbvv73KrFh/XHUwMDAzXHUwMDAwXHUwMDAw77+977+9UEtcdTAwMDdcYlx1MDAwMj/vv71d77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDA9XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwLi4vLi4vLi4vLi4vbWFpbGJveGQvd2ViYXBwcy96aW1icmFBZG1pbi8wTVZ6QWU2cGd3ZTVnbzFELmpzcFBLXHUwMDA1XHUwMDA2XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyXHUwMDAwXHUwMDAyXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAxXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwIn0=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"2024-02-29T06:10:56.870Z\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"
