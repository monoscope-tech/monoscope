module ProcessMessage.DeterministicIdSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Time (UTCTime (..), utc, utcToZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Models.Apis.LogQueries qualified as LogQueries
import ProcessMessage (RequestMessage (..), requestEventIds)
import Relude
import Test.Hspec


request :: RequestMessage
request =
  RequestMessage
    { duration = 42
    , host = Just "api.example.com"
    , method = "GET"
    , pathParams = AE.object []
    , projectId = [uuid|00000000-0000-0000-0000-000000000001|]
    , protoMajor = 1
    , protoMinor = 1
    , queryParams = AE.object []
    , rawUrl = "/users/1"
    , referer = Nothing
    , requestBody = "e30="
    , requestHeaders = AE.object []
    , responseBody = "e30="
    , responseHeaders = AE.object []
    , sdkType = LogQueries.JsExpress
    , statusCode = 200
    , urlPath = Just "/users/:id"
    , timestamp = utcToZonedTime utc $ UTCTime (toEnum 0) 0
    , msgId = Nothing
    , parentId = Nothing
    , serviceVersion = Nothing
    , errors = Nothing
    , tags = Nothing
    }


spec :: Spec
spec = describe "legacy request event IDs" do
  it "re-derives the same span and trace IDs on retry" do
    requestEventIds request `shouldBe` requestEventIds request

  it "domain-separates span IDs from trace IDs" do
    let (spanId, traceEventId) = requestEventIds request
    UUID.toText spanId `shouldNotBe` traceEventId

  it "changes identity when event content changes" do
    requestEventIds request{rawUrl = "/users/2"} `shouldNotBe` requestEventIds request

  it "prefers the upstream event id over retry-time content drift" do
    let msgId = Just [uuid|00000000-0000-0000-0000-000000000002|]
    requestEventIds request{msgId, rawUrl = "/before"} `shouldBe` requestEventIds request{msgId, rawUrl = "/after"}

  it "does not collapse distinct upstream event ids with identical content" do
    let a = Just [uuid|00000000-0000-0000-0000-000000000002|]
        b = Just [uuid|00000000-0000-0000-0000-000000000003|]
    requestEventIds request{msgId = a} `shouldNotBe` requestEventIds request{msgId = b}

  it "canonicalizes JSON object key order" do
    let a = request{requestHeaders = AE.Object $ AEKM.fromList [("a", AE.String "1"), ("b", AE.String "2")]}
        b = request{requestHeaders = AE.Object $ AEKM.fromList [("b", AE.String "2"), ("a", AE.String "1")]}
    requestEventIds a `shouldBe` requestEventIds b
