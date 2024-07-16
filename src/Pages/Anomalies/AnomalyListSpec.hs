module Pages.Anomalies.AnomalyListSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Base64 qualified as B64
import Data.Text qualified as T

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID

import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Apis.Anomalies
import Models.Projects.Projects qualified as Projects
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import RequestMessages (RequestMessage (..), replaceNullChars, toXXHash, valueToFields)

import Relude.Unsafe qualified as Unsafe

import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Anomaly List" do
    it "should return an empty list" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          length anomalies `shouldBe` 0
        _ -> error "Unexpected response"

    it "should return a list of anomalies" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      -- let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg1) -- same message
            , ("m3", reqMsg1) -- same message
            , ("m4", reqMsg1) -- same message
            ]
      res <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx

      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          let endpointAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATEndpoint) anomalies
          let shapesAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATShape) anomalies
          let formatAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATFormat) anomalies
          (length endpointAnomalies) `shouldBe` 1
          (length shapesAnomalies) `shouldBe` 0 -- All anomalies under the endpoint are not counted until the endpoint is acknowledged.
          (length formatAnomalies) `shouldBe` 0 -- Same as above
          (length anomalies) `shouldBe` 1
        -- TODO: add more tests
        _ -> error "Unexpected response"

    it "should acknowledge anomaly" \TestResources{..} -> do
      let msg1EndpointHash = toXXHash $ testPid.toText <> "172.31.29.11" <> "GET" <> "/"
      anm <- withPool trPool $ selectIssueByHash testPid msg1EndpointHash
      isJust anm `shouldBe` True
      let anmId = anm & Unsafe.fromJust & (\a -> a.id)
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.acknowlegeAnomalyGetH testPid anmId
      case pg of
        AnomalyList.Acknowlege pid anid isack -> do
          pid `shouldBe` testPid
          isack `shouldBe` True
          anid `shouldBe` anmId
        _ -> error "Unexpected response"

    it "should detect new endpoint and shape" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let reqMsg3 = Unsafe.fromJust $ convert $ msg3 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg2)
            , ("m3", reqMsg3)
            , ("m4", reqMsg2)
            ]
      res <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx

      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          let endpointAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATEndpoint) anomalies
          let shapesAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATShape) anomalies
          let formatAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATFormat) anomalies
          (length endpointAnomalies) `shouldBe` 1
          (length shapesAnomalies) `shouldBe` 1 -- reqMsg3 is same endpoint as reqMsg1 with different request body shape
          (length formatAnomalies) `shouldBe` 0 -- lower levels anomalies are ignored until the parent is acknowledge
          (length anomalies) `shouldBe` 2
        _ -> error "Unexpected response"

    it "should detect new format" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg3 = Unsafe.fromJust $ convert $ msg3 nowTxt
      let endpointHash = toXXHash $ testPid.toText <> "172.31.29.11" <> "GET" <> "/"
      let sanitizeNullChars = encodeUtf8 . replaceNullChars . decodeUtf8
          reqBodyB64 = fromRight "" $ B64.decodeBase64Untyped $ encodeUtf8 reqMsg3.requestBody
          respBodyB64 = fromRight "" $ B64.decodeBase64Untyped $ encodeUtf8 reqMsg3.responseBody

      let reqBody = fromRight (AE.object []) $ AE.eitherDecodeStrict $ sanitizeNullChars reqBodyB64
          respBody = fromRight (AE.object []) $ AE.eitherDecodeStrict $ sanitizeNullChars respBodyB64
          queryParamFields = valueToFields reqMsg3.queryParams
          respHeaderFields = valueToFields reqMsg3.responseHeaders
          reqBodyFields = valueToFields reqBody
          respBodyFields = valueToFields respBody
          queryParamsKP = V.map fst queryParamFields
          responseHeadersKP = V.map fst respHeaderFields
          requestBodyKP = V.map fst reqBodyFields
          responseBodyKP = V.map fst respBodyFields
      let combinedKeyPathStr = T.concat $ sort $ V.toList $ queryParamsKP <> responseHeadersKP <> requestBodyKP <> responseBodyKP
      let !shapeHash = endpointHash <> show reqMsg3.statusCode <> toXXHash combinedKeyPathStr

      anm <- withPool trPool $ selectIssueByHash testPid shapeHash

      isJust anm `shouldBe` True
      let anmId = anm & Unsafe.fromJust & (\a -> a.id)

      _ <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.acknowlegeAnomalyGetH testPid anmId

      let reqMsg4 = Unsafe.fromJust $ convert $ msg4 nowTxt
      let msgs =
            [("m4", reqMsg4)]
      res <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx

      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          let endpointAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATEndpoint) anomalies
          let shapesAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATShape) anomalies
          let formatAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATFormat) anomalies
          (length endpointAnomalies) `shouldBe` 1
          (length shapesAnomalies) `shouldBe` 0 -- reqMsg3 is same endpoint as reqMsg1 with different request body shape
          (length formatAnomalies) `shouldBe` 1 -- lower levels anomalies are ignored until the parent is acknowledge
          (length anomalies) `shouldBe` 2
        -- TODO: add more tests
        _ -> error "Unexpected response"

    it "should get acknowledged anomalies" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyListGetH testPid Nothing (Just "Acknowleged") Nothing Nothing Nothing Nothing Nothing Nothing
      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          let endpointAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATEndpoint) anomalies
          let shapesAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATShape) anomalies
          let formatAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATFormat) anomalies
          (length endpointAnomalies) `shouldBe` 1 -- acknowledged one endpoint anomaly
          (length shapesAnomalies) `shouldBe` 1 -- acknowledged one shape anomaly
          (length formatAnomalies) `shouldBe` 0 -- acknowledged zero format anomaly
          (length anomalies) `shouldBe` 2
        _ -> error "Unexpected response"


-- Same endpoint as msg1 but with different request body shape, to test shape anomaly detection
msg3 :: Text -> Value
msg3 timestamp =
  [aesonQQ|{"duration":476434,
            "host":"172.31.29.11",
            "method":"GET",
            "path_params":{},
            "project_id":"00000000-0000-0000-0000-000000000000",
            "proto_minor":1,
            "proto_major":1,"query_params":{},
            "raw_url":"/","referer":"","request_body":"eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOiJhZG1pbkBncm92ZXBheS5jby51ayJ9",
            "request_headers":{
              "connection":["upgrade"],"host":["172.31.29.11"],
              "x-real-ip":["172.31.81.1"],"x-forwarded-for":["172.31.81.1"],
              "user-agent":["ELB-HealthChecker/2.0"],"accept-encoding":["gzip, compressed"]},
              "response_body":"V2VsY29tZSB0byBSZXRhaWxsb29w","response_headers":{"x-powered-by":["Express"],
              "vary":["Origin"],"access-control-allow-credentials":["true"],"content-type":["text/html; charset=utf-8"],
              "content-length":["21"],"etag":["W/\"15-2rFUmgZR2gmQik/+S8kDb7KSIZk\""]
            },
            "sdk_type":"JsExpress",
            "status_code":200,
            "timestamp": #{timestamp},
            "url_path":"/","errors":[],"tags":[]} 
      |]


-- Test format detection
msg4 :: Text -> Value
msg4 timestamp =
  [aesonQQ|{"duration":476434,
            "host":"172.31.29.11",
            "method":"GET",
            "path_params":{},
            "project_id":"00000000-0000-0000-0000-000000000000",
            "proto_minor":1,
            "proto_major":1,"query_params":{},
            "raw_url":"/","referer":"","request_body":"eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOjJ9",
            "request_headers":{
              "connection":["upgrade"],"host":["172.31.29.11"],
              "x-real-ip":["172.31.81.1"],"x-forwarded-for":["172.31.81.1"],
              "user-agent":["ELB-HealthChecker/2.0"],"accept-encoding":["gzip, compressed"]},
              "response_body":"V2VsY29tZSB0byBSZXRhaWxsb29w","response_headers":{"x-powered-by":["Express"],
              "vary":["Origin"],"access-control-allow-credentials":["true"],"content-type":["text/html; charset=utf-8"],
              "content-length":["21"],"etag":["W/\"15-2rFUmgZR2gmQik/+S8kDb7KSIZk\""]
            },
            "sdk_type":"JsExpress",
            "status_code":200,
            "timestamp": #{timestamp},
            "url_path":"/","errors":[],"tags":[]} 
      |]
