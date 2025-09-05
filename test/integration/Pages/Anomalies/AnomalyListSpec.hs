{-# LANGUAGE PackageImports #-}

module Pages.Anomalies.AnomalyListSpec ({- spec -}) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.QQ (aesonQQ)
import Data.Base64.Types qualified as B64T
import "base64" Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..), query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import OddJobs.Job (Job (..))
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints.ApiCatalog qualified as ApiCatalog
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import ProcessMessage (processMessages)
import BackgroundJobs (processFiveMinuteSpans, processBackgroundJob, processOneMinuteErrors)
import BackgroundJobs qualified
import Relude
import Relude.Unsafe qualified as Unsafe
import RequestMessages (RequestMessage (..), replaceNullChars, valueToFields)
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy)
import Utils (toXXHash)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil

-- Helper function to get non-endpoint anomalies from API
getNonEndpointAnomalies :: TestResources -> IO (V.Vector AnomalyList.IssueVM)
getNonEndpointAnomalies TestResources{..} = do
  pg <- toServantResponse trATCtx trSessAndHeader trLogger $ 
    AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  case pg of
    AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> pure anomalies
    _ -> error "Unexpected response from anomaly list"


{- spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Anomaly List" do
    it "should return an empty list" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          length anomalies `shouldBe` 0
        _ -> error "Unexpected response"

    it "should create endpoint anomalies (not visible in anomaly list)" \tr@TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let msgs =
            [ ("m1", BL.toStrict $ AE.encode reqMsg1)
            , ("m2", BL.toStrict $ AE.encode reqMsg1) -- same message
            , ("m3", BL.toStrict $ AE.encode reqMsg1) -- same message
            , ("m4", BL.toStrict $ AE.encode reqMsg1) -- same message
            ]
      
      processMessagesAndBackgroundJobs tr msgs
      processAllBackgroundJobsMultipleTimes tr
      
      -- Check that endpoint was created in database
      endpoints <- withPool trPool $ DBT.query [sql|
        SELECT hash FROM apis.endpoints WHERE project_id = ?
      |] (Only testPid) :: IO (V.Vector (Only Text))
      V.length endpoints `shouldBe` 1
      
      -- Process endpoint anomaly jobs to create issues
      processEndpointAnomalyJobs tr
      _ <- runAllBackgroundJobs trATCtx
      createRequestDumps tr testPid 10
      
      -- Check that endpoint issue was created
      endpointIssues <- withPool trPool $ DBT.query [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND anomaly_type = 'endpoint'
      |] (Only testPid) :: IO (V.Vector (Only AnomalyId))
      V.length endpointIssues `shouldBe` 1
      
      -- Anomaly list API excludes endpoint anomalies
      anomalies <- getNonEndpointAnomalies tr
      -- Should have shape and other anomalies created alongside endpoint
      length anomalies `shouldBe` 1 -- All anomalies under unacknowledged endpoint are hidden

    it "should acknowledge endpoint anomaly and reveal child anomalies" \tr@TestResources{..} -> do
      let msg1EndpointHash = toXXHash $ testPid.toText <> "172.31.29.11" <> "GET" <> "/"
      anm <- withPool trPool $ selectIssueByHash testPid msg1EndpointHash
      isJust anm `shouldBe` True
      let anmId = anm & Unsafe.fromJust & (\a -> a.id)
      
      -- Process shape and field anomaly jobs to create issues
      processShapeAndFieldAnomalyJobs tr
      
      -- Acknowledge the endpoint anomaly
      pg <- toServantResponse trATCtx trSessAndHeader trLogger $ 
        AnomalyList.acknowlegeAnomalyGetH testPid anmId (Just "")
      case pg of
        AnomalyList.Acknowlege pid anid isack -> do
          pid `shouldBe` testPid
          isack `shouldBe` True
          anid `shouldBe` Issues.IssueId anmId.unAnomalyId
        _ -> error "Unexpected response"
      
      -- After acknowledging endpoint, child anomalies should be visible
      anomalies <- getNonEndpointAnomalies tr
      -- In the new Issues system, shape/field/format anomalies are all API changes
      let apiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
      
      -- There should be API change issues visible after acknowledging the endpoint
      V.length apiChangeIssues `shouldSatisfy` (> 0)

    it "should detect new shape anomaly after endpoint acknowledgment" \tr@TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let reqMsg3 = Unsafe.fromJust $ convert $ msg3 nowTxt -- Same endpoint, different shape
      let msgs =
            [ ("m1", BL.toStrict $ AE.encode reqMsg1)
            , ("m2", BL.toStrict $ AE.encode reqMsg2)
            , ("m3", BL.toStrict $ AE.encode reqMsg3)
            , ("m4", BL.toStrict $ AE.encode reqMsg2)
            ]

      processMessagesAndBackgroundJobs tr msgs
      createRequestDumps tr testPid 10
      processAllBackgroundJobsMultipleTimes tr
      
      -- Process shape and field anomaly jobs to create issues
      processShapeAndFieldAnomalyJobs tr
      
      -- Acknowledge endpoints first
      endpointIssues <- withPool trPool $ DBT.query [sql|
        SELECT id, target_hash FROM apis.issues WHERE project_id = ? AND anomaly_type = 'endpoint'
      |] (Only testPid) :: IO (V.Vector (AnomalyId, Text))
      
      forM_ endpointIssues $ \(anomalyId, _) -> do
        _ <- toServantResponse trATCtx trSessAndHeader trLogger $ 
          AnomalyList.acknowlegeAnomalyGetH testPid anomalyId (Just "")
        pass

      -- Now check anomaly list
      anomalies <- getNonEndpointAnomalies tr
      let apiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
      -- In the new Issues system, shape anomalies are part of API changes
      
      length apiChangeIssues `shouldSatisfy` (>= 1) -- At least one API change issue
      length anomalies `shouldSatisfy` (> 0)

    it "should detect new format anomaly" \tr@TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      
      -- First, process shape anomaly job to create shape issue
      processShapeAndFieldAnomalyJobs tr
      
      -- Find and acknowledge the shape anomaly
      shapeIssues <- withPool trPool $ DBT.query [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND anomaly_type = 'shape'
      |] (Only testPid) :: IO (V.Vector (Only AnomalyId))
      
      V.length shapeIssues `shouldSatisfy` (>= 1)
      
      -- Acknowledge the first shape anomaly
      case V.headM shapeIssues of
        Just (Only shapeAnmId) -> do
          _ <- toServantResponse trATCtx trSessAndHeader trLogger $ 
            AnomalyList.acknowlegeAnomalyGetH testPid shapeAnmId (Just "")
          pass
        Nothing -> error "No shape anomaly found"

      -- Now send a message with different format
      let reqMsg4 = Unsafe.fromJust $ convert $ msg4 nowTxt
      let msgs = [("m4", BL.toStrict $ AE.encode reqMsg4)]
      processMessagesAndBackgroundJobs tr msgs
      processAllBackgroundJobsMultipleTimes tr
      
      -- Process format anomaly jobs
      processFormatAnomalyJobs tr

      -- Get updated anomaly list
      anomalies <- getNonEndpointAnomalies tr
      let apiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
      
      -- In the new Issues system, format anomalies are part of API changes
      length apiChangeIssues `shouldSatisfy` (>= 1)
      length anomalies `shouldSatisfy` (> 0)

    it "should get acknowledged anomalies" \tr@TestResources{..} -> do
      pg <- toServantResponse trATCtx trSessAndHeader trLogger $ 
        AnomalyList.anomalyListGetH testPid Nothing (Just "Acknowleged") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          -- Acknowledged anomalies should include API changes (endpoint anomalies are never returned)
          let apiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
          
          -- We acknowledged at least one API change issue in the previous test
          length apiChangeIssues `shouldSatisfy` (>= 1) 
          length anomalies `shouldSatisfy` (> 0)
        _ -> error "Unexpected response"


-- Same endpoint as msg1 but with different request body shape, to test shape anomaly detection
msg3 :: Text -> AE.Value
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
msg4 :: Text -> AE.Value
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
-}
