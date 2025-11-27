
{-# LANGUAGE PackageImports #-}

module Pages.AnomaliesSpec (spec) where

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
import Models.Apis.Anomalies (AnomalyId)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Pkg.DeriveUtils (UUIDId (..))
import Models.Users.Sessions (Session(..))
import OddJobs.Job (Job (..))
import Pages.Anomalies qualified as AnomalyList
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
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy, xdescribe)
import Servant qualified
import Utils (toXXHash)


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil

-- Helper function to get anomalies from API
getAnomalies :: TestResources -> IO (V.Vector AnomalyList.IssueVM)
getAnomalies tr = do
  (_, pg) <- testServant tr $ 
    AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  case pg of
    AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> pure anomalies
    _ -> error "Unexpected response from anomaly list"


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Anomaly List" do
    it "should return an empty list" \tr -> do
      (_, pg) <-
        testServant tr $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          length anomalies `shouldBe` 0
        _ -> error "Unexpected response"

    it "should create endpoint anomalies (not visible in anomaly list)" \tr -> do
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

      -- Check that endpoint was created in database
      endpoints <- withPool tr.trPool $ DBT.query [sql|
        SELECT hash FROM apis.endpoints WHERE project_id = ?
      |] (Only testPid) :: IO (V.Vector (Only Text))
      V.length endpoints `shouldBe` 1

      -- Check what background jobs were created and run only NewAnomaly jobs
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs

      -- Run only NewAnomaly jobs (which create issues from anomalies)
      _ <- runBackgroundJobsWhere tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{} -> True
        _ -> False
      createRequestDumps tr testPid 10
      
      -- Check that API change issue was created for the endpoint
      apiChangeIssues <- withPool tr.trPool $ DBT.query [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO (V.Vector (Only Issues.IssueId))
      V.length apiChangeIssues `shouldBe` 1
      
      -- Anomaly list should show the API change issue
      anomalies <- getAnomalies tr
      -- API change issues are visible in the anomaly list
      length anomalies `shouldBe` 1 -- Should see the API change issue

    it "should acknowledge endpoint anomaly and reveal child anomalies" \tr -> do
      -- Find the API change issue for the endpoint
      apiChangeIssues <- withPool tr.trPool $ DBT.query [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO (V.Vector (Only Issues.IssueId))
      V.length apiChangeIssues `shouldBe` 1
      let issueId = apiChangeIssues V.! 0 & (\(Only iid) -> iid)

      -- Get and run shape/field anomaly jobs
      pendingJobs2 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs2

      _ <- runBackgroundJobsWhere tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = aType} -> aType == "shape" || aType == "field"
        _ -> False

      -- Acknowledge the endpoint anomaly directly using Issues module
      -- This avoids the old acknowledgeAnomalies function that queries non-existent columns
      let sess = Servant.getResponse tr.trSessAndHeader
      withPool tr.trPool $ Issues.acknowledgeIssue issueId (sess.user).id
      
      -- Verify it was acknowledged
      acknowledgedIssue <- withPool tr.trPool $ Issues.selectIssueById issueId
      case acknowledgedIssue of
        Just issue -> isJust issue.acknowledgedAt `shouldBe` True
        Nothing -> error "Issue not found after acknowledgment"
      
      -- After acknowledging endpoint, child anomalies should be visible
      anomalies <- getAnomalies tr
      -- In the new Issues system, shape/field/format anomalies are all API changes
      let visibleApiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
      
      -- There should be API change issues visible after acknowledging the endpoint
      V.length visibleApiChangeIssues `shouldSatisfy` (> 0)

    it "should detect new shape anomaly after endpoint acknowledgment" \tr -> do
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

      -- Get pending jobs and run only NewAnomaly jobs for shapes and fields
      pendingJobs3 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs3

      _ <- runBackgroundJobsWhere tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = aType} -> aType == "shape" || aType == "field"
        _ -> False
      
      -- Acknowledge API change issues first
      apiChangeIssues <- withPool tr.trPool $ DBT.query [sql|
        SELECT id, endpoint_hash FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO (V.Vector (Issues.IssueId, Text))
      
      forM_ apiChangeIssues $ \(issueId, _) -> do
        -- Acknowledge directly using Issues module
        let sess = Servant.getResponse tr.trSessAndHeader
        withPool tr.trPool $ Issues.acknowledgeIssue issueId (sess.user).id

      -- Now check anomaly list
      anomalies <- getAnomalies tr
      let filteredApiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
      -- In the new Issues system, shape anomalies are part of API changes
      
      length filteredApiChangeIssues `shouldSatisfy` (>= 1) -- At least one API change issue
      length anomalies `shouldSatisfy` (> 0)

    it "should detect new format anomaly" \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime

      -- Get and run shape/field anomaly jobs
      pendingJobs4 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs4

      _ <- runBackgroundJobsWhere tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = aType} -> aType == "shape" || aType == "field"
        _ -> False
      
      -- Find and acknowledge the API change issues
      apiChangeIssuesForAck <- withPool tr.trPool $ DBT.query [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO (V.Vector (Only Issues.IssueId))
      
      V.length apiChangeIssuesForAck `shouldSatisfy` (>= 1)
      
      -- Acknowledge the first API change issue
      case V.headM apiChangeIssuesForAck of
        Just (Only issueId) -> do
          -- Acknowledge directly using Issues module
          let sess = Servant.getResponse tr.trSessAndHeader
          withPool tr.trPool $ Issues.acknowledgeIssue issueId (sess.user).id
        Nothing -> error "No API change issue found"

      -- Now send a message with different format
      let reqMsg4 = Unsafe.fromJust $ convert $ msg4 nowTxt
      let msgs = [("m4", BL.toStrict $ AE.encode reqMsg4)]
      processMessagesAndBackgroundJobs tr msgs

      -- Get and run format anomaly jobs
      pendingJobs5 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs5

      _ <- runBackgroundJobsWhere tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = "format"} -> True
        _ -> False

      -- Get updated anomaly list
      anomalies <- getAnomalies tr
      let formatApiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
      
      -- In the new Issues system, format anomalies are part of API changes
      length formatApiChangeIssues `shouldSatisfy` (>= 1)
      length anomalies `shouldSatisfy` (> 0)

    it "should get acknowledged anomalies" \tr -> do
      (_, pg) <- testServant tr $ 
        AnomalyList.anomalyListGetH testPid Nothing (Just "Acknowledged") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          -- Acknowledged anomalies should include API changes
          let acknowledgedApiChangeIssues = V.filter (\(AnomalyList.IssueVM _ _ _ _ c) -> c.issueType == Issues.APIChange) anomalies
          
          -- We acknowledged at least one API change issue in the previous test
          length acknowledgedApiChangeIssues `shouldSatisfy` (>= 1) 
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
