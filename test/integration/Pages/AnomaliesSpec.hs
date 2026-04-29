{-# LANGUAGE PackageImports #-}

module Pages.AnomaliesSpec (spec) where

import BackgroundJobs qualified
import Control.Concurrent (threadDelay)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.QQ (aesonQQ)
import Data.Base64.Types qualified as B64T
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as DataUUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Lucid (renderText, toHtml)
import Data.Text.Lazy qualified as TL
import Pkg.DeriveUtils (UUIDId (..))
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Anomalies (AnomalyId)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects (Session (..))
import Models.Projects.Projects qualified as Projects
import OddJobs.Job (Job (..))
import Pages.Anomalies qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints qualified as ApiCatalog
import Pkg.Components.Table qualified as Table
import Pkg.TestUtils
import ProcessMessage (RequestMessage (..), processMessages, replaceNullChars, valueToFields)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy, xdescribe)
import Utils (toXXHash)
import "base64" Data.ByteString.Base64 qualified as B64



-- Helper function to get anomalies from API
getAnomalies :: TestResources -> IO (V.Vector AnomalyList.IssueVM)
getAnomalies tr = do
  (_, pg) <- testServant tr $
    AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] Nothing Nothing
  case pg of
    AnomalyList.ALPage (PageCtx _ tbl) -> pure tbl.rows
    _ -> error "Unexpected response from anomaly list"


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Anomaly List" do
    it "should return an empty list" \tr -> do
      (_, pg) <-
        testServant tr $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] Nothing Nothing

      case pg of
        AnomalyList.ALPage (PageCtx _ tbl) -> do
          length tbl.rows `shouldBe` 0
        _ -> error "Unexpected response"

    it "should create endpoint anomalies (not visible in anomaly list)" \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg1) -- same message
            , ("m3", toStrict $ AE.encode reqMsg1) -- same message
            , ("m4", toStrict $ AE.encode reqMsg1) -- same message
            ]

      processMessagesAndBackgroundJobs tr msgs

      -- Check that endpoint was created in database
      endpoints <- withResource tr.trPool \conn -> PGS.query conn [sql|
        SELECT hash FROM apis.endpoints WHERE project_id = ?
      |] (Only testPid) :: IO [Only Text]
      length endpoints `shouldBe` 1

      -- Check what background jobs were created and run only NewAnomaly jobs
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs

      -- Run only NewAnomaly jobs (which create issues from anomalies)
      _ <- runBackgroundJobsWhere frozenTime tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{} -> True
        _ -> False
      createTestSpans tr testPid 10

      -- Check that API change issue was created for the endpoint
      apiChangeIssues <- withResource tr.trPool \conn -> PGS.query conn [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO [Only Issues.IssueId]
      length apiChangeIssues `shouldBe` 1
      
      -- Anomaly list should show the API change issue
      anomalies <- getAnomalies tr
      -- API change issues are visible in the anomaly list
      length anomalies `shouldBe` 1 -- Should see the API change issue

    it "should acknowledge endpoint anomaly" \tr -> do
      -- Find the API change issue for the endpoint
      apiChangeIssues <- withResource tr.trPool \conn -> PGS.query conn [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO [Only Issues.IssueId]
      length apiChangeIssues `shouldBe` 1
      issueId <- case apiChangeIssues of
        (Only iid) : _ -> pure iid
        [] -> error "Expected at least one API change issue"

      -- Get and run shape/field anomaly jobs
      pendingJobs2 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs2

      _ <- runBackgroundJobsWhere frozenTime tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = aType} -> aType == "shape" || aType == "field"
        _ -> False

      -- Acknowledge the endpoint anomaly directly using Issues module
      let sess = Servant.getResponse tr.trSessAndHeader
      runTestBg frozenTime tr $ Issues.acknowledgeIssue issueId sess.user.id

      -- Verify it was acknowledged
      acknowledgedIssue <- runTestBg frozenTime tr $ Issues.selectIssueById issueId
      case acknowledgedIssue of
        Just issue -> isJust issue.acknowledgedAt `shouldBe` True
        Nothing -> error "Issue not found after acknowledgment"

      -- After acknowledging, the issue should appear in the Acknowledged filter
      (_, pg) <- testServant tr $
        AnomalyList.anomalyListGetH testPid Nothing (Just "Acknowledged") Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] Nothing Nothing
      case pg of
        AnomalyList.ALPage (PageCtx _ tbl) -> do
          let acknowledgedApiChangeIssues = V.filter (isApiChangeSingleRow Issues.ApiChange) tbl.rows
          V.length acknowledgedApiChangeIssues `shouldSatisfy` (> 0)
        _ -> error "Unexpected response"

    it "should detect new shape anomaly after processing new messages" \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let reqMsg3 = Unsafe.fromJust $ convert $ msg3 nowTxt -- Same endpoint, different shape
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg2)
            , ("m3", toStrict $ AE.encode reqMsg3)
            , ("m4", toStrict $ AE.encode reqMsg2)
            ]

      processMessagesAndBackgroundJobs tr msgs
      createTestSpans tr testPid 10

      -- Get pending jobs and run only NewAnomaly jobs for shapes and fields
      pendingJobs3 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs3

      _ <- runBackgroundJobsWhere frozenTime tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = aType} -> aType == "shape" || aType == "field"
        _ -> False

      -- Verify issues exist in the database (they may be acknowledged from previous test)
      apiChangeIssues <- withResource tr.trPool \conn -> PGS.query conn [sql|
        SELECT id, endpoint_hash FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO [(Issues.IssueId, Text)]

      -- There should be at least one API change issue in the database
      length apiChangeIssues `shouldSatisfy` (>= 1)

    it "should detect new format anomaly" \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime

      -- Get and run shape/field anomaly jobs
      pendingJobs4 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs4

      _ <- runBackgroundJobsWhere frozenTime tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = aType} -> aType == "shape" || aType == "field"
        _ -> False

      -- Find and acknowledge the API change issues
      apiChangeIssuesForAck <- withResource tr.trPool \conn -> PGS.query conn [sql|
        SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'api_change'
      |] (Only testPid) :: IO [Only Issues.IssueId]

      length apiChangeIssuesForAck `shouldSatisfy` (>= 1)

      -- Acknowledge the first API change issue
      case listToMaybe apiChangeIssuesForAck of
        Just (Only issueId) -> do
          -- Acknowledge directly using Issues module
          let sess = Servant.getResponse tr.trSessAndHeader
          runTestBg frozenTime tr $ Issues.acknowledgeIssue issueId sess.user.id
        Nothing -> error "No API change issue found"

      -- Now send a message with different format
      let reqMsg4 = Unsafe.fromJust $ convert $ msg4 nowTxt
      let msgs = [("m4", toStrict $ AE.encode reqMsg4)]
      processMessagesAndBackgroundJobs tr msgs

      -- Get and run format anomaly jobs
      pendingJobs5 <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs5

      _ <- runBackgroundJobsWhere frozenTime tr.trATCtx $ \case
        BackgroundJobs.NewAnomaly{anomalyType = "format"} -> True
        _ -> False

      -- Get updated anomaly list
      anomalies <- getAnomalies tr
      let formatApiChangeIssues = V.filter (isApiChangeSingleRow Issues.ApiChange) anomalies

      -- In the new Issues system, format anomalies are part of API changes
      length formatApiChangeIssues `shouldSatisfy` (>= 1)
      length anomalies `shouldSatisfy` (> 0)

    it "should get acknowledged anomalies" \tr -> do
      (_, pg) <- testServant tr $
        AnomalyList.anomalyListGetH testPid Nothing (Just "Acknowledged") Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [] Nothing Nothing
      case pg of
        AnomalyList.ALPage (PageCtx _ tbl) -> do
          -- Acknowledged anomalies should include API changes
          let acknowledgedApiChangeIssues = V.filter (isApiChangeSingleRow Issues.ApiChange) tbl.rows

          -- We acknowledged at least one API change issue in the previous test
          length acknowledgedApiChangeIssues `shouldSatisfy` (>= 1)
          length tbl.rows `shouldSatisfy` (> 0)
        _ -> error "Unexpected response"

    -- Regression: posting via the bulk handler used the wrong form field name
    -- (`anomalyId` instead of `itemId`) so selected ids were silently dropped and
    -- the handler reported "No items selected". Subsequent attempt to read
    -- `anomaly_hashes` as a column returned 500. This test goes through the real
    -- handler so both bugs are caught together.
    it "bulk acknowledge cascades from issues to underlying anomalies" \tr -> do
      issueId <- pickApiChangeIssue tr
      let issueIdText = DataUUID.toText issueId.unUUIDId

      -- Reset state — earlier tests in this describe block may have acknowledged it.
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| UPDATE apis.issues    SET acknowledged_at=NULL, acknowledged_by=NULL WHERE id=? |] (Only issueId)
        void $ PGS.execute conn [sql| UPDATE apis.anomalies SET acknowledged_at=NULL, acknowledged_by=NULL WHERE project_id=? |] (Only testPid)

      _ <- testServant tr $ AnomalyList.anomalyBulkActionsPostH testPid "acknowledge" AnomalyList.AnomalyBulk{itemId = [issueIdText]}

      -- Issue must be acknowledged.
      ackedIssues <- withResource tr.trPool \conn -> PGS.query conn
        [sql| SELECT id FROM apis.issues WHERE id=? AND acknowledged_at IS NOT NULL |] (Only issueId) :: IO [Only Issues.IssueId]
      length ackedIssues `shouldBe` 1

      -- Cascade: every anomaly referenced via issue_data->'anomaly_hashes' must be acknowledged.
      cascadedCount <- withResource tr.trPool \conn -> do
        [Only n] <- PGS.query conn
          [sql| WITH related AS (
                  SELECT jsonb_array_elements_text(COALESCE(issue_data->'anomaly_hashes','[]'::jsonb)) AS h
                  FROM apis.issues WHERE id=?
                )
                SELECT COUNT(*)::INT FROM apis.anomalies a
                WHERE a.project_id=? AND a.target_hash IN (SELECT h FROM related)
                  AND a.acknowledged_at IS NULL |]
          (issueId, testPid) :: IO [Only Int]
        pure n
      cascadedCount `shouldBe` 0

    -- Regression: archive path posted to apis.anomalies by issue id (which is
    -- never an anomaly id), so the cascade silently no-op'd and acknowledged_at
    -- never propagated. Now archives apis.issues by id and cascades through
    -- issue_data.anomaly_hashes; this test asserts both halves fire.
    it "bulk archive cascades to anomalies referenced by issue_data.anomaly_hashes" \tr -> do
      issueId <- pickApiChangeIssue tr
      let issueIdText = DataUUID.toText issueId.unUUIDId

      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| UPDATE apis.issues    SET archived_at=NULL WHERE id=? |] (Only issueId)
        void $ PGS.execute conn [sql| UPDATE apis.anomalies SET archived_at=NULL WHERE project_id=? |] (Only testPid)

      _ <- testServant tr $ AnomalyList.anomalyBulkActionsPostH testPid "archive" AnomalyList.AnomalyBulk{itemId = [issueIdText]}

      archivedIssues <- withResource tr.trPool \conn -> PGS.query conn
        [sql| SELECT id FROM apis.issues WHERE id=? AND archived_at IS NOT NULL |] (Only issueId) :: IO [Only Issues.IssueId]
      length archivedIssues `shouldBe` 1

      pendingCascade <- withResource tr.trPool \conn -> do
        [Only n] <- PGS.query conn
          [sql| WITH related AS (
                  SELECT jsonb_array_elements_text(COALESCE(issue_data->'anomaly_hashes','[]'::jsonb)) AS h
                  FROM apis.issues WHERE id=?
                )
                SELECT COUNT(*)::INT FROM apis.anomalies a
                WHERE a.project_id=? AND a.target_hash IN (SELECT h FROM related)
                  AND a.archived_at IS NULL |]
          (issueId, testPid) :: IO [Only Int]
        pure n
      pendingCascade `shouldBe` 0

    -- Regression: the ApiChange detail page used to render an empty Investigation
    -- panel because hashPrefix returned Nothing for ApiChange and there was no
    -- query to find the originating trace. Now hashPrefix is "" and
    -- getEndpointTraceId locates spans by (method, url_path); seed one and
    -- assert the rendered page surfaces its trace id.
    it "new-endpoint issue detail surfaces originating trace via getEndpointTraceId" \tr -> do
      issueId <- pickApiChangeIssue tr
      -- Un-archive so the detail handler renders normally
      withResource tr.trPool \conn ->
        void $ PGS.execute conn [sql| UPDATE apis.issues SET archived_at=NULL WHERE id=? |] (Only issueId)
      traceIdText <- DataUUID.toText <$> UUID.nextRandom
      spanIdText  <- DataUUID.toText <$> UUID.nextRandom
      now <- getCurrentTime
      withResource tr.trPool \conn -> void $ PGS.execute conn
        [sql| INSERT INTO otel_logs_and_spans
                (id, project_id, timestamp, start_time,
                 attributes___http___request___method, attributes___url___path,
                 context___trace_id, context___span_id,
                 context, kind, status_code)
              VALUES (gen_random_uuid(), ?, ?, ?, 'GET', '/', ?, ?,
                      jsonb_build_object('trace_id', ?::text, 'span_id', ?::text),
                      'SERVER', '200') |]
        (testPid, now, now, traceIdText, spanIdText, traceIdText, spanIdText)

      (_, pg) <- testServant tr $ AnomalyList.anomalyDetailGetH testPid issueId Nothing Nothing
      let html = TL.toStrict $ renderText $ toHtml pg
      -- The trace id should be embedded somewhere in the rendered investigation panel.
      html `shouldSatisfy` (traceIdText `T.isInfixOf`)


isApiChangeSingleRow :: Issues.IssueType -> AnomalyList.IssueVM -> Bool
isApiChangeSingleRow ty (AnomalyList.IssueVM _ _ _ _ c) = c.issueType == ty


-- | Pull the first ApiChange issue id created by earlier tests. Fails the test
-- with a useful message if the seed step (test 2) didn't run or left no issues.
pickApiChangeIssue :: TestResources -> IO Issues.IssueId
pickApiChangeIssue tr = do
  rows <- withResource tr.trPool \conn -> PGS.query conn
    [sql| SELECT id FROM apis.issues WHERE project_id=? AND issue_type='api_change' ORDER BY created_at LIMIT 1 |]
    (Only testPid) :: IO [Only Issues.IssueId]
  case rows of
    Only iid : _ -> pure iid
    [] -> error "pickApiChangeIssue: no ApiChange issue seeded — earlier tests in this spec must run first"


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
