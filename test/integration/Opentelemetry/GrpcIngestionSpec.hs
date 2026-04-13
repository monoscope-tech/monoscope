{-# LANGUAGE LambdaCase #-}

module Opentelemetry.GrpcIngestionSpec (spec) where

import Data.Time (addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Network.GRPC.Common (GrpcError (..), GrpcException (..))
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pages.Settings qualified as Api
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key (fromText)
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldContain, shouldSatisfy, shouldThrow)
import Data.List (isInfixOf, sortBy)
import Data.Set qualified as Set
import Control.Exception (ErrorCall (..), evaluate)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil

testTimeRange :: (Text, Text)
testTimeRange = (toText $ iso8601Show $ addUTCTime (-3600) frozenTime, toText $ iso8601Show $ addUTCTime 3600 frozenTime)

-- | Helper to query logs with default parameters
queryLogs :: TestResources -> Maybe Text -> IO Log.LogsGet
queryLogs tr queryM = do
  let (timeFrom, timeTo) = testTimeRange
  (_, result) <- toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger
    $ Log.apiLogH pid queryM Nothing Nothing Nothing (Just timeFrom) (Just timeTo) Nothing (Just "spans") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing Nothing
  pure result

-- | Helper to query logs with a viz_type (e.g. "sessions", "patterns") using the JSON fast path
queryLogsViz :: TestResources -> Maybe Text -> Text -> IO Log.LogsGet
queryLogsViz tr queryM vizType = queryLogsVizSorted tr queryM vizType Nothing

queryLogsVizSorted :: TestResources -> Maybe Text -> Text -> Maybe Text -> IO Log.LogsGet
queryLogsVizSorted tr queryM vizType sortByM = do
  let (timeFrom, timeTo) = testTimeRange
  (_, result) <- toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger
    $ Log.apiLogH pid queryM Nothing Nothing Nothing (Just timeFrom) (Just timeTo) Nothing (Just "spans") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") (Just vizType) Nothing Nothing Nothing sortByM
  pure result

-- | Helper to extract dataset from LogsResp
expectLogsJson :: Log.LogsGet -> IO (V.Vector (V.Vector AE.Value))
expectLogsJson = \case
  Log.LogsGetJson r -> pure r.logsData
  Log.LogPage _ -> fail "Got LogPage instead of LogsGetJson - json parameter not working?"
  Log.LogsGetError (PageCtx _ err) -> fail $ "Got LogsGetError: " <> toString err
  Log.LogsGetErrorSimple err -> fail $ "Got LogsGetErrorSimple: " <> toString err
  Log.LogsQueryLibrary{} -> fail "Got LogsQueryLibrary instead of LogsGetJson"
  Log.LogsPatternJson{} -> fail "Got LogsPatternJson instead of LogsGetJson"
  Log.LogsSessionsJson{} -> fail "Got LogsSessionsJson instead of LogsGetJson"


spec :: Spec
spec = aroundAll withTestResources do
  describe "gRPC Ingestion via Service Handlers" do
    it "Test 1.1: should create and verify 3 API keys work" $ \tr -> do
      keys <- traverse (createTestAPIKey tr pid) ["test-key-1", "test-key-2", "test-key-3"]
      forM_ keys $ \key -> ingestLog tr key "Test log" frozenTime

    it "Test 2.1: should ingest logs via all 3 API keys using logsServiceExport" $ \tr -> do
      keys <- traverse (createTestAPIKey tr pid) ["log-key-1", "log-key-2", "log-key-3"]
      forM_ (zip keys ["Log from key 1", "Log from key 2", "Log from key 3"]) $ \(key, msg) -> ingestLog tr key msg frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      result <- queryLogs tr (Just "kind == \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 3)

    it "Test 2.2: should reject log ingestion with invalid API key" $ \tr -> do
      OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createOtelLogAtTime "invalid-key-that-does-not-exist" "Should not be stored" frozenTime)
        `shouldThrow` \case GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False

    it "Test 3.1: should ingest traces via all 3 keys using traceServiceExport" $ \tr -> do
      keys <- traverse (createTestAPIKey tr pid) ["trace-key-1", "trace-key-2", "trace-key-3"]
      forM_ (zip keys ["GET /api/users", "POST /api/posts", "DELETE /api/comments"]) $ \(key, spanName) -> ingestTrace tr key spanName frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      result <- queryLogs tr (Just "kind != \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 3)

    it "Test 3.2: should reject trace ingestion with missing API key" $ \tr -> do
      traceReq <- createOtelTraceAtTime "" "Test Span" frozenTime
      OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto traceReq)
        `shouldThrow` \case GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False

    it "Test 4.1: should ingest metrics via all 3 keys using metricsServiceExport" $ \tr -> do
      keys <- traverse (createTestAPIKey tr pid) ["metric-key-1", "metric-key-2", "metric-key-3"]
      forM_ (zip3 keys ["cpu.usage", "memory.usage", "disk.usage"] [75.5, 82.3, 45.1]) $ \(key, metricName, value) -> ingestMetric tr key metricName value frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      let (timeFrom, timeTo) = testTimeRange
      result <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "metrics") []
      V.length result.dataset `shouldSatisfy` (> 0)

    it "Test 4.2: should reject metrics with invalid API key" $ \tr -> do
      OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createGaugeMetricAtTime "definitely-not-a-valid-key" "rejected.metric" 99.9 frozenTime)
        `shouldThrow` \case GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False

    it "Test 5.1: should query ingested logs via apiLogH handler" $ \tr -> do
      key <- createTestAPIKey tr pid "query-key-1"
      ingestLog tr key "Test log for querying" frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      result <- queryLogs tr (Just "kind == \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 1)

    it "Test 6.1: should query time-series data via chart_data endpoint" $ \tr -> do
      key <- createTestAPIKey tr pid "chart-key-1"
      forM_ ([0 .. 5] :: [Int]) $ \i -> ingestLog tr key "Log entry" (addUTCTime (fromIntegral (i * 60)) frozenTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      let (timeFrom, timeTo) = testTimeRange
      result <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "spans") []
      V.length result.dataset `shouldSatisfy` (> 0)
      V.toList result.headers `shouldContain` ["timestamp"]

    it "Test 7.1: should complete end-to-end integration" $ \tr -> do
      keys <- traverse (\i -> createTestAPIKey tr pid ("E2E Test Key " <> show i)) ([1, 2, 3] :: [Int])
      forM_ keys $ \key -> do
        ingestLog tr key "E2E test log" frozenTime
        ingestTrace tr key "GET /api/test" frozenTime
        ingestMetric tr key "test.metric" 42.0 frozenTime
      drainExtractionWorker tr
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      -- Verify logs and traces
      result <- queryLogs tr Nothing
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 6) -- 3 logs + 3 traces
      -- Verify metrics
      let (timeFrom, timeTo) = testTimeRange
      metricResult <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "metrics") []
      V.length metricResult.dataset `shouldSatisfy` (> 0)

    it "Test 8.1: should handle bulk ingestion (50+ messages)" $ \tr -> do
      key <- createTestAPIKey tr pid "Bulk Test Key"
      forM_ ([1 .. 50] :: [Int]) $ \i -> ingestLog tr key ("Bulk log " <> show i) frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      result <- queryLogs tr (Just "kind == \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 50)

    -- Tests for gRPC Authorization header authentication (NEW!)
    describe "gRPC Authorization Header Authentication" do
      it "Test 9.1: should authenticate logs using gRPC Authorization header" $ \tr -> do
        key <- createTestAPIKey tr pid "header-log-key"
        -- Ingest using Authorization header instead of resource attributes
        ingestLogWithHeader tr key "Log via header auth" frozenTime
        void $ runAllBackgroundJobs frozenTime tr.trATCtx
        result <- queryLogs tr (Just "kind == \"log\"")
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 1)

      it "Test 9.2: should authenticate traces using gRPC Authorization header" $ \tr -> do
        key <- createTestAPIKey tr pid "header-trace-key"
        ingestTraceWithHeader tr key "GET /api/header-auth" frozenTime
        void $ runAllBackgroundJobs frozenTime tr.trATCtx
        result <- queryLogs tr (Just "kind != \"log\"")
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 1)

      it "Test 9.3: should authenticate metrics using gRPC Authorization header" $ \tr -> do
        key <- createTestAPIKey tr pid "header-metric-key"
        ingestMetricWithHeader tr key "header.metric" 123.45 frozenTime
        void $ runAllBackgroundJobs frozenTime tr.trATCtx
        let (timeFrom, timeTo) = testTimeRange
        result <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "metrics") []
        V.length result.dataset `shouldSatisfy` (> 0)

      it "Test 9.4: should prefer resource attribute auth over header when both present" $ \tr -> do
        resourceKey <- createTestAPIKey tr pid "resource-key"
        headerKey <- createTestAPIKey tr pid "header-key"

        -- Process with both keys - resource should take precedence
        void $ runTestBg frozenTime tr $ OtlpServer.processLogsRequest (Just headerKey) (createOtelLogAtTime resourceKey "Dual auth log" frozenTime)
        void $ runAllBackgroundJobs frozenTime tr.trATCtx

        result <- queryLogs tr (Just "kind == \"log\"")
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 1)

      it "Test 9.5: should reject logs with invalid Authorization header" $ \tr -> do
        runTestBg frozenTime tr (OtlpServer.processLogsRequest (Just "invalid-header-key") (createOtelLogAtTime "" "Should not be stored" frozenTime))
          `shouldThrow` \case GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False

      it "Test 9.6: should handle mixed authentication methods in bulk" $ \tr -> do
        key <- createTestAPIKey tr pid "mixed-auth-key"
        -- Mix of resource attribute and header auth
        ingestLog tr key "Resource auth log" frozenTime
        ingestLogWithHeader tr key "Header auth log" frozenTime
        ingestTrace tr key "Resource auth trace" frozenTime
        ingestTraceWithHeader tr key "Header auth trace" frozenTime

        void $ runAllBackgroundJobs frozenTime tr.trATCtx
        result <- queryLogs tr Nothing
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 4)

    -- Guards against regressions where UUID minting is skipped at an ingestion
    -- call site after the re-mint inside `bulkInsertOtelLogsAndSpansTF` was
    -- removed (plan squishy-imagining-diffie.md §1). If a call site forgets to
    -- call `mintOtelLogIds` or passes `UUID.nil`, rows either collide or land
    -- with the nil UUID — both caught here.
    it "Test 10.1: ingested rows have distinct non-nil UUID ids (minted upstream)" $ \tr -> do
      key <- createTestAPIKey tr pid "id-mint-key"
      let names = ["GET /a", "GET /b", "GET /c", "GET /d", "GET /e"] :: [Text]
      forM_ names $ \n -> ingestTrace tr key n frozenTime
      ids <-
        withPool tr.trPool
          $ DBT.query
            [sql|
              SELECT id::text FROM otel_logs_and_spans
              WHERE project_id = ? AND name = ANY(?)
            |]
            (pid, V.fromList names)
          :: IO (V.Vector (Only Text))
      let idList = V.toList (fmap (\(Only t) -> t) ids)
      length idList `shouldBe` length names
      idList `shouldSatisfy` all (\t -> t /= UUID.toText UUID.nil)
      Set.size (Set.fromList idList) `shouldBe` length idList

    -- Sessions aggregation: verify fetchSessions groups events by session.id,
    -- surfaces user identity, counts errors, and honors KQL pre-filters.
    describe "Sessions aggregation" do
      it "Test 11.1: aggregates events by session with user identity and error count" $ \tr -> do
        key <- createTestAPIKey tr pid "sessions-key"
        let aliceAttrs =
              [ ("session.id", "abc")
              , ("user.id", "u1")
              , ("user.email", "alice@example.com")
              ]
            bobAttrs =
              [ ("session.id", "xyz")
              , ("user.id", "u2")
              , ("user.email", "bob@example.com")
              ]
        -- 3 non-error spans for alice, 1 error span for alice, 2 non-error spans for bob
        forM_ ([1 .. 3] :: [Int]) $ \i ->
          ingestSessionEvent tr key ("GET /alice/" <> show i) aliceAttrs False frozenTime
        ingestSessionEvent tr key "GET /alice/boom" aliceAttrs True frozenTime
        forM_ ([1 .. 2] :: [Int]) $ \i ->
          ingestSessionEvent tr key ("GET /bob/" <> show i) bobAttrs False frozenTime
        void $ runAllBackgroundJobs frozenTime tr.trATCtx

        result <- queryLogsViz tr Nothing "sessions"
        case result of
          Log.LogsSessionsJson total sessionRows -> do
            total `shouldBe` 2
            V.length sessionRows `shouldBe` 2
            let findBySid sid = V.find (\s -> s.sessionId == sid) sessionRows
            case findBySid "abc" of
              Just s -> do
                s.eventCount `shouldBe` 4
                s.errorCount `shouldBe` 1
                s.userEmail `shouldBe` Just "alice@example.com"
                s.userId `shouldBe` Just "u1"
              Nothing -> expectationFailure "session abc not found"
            case findBySid "xyz" of
              Just s -> do
                s.eventCount `shouldBe` 2
                s.errorCount `shouldBe` 0
                s.userEmail `shouldBe` Just "bob@example.com"
              Nothing -> expectationFailure "session xyz not found"
          _ -> expectationFailure "expected LogsSessionsJson"

      it "Test 11.2: KQL filter narrows sessions result to a single user" $ \tr -> do
        key <- createTestAPIKey tr pid "sessions-filter-key"
        -- Use unique emails to avoid collision with Test 11.1 data (aroundAll shares state)
        ingestSessionEvent tr key "GET /a" [("session.id", "s1-filter"), ("user.email", "alice-filter@example.com")] False frozenTime
        ingestSessionEvent tr key "GET /b" [("session.id", "s2-filter"), ("user.email", "bob-filter@example.com")] False frozenTime
        void $ runAllBackgroundJobs frozenTime tr.trATCtx
        result <- queryLogsViz tr (Just "attributes.user.email == \"alice-filter@example.com\"") "sessions"
        case result of
          Log.LogsSessionsJson _ sessionRows -> do
            V.length sessionRows `shouldBe` 1
            (V.head sessionRows).sessionId `shouldBe` "s1-filter"
          _ -> expectationFailure "expected LogsSessionsJson"

      it "Test 11.3: sessions with missing user identity fields still aggregate" $ \tr -> do
        key <- createTestAPIKey tr pid "sessions-anon-key"
        -- Ingest spans with session.id but NO user.id / user.email / user.name
        forM_ ([1 .. 3] :: [Int]) $ \i ->
          ingestSessionEvent tr key ("GET /anon/" <> show i) [("session.id", "anon-sess")] False frozenTime
        void $ runAllBackgroundJobs frozenTime tr.trATCtx
        result <- queryLogsViz tr Nothing "sessions"
        case result of
          Log.LogsSessionsJson _ sessionRows -> do
            case V.find (\s -> s.sessionId == "anon-sess") sessionRows of
              Just s -> do
                s.eventCount `shouldBe` 3
                s.userId `shouldBe` Nothing
                s.userEmail `shouldBe` Nothing
                s.userName `shouldBe` Nothing
              Nothing -> expectationFailure "session anon-sess not found"
          _ -> expectationFailure "expected LogsSessionsJson"

      it "Test 11.4: expand endpoint returns child events for a session" $ \tr -> do
        key <- createTestAPIKey tr pid "sessions-expand-key"
        forM_ ([1 .. 3] :: [Int]) $ \i ->
          ingestSessionEvent tr key ("GET /expand/" <> show i) [("session.id", "expand-sess"), ("user.id", "eu1")] False frozenTime
        void $ runAllBackgroundJobs frozenTime tr.trATCtx
        let (timeFrom, timeTo) = testTimeRange
        (_, expandResult) <- toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger
          $ Log.apiLogExpandH pid (Just "session") (Just "expand-sess") Nothing Nothing Nothing (Just timeFrom) (Just timeTo)
        let field k = case expandResult of AE.Object o -> KM.lookup (fromText k) o; _ -> Nothing
            rows = fromMaybe [] $ AE.decode @[AE.Value] . AE.encode =<< field "rows"
            cols = fromMaybe [] $ AE.decode @[Text] . AE.encode =<< field "cols"
            hasMore = fromMaybe False $ AE.decode @Bool . AE.encode =<< field "hasMore"
        length rows `shouldSatisfy` (>= 3)
        cols `shouldContain` ["timestamp"]
        hasMore `shouldBe` False

      it "Test 11.5: expand endpoint rejects missing key with 400" $ \tr -> do
        let (timeFrom, timeTo) = testTimeRange
        (do (h, _) <- toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger
              (Log.apiLogExpandH pid (Just "session") Nothing Nothing Nothing Nothing (Just timeFrom) (Just timeTo))
            evaluate h >> pass)
          `shouldThrow` \(ErrorCall msg) -> "400" `isInfixOf` msg

      it "Test 11.6: expand endpoint rejects invalid kind with 400" $ \tr -> do
        let (timeFrom, timeTo) = testTimeRange
        (do (h, _) <- toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger
              (Log.apiLogExpandH pid (Just "invalid") (Just "some-key") Nothing Nothing Nothing (Just timeFrom) (Just timeTo))
            evaluate h >> pass)
          `shouldThrow` \(ErrorCall msg) -> "400" `isInfixOf` msg

      it "Test 11.7: sort_by=events orders sessions by event count" $ \tr -> do
        result <- queryLogsVizSorted tr Nothing "sessions" (Just "events")
        case result of
          Log.LogsSessionsJson _ sessionRows -> do
            let counts = V.toList $ V.map (.eventCount) sessionRows
            counts `shouldBe` sortBy (flip compare) counts
          _ -> expectationFailure "expected LogsSessionsJson"
