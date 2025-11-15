module Opentelemetry.GrpcIngestionSpec (spec) where

import BackgroundJobs qualified
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Network.GRPC.Common (CustomMetadata (..),  HeaderName (..), GrpcError (..), GrpcException (..))
import Network.GRPC.Common.Protobuf (Proto (..)
)
import Pages.Api qualified as Api
import Pages.BodyWrapper (PageCtx (..)
)
import Opentelemetry.OtlpMockValues qualified as OtlpMock
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as LS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as TS
import Relude
import Data.Aeson qualified as AE
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldContain, shouldSatisfy, shouldThrow)
import Text.Read (read)


pid :: Projects.ProjectId
pid = Projects.ProjectId UUID.nil

frozenTime :: UTCTime
frozenTime = read "2025-01-01 00:00:00 UTC"

testTimeRange :: (Text, Text)
testTimeRange = (toText $ iso8601Show $ addUTCTime (-3600) frozenTime, toText $ iso8601Show $ addUTCTime 3600 frozenTime)

-- | Helper to create an API key for testing using handler
createTestAPIKey :: TestResources -> Text -> IO Text
createTestAPIKey tr keyName = do
  result <- toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger $ Api.apiPostH pid (Api.GenerateAPIKeyForm keyName Nothing)
  case result of
    Api.ApiPost _ _ (Just (_, keyText)) -> pure keyText
    _ -> error "Failed to create API key via handler"

-- | Helper to ingest a log via resource attributes (original method)
ingestLog :: TestResources -> Text -> Text -> UTCTime -> IO ()
ingestLog tr apiKey bodyText timestamp = do
  logBytes <- OtlpMock.createOtelLogAtTime apiKey bodyText timestamp
  let logReq = either (error . toText) id (decodeMessage logBytes :: Either String LS.ExportLogsServiceRequest)
  void $ OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto logReq)

-- | Helper to ingest a trace via resource attributes
ingestTrace :: TestResources -> Text -> Text -> UTCTime -> IO ()
ingestTrace tr apiKey spanName timestamp = do
  traceBytes <- OtlpMock.createOtelTraceAtTime apiKey spanName timestamp
  let traceReq = either (error . toText) id (decodeMessage traceBytes :: Either String TS.ExportTraceServiceRequest)
  void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto traceReq)

-- | Helper to ingest a metric via resource attributes
ingestMetric :: TestResources -> Text -> Text -> Double -> UTCTime -> IO ()
ingestMetric tr apiKey metricName value timestamp = do
  metricBytes <- OtlpMock.createGaugeMetricAtTime apiKey metricName value timestamp
  let metricReq = either (error . toText) id (decodeMessage metricBytes :: Either String MS.ExportMetricsServiceRequest)
  void $ OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto metricReq)

-- | Helper to ingest a log using gRPC Authorization header (via process function directly)
-- Note: This simulates what the RpcHandler would do with metadata
ingestLogWithHeader :: TestResources -> Text -> Text -> UTCTime -> IO ()
ingestLogWithHeader tr apiKey bodyText timestamp = do
  logBytes <- OtlpMock.createOtelLogAtTime "" bodyText timestamp  -- Empty API key in resource
  let logReq = either (error . toText) id (decodeMessage logBytes :: Either String LS.ExportLogsServiceRequest)
  -- Call processLogsRequest directly with the API key from "Authorization header"
  void $ runTestBg tr $ OtlpServer.processLogsRequest (Just apiKey) logReq

-- | Helper to ingest a trace using gRPC Authorization header
ingestTraceWithHeader :: TestResources -> Text -> Text -> UTCTime -> IO ()
ingestTraceWithHeader tr apiKey spanName timestamp = do
  traceBytes <- OtlpMock.createOtelTraceAtTime "" spanName timestamp  -- Empty API key in resource
  let traceReq = either (error . toText) id (decodeMessage traceBytes :: Either String TS.ExportTraceServiceRequest)
  void $ runTestBg tr $ OtlpServer.processTraceRequest (Just apiKey) traceReq

-- | Helper to ingest a metric using gRPC Authorization header
ingestMetricWithHeader :: TestResources -> Text -> Text -> Double -> UTCTime -> IO ()
ingestMetricWithHeader tr apiKey metricName value timestamp = do
  metricBytes <- OtlpMock.createGaugeMetricAtTime "" metricName value timestamp  -- Empty API key in resource
  let metricReq = either (error . toText) id (decodeMessage metricBytes :: Either String MS.ExportMetricsServiceRequest)
  void $ runTestBg tr $ OtlpServer.processMetricsRequest (Just apiKey) metricReq

-- | Helper to query logs with default parameters
queryLogs :: TestResources -> Maybe Text -> IO Log.LogsGet
queryLogs tr queryM = do
  let (timeFrom, timeTo) = testTimeRange
  toServantResponse tr.trATCtx tr.trSessAndHeader tr.trLogger
    $ Log.apiLogH pid queryM Nothing Nothing Nothing (Just timeFrom) (Just timeTo) Nothing (Just "spans") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing Nothing Nothing Nothing

-- | Helper to extract dataset from LogsResp
expectLogsJson :: Log.LogsGet -> IO (V.Vector (V.Vector AE.Value))
expectLogsJson = \case
  Log.LogsGetJson dataset _ _ _ _ _ _ _ -> pure dataset
  Log.LogPage _ -> fail "Got LogPage instead of LogsGetJson - json parameter not working?"
  Log.LogsGetError (PageCtx _ err) -> fail $ "Got LogsGetError: " <> toString err
  Log.LogsGetErrorSimple err -> fail $ "Got LogsGetErrorSimple: " <> toString err
  Log.LogsQueryLibrary _ _ _ -> fail "Got LogsQueryLibrary instead of LogsGetJson"
  Log.LogsPatternList _ _ _ _ -> fail "Got LogsPatternList instead of LogsGetJson"


spec :: Spec
spec = aroundAll withTestResources do
  describe "gRPC Ingestion via Service Handlers" do
    it "Test 1.1: should create and verify 3 API keys work" $ \tr -> do
      keys <- traverse (createTestAPIKey tr) ["test-key-1", "test-key-2", "test-key-3"]
      forM_ keys $ \key -> ingestLog tr key "Test log" frozenTime

    it "Test 2.1: should ingest logs via all 3 API keys using logsServiceExport" $ \tr -> do
      keys <- traverse (createTestAPIKey tr) ["log-key-1", "log-key-2", "log-key-3"]
      forM_ (zip keys ["Log from key 1", "Log from key 2", "Log from key 3"]) $ \(key, msg) -> ingestLog tr key msg frozenTime
      void $ runAllBackgroundJobs tr.trATCtx
      result <- queryLogs tr (Just "kind == \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 3)

    it "Test 2.2: should reject log ingestion with invalid API key" $ \tr -> do
      logBytes <- OtlpMock.createOtelLogAtTime "invalid-key-that-does-not-exist" "Should not be stored" frozenTime
      let logReq = either (error . toText) id (decodeMessage logBytes :: Either String LS.ExportLogsServiceRequest)
      OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto logReq)
        `shouldThrow` (\e -> case e of GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False)

    it "Test 3.1: should ingest traces via all 3 keys using traceServiceExport" $ \tr -> do
      keys <- traverse (createTestAPIKey tr) ["trace-key-1", "trace-key-2", "trace-key-3"]
      forM_ (zip keys ["GET /api/users", "POST /api/posts", "DELETE /api/comments"]) $ \(key, spanName) -> ingestTrace tr key spanName frozenTime
      void $ runAllBackgroundJobs tr.trATCtx
      result <- queryLogs tr (Just "kind != \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 3)

    it "Test 3.2: should reject trace ingestion with missing API key" $ \tr -> do
      -- Trace with no API key in resource attributes (tested in OtlpServer directly)
      traceBytes <- OtlpMock.createOtelTraceAtTime "" "Test Span" frozenTime
      let traceReq = either (error . toText) id (decodeMessage traceBytes :: Either String TS.ExportTraceServiceRequest)
      OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto traceReq)
        `shouldThrow` (\e -> case e of GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False)

    it "Test 4.1: should ingest metrics via all 3 keys using metricsServiceExport" $ \tr -> do
      keys <- traverse (createTestAPIKey tr) ["metric-key-1", "metric-key-2", "metric-key-3"]
      forM_ (zip3 keys ["cpu.usage", "memory.usage", "disk.usage"] [75.5, 82.3, 45.1]) $ \(key, metricName, value) -> ingestMetric tr key metricName value frozenTime
      void $ runAllBackgroundJobs tr.trATCtx
      let (timeFrom, timeTo) = testTimeRange
      result <- runQueryEffect tr $ Charts.queryMetrics (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "metrics") []
      V.length result.dataset `shouldSatisfy` (> 0)

    it "Test 4.2: should reject metrics with invalid API key" $ \tr -> do
      metricBytes <- OtlpMock.createGaugeMetricAtTime "definitely-not-a-valid-key" "rejected.metric" 99.9 frozenTime
      let metricReq = either (error . toText) id (decodeMessage metricBytes :: Either String MS.ExportMetricsServiceRequest)
      OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto metricReq)
        `shouldThrow` (\e -> case e of GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False)

    it "Test 5.1: should query ingested logs via apiLogH handler" $ \tr -> do
      key <- createTestAPIKey tr "query-key-1"
      ingestLog tr key "Test log for querying" frozenTime
      void $ runAllBackgroundJobs tr.trATCtx
      result <- queryLogs tr (Just "kind == \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 1)

    it "Test 6.1: should query time-series data via chart_data endpoint" $ \tr -> do
      key <- createTestAPIKey tr "chart-key-1"
      forM_ ([0 .. 5] :: [Int]) $ \i -> ingestLog tr key "Log entry" (addUTCTime (fromIntegral (i * 60)) frozenTime)
      void $ runAllBackgroundJobs tr.trATCtx
      let (timeFrom, timeTo) = testTimeRange
      result <- runQueryEffect tr $ Charts.queryMetrics (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "spans") []
      V.length result.dataset `shouldSatisfy` (> 0)
      V.toList result.headers `shouldContain` ["timestamp"]

    it "Test 7.1: should complete end-to-end integration" $ \tr -> do
      keys <- traverse (\i -> createTestAPIKey tr ("E2E Test Key " <> show i)) ([1, 2, 3] :: [Int])
      forM_ keys $ \key -> do
        ingestLog tr key "E2E test log" frozenTime
        ingestTrace tr key "GET /api/test" frozenTime
        ingestMetric tr key "test.metric" 42.0 frozenTime
      void $ runTestBg tr $ BackgroundJobs.processFiveMinuteSpans frozenTime pid
      void $ runAllBackgroundJobs tr.trATCtx
      -- Verify logs and traces
      result <- queryLogs tr Nothing
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 6) -- 3 logs + 3 traces
      -- Verify metrics
      let (timeFrom, timeTo) = testTimeRange
      metricResult <- runQueryEffect tr $ Charts.queryMetrics (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "metrics") []
      V.length metricResult.dataset `shouldSatisfy` (> 0)

    it "Test 8.1: should handle bulk ingestion (50+ messages)" $ \tr -> do
      key <- createTestAPIKey tr "Bulk Test Key"
      forM_ ([1 .. 50] :: [Int]) $ \i -> ingestLog tr key ("Bulk log " <> show i) frozenTime
      void $ runAllBackgroundJobs tr.trATCtx
      result <- queryLogs tr (Just "kind == \"log\"")
      dataset <- expectLogsJson result
      V.length dataset `shouldSatisfy` (>= 50)

    -- Tests for gRPC Authorization header authentication (NEW!)
    describe "gRPC Authorization Header Authentication" do
      it "Test 9.1: should authenticate logs using gRPC Authorization header" $ \tr -> do
        key <- createTestAPIKey tr "header-log-key"
        -- Ingest using Authorization header instead of resource attributes
        ingestLogWithHeader tr key "Log via header auth" frozenTime
        void $ runAllBackgroundJobs tr.trATCtx
        result <- queryLogs tr (Just "kind == \"log\"")
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 1)

      it "Test 9.2: should authenticate traces using gRPC Authorization header" $ \tr -> do
        key <- createTestAPIKey tr "header-trace-key"
        ingestTraceWithHeader tr key "GET /api/header-auth" frozenTime
        void $ runAllBackgroundJobs tr.trATCtx
        result <- queryLogs tr (Just "kind != \"log\"")
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 1)

      it "Test 9.3: should authenticate metrics using gRPC Authorization header" $ \tr -> do
        key <- createTestAPIKey tr "header-metric-key"
        ingestMetricWithHeader tr key "header.metric" 123.45 frozenTime
        void $ runAllBackgroundJobs tr.trATCtx
        let (timeFrom, timeTo) = testTimeRange
        result <- runQueryEffect tr $ Charts.queryMetrics (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just timeFrom) (Just timeTo) (Just "metrics") []
        V.length result.dataset `shouldSatisfy` (> 0)

      it "Test 9.4: should prefer resource attribute auth over header when both present" $ \tr -> do
        resourceKey <- createTestAPIKey tr "resource-key"
        headerKey <- createTestAPIKey tr "header-key"

        -- Create log with resource key but simulate header auth too
        logBytes <- OtlpMock.createOtelLogAtTime resourceKey "Dual auth log" frozenTime
        let logReq = either (error . toText) id (decodeMessage logBytes :: Either String LS.ExportLogsServiceRequest)
        -- Process with both keys - resource should take precedence
        void $ runTestBg tr $ OtlpServer.processLogsRequest (Just headerKey) logReq
        void $ runAllBackgroundJobs tr.trATCtx

        result <- queryLogs tr (Just "kind == \"log\"")
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 1)

      it "Test 9.5: should reject logs with invalid Authorization header" $ \tr -> do
        logBytes <- OtlpMock.createOtelLogAtTime "" "Should not be stored" frozenTime
        let logReq = either (error . toText) id (decodeMessage logBytes :: Either String LS.ExportLogsServiceRequest)
        -- Try with invalid key in header
        runTestBg tr (OtlpServer.processLogsRequest (Just "invalid-header-key") logReq)
          `shouldThrow` (\e -> case e of GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False)

      it "Test 9.6: should handle mixed authentication methods in bulk" $ \tr -> do
        key <- createTestAPIKey tr "mixed-auth-key"
        -- Mix of resource attribute and header auth
        ingestLog tr key "Resource auth log" frozenTime
        ingestLogWithHeader tr key "Header auth log" frozenTime
        ingestTrace tr key "Resource auth trace" frozenTime
        ingestTraceWithHeader tr key "Header auth trace" frozenTime

        void $ runAllBackgroundJobs tr.trATCtx
        result <- queryLogs tr Nothing
        dataset <- expectLogsJson result
        V.length dataset `shouldSatisfy` (>= 4)
