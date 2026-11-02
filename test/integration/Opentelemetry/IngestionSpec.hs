-- | Ingestion end to end: OTLP logs, traces and metrics (resource and header auth) → PG and
-- TimeFusion rows → explorer, chart and session reads → trace-session propagation →
-- semantic-convention normalisation and endpoint identity → SDK request messages → extraction
-- worker → the dual-write failure contract and DLQ replay → Kafka consumer polling and commits.
module Opentelemetry.IngestionSpec (spec) where

import BackgroundJobs qualified
import Control.Concurrent.STM (check, stateTVar)
import Control.Concurrent.STM.TBQueue (flushTBQueue)
import Control.Exception (ErrorCall (..), evaluate)
import Control.Lens ((%~), (.~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Sleep (sleepMicros)
import Data.HashMap.Strict qualified as HM
import Data.List (isInfixOf)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Pool (withResource)
import Data.ProtoLens (defMessage, encodeMessage)
import Data.Text qualified as T
import Data.These.Combinators (isThat)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Query, execute_)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (runErrorNoCallStack)
import Hasql.Errors qualified as HE
import Hasql.Interpolate qualified as HI
import Hasql.Pool qualified as HP
import Kafka.Consumer qualified as K
import Kafka.Effectful.Consumer.Effect (KafkaConsumer (..))
import Kafka.Effectful.Producer.Effect (KafkaProducer (..))
import Kafka.Producer qualified as KP
import Log qualified as LogBase
import Log.Backend.LogList qualified as LogList
import Log.Data (LogMessage (..))
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry hiding (BulkCol (..))
import Network.GRPC.Common (GrpcError (..), GrpcException (..))
import Network.GRPC.Common.Protobuf (Proto (..))
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pages.Replay qualified as Replay
import Pages.Telemetry qualified as TelemetryPage
import Pkg.DeriveUtils (AesonText (..), mkHasqlPool, rawSql)
import Pkg.Drain qualified as Drain
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.PatternMerge qualified as PatternMerge
import Pkg.Queue qualified as Queue
import Pkg.TestUtils
import Pkg.TraceSessionCache qualified as TSC
import ProcessMessage (PathClassifier (..), emptyPathClassifier, processMessages, processSpanToEntities, stampHashesAtIngest)
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as LSF
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as PLF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as PRF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..), EnvConfig (..))
import System.Config qualified as Config
import System.Timeout (timeout)
import System.Types (ATBackgroundCtx, runBackground)
import UnliftIO.Async (Async, race_, wait, withAsync)
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.Exception (bracket, finally, throwIO, try)
import Utils (toXXHash)


spec :: Spec
spec = do
  it "pure transforms: redaction, endpoint identity, and http.route as the framework's matched route" do
    now <- getCurrentTime
    let redacted =
          Telemetry.redactOtelSpan
            (emptySpan now)
              { Telemetry.attributes = Just $ AesonText $ Map.singleton "http.request.headers.authorization" (AE.Array $ V.singleton $ AE.String "Bearer secret")
              , Telemetry.resource = Just $ AesonText $ Map.singleton "cookie" (AE.String "session-secret")
              , Telemetry.body = Just $ AesonText $ AE.object ["password" AE..= ("secret" :: Text)]
              , Telemetry.events = Just $ AesonText $ AE.object ["set-cookie" AE..= ("session-secret" :: Text)]
              }
    (redacted.attributes >>= \(AesonText m) -> Map.lookup "http.request.headers.authorization" m) `shouldBe` Just (AE.Array $ V.singleton $ AE.String "[REDACTED]")
    (redacted.resource >>= \(AesonText m) -> Map.lookup "cookie" m) `shouldBe` Just (AE.String "[REDACTED]")
    (redacted.body <&> \(AesonText v) -> v) `shouldBe` Just (AE.object ["password" AE..= ("[REDACTED]" :: Text)])
    (redacted.events <&> \(AesonText v) -> v) `shouldBe` Just (AE.object ["set-cookie" AE..= ("[REDACTED]" :: Text)])
    -- An unparseable project_id used to be `Unsafe.fromJust . UUID.fromText`, crashing the
    -- whole batch; the owning ProjectId is threaded in typed instead.
    let entities c sp = processSpanToEntities c testPid sp
        mints c sp = isJust $ (\(mk, _, _, _) -> mk UUID.nil) (entities c sp)
        rootSpan = (emptySpan now){Telemetry.name = Just "GET /"}
    fmap (.projectId) ((\(mk, _, _, _) -> mk UUID.nil) $ entities emptyPathClassifier rootSpan{Telemetry.project_id = "not-a-uuid"}) `shouldBe` Just testPid
    -- isNewEndpoint reads knownHashes: a hash already there is not re-minted.
    hashOf <- maybe (fail "no endpoint hash stamped") pure $ (\(_, hs, _, _) -> hs V.!? 0) (entities emptyPathClassifier rootSpan)
    (mints emptyPathClassifier rootSpan, mints emptyPathClassifier{knownHashes = fromList [hashOf]} rootSpan) `shouldBe` (True, False)
    let pathOf c sp = (\(_, _, np, _) -> np) $ entities c sp
        fwHashOf c sp = (\(_, _, _, fw) -> fw) $ entities c sp
        stampOne sp = V.head $ stampHashesAtIngest (one (testPid, Projects.defaultProjectCache)) (V.singleton sp)
        attrsOf row = maybe (fail "missing stamped attributes") (\(AesonText m) -> pure m) row.attributes
        endpointAttr r inferred = Just (AE.object ["endpoint" AE..= AE.object ["route" AE..= (r :: Text), "route_inferred" AE..= inferred]])
        storesPath = "/api/stores/6a42926ba6838426a09a58c0/business-hours"
    -- A batch's route declarations stay within their own project.
    let otherPid = UUIDId $ UUID.fromWords 1 2 3 4
        declaredElsewhere = (route "/api/stores/:id/business-hours" storesPath now){Telemetry.project_id = otherPid.toText}
        stamped = stampHashesAtIngest (HM.fromList [(testPid, Projects.defaultProjectCache), (otherPid, Projects.defaultProjectCache)]) $ V.fromList [declaredElsewhere, route "" storesPath now]
    ((.hashes) <$> stamped V.!? 1) `shouldBe` Just (Just $ V.singleton $ toXXHash $ testPid.toText <> "GET" <> "/api/stores/{uuid}/business-hours")
    -- Legacy host and lower-case method are canonicalised for endpoint filters.
    legacy <- attrsOf $ stampOne (emptySpan now){Telemetry.kind = Just "client", Telemetry.attributes = Just $ AesonText $ Map.fromList [("http", AE.object ["request" AE..= AE.object ["method" AE..= ("post" :: Text)]]), ("net", AE.object ["host" AE..= AE.object ["name" AE..= ("integrations.routelift.com" :: Text)]]), ("url", AE.object ["path" AE..= ("/v1/deliveries/estimate/v2" :: Text)])]}
    (Map.lookup "server" legacy, Map.lookup "url" legacy, Map.lookup "http" legacy, Map.lookup "monoscope" legacy)
      `shouldBe` (Just (AE.object ["address" AE..= ("integrations.routelift.com" :: Text)]), Just (AE.object ["path" AE..= ("/v1/deliveries/estimate/v2" :: Text)]), Just (AE.object ["request" AE..= AE.object ["method" AE..= ("POST" :: Text), "method_original" AE..= ("post" :: Text)], "route" AE..= ("/v1/deliveries/estimate/v2" :: Text)]), endpointAttr "/v1/deliveries/estimate/v2" True)
    -- A client url.template defines the inferred route.
    let templated = stampOne (emptySpan now){Telemetry.kind = Just "client", Telemetry.attributes = Just $ AesonText $ Map.fromList [("http", AE.object ["request" AE..= AE.object ["method" AE..= ("GET" :: Text)]]), ("url", AE.object ["path" AE..= ("/orders/42" :: Text), "template" AE..= ("/orders/{id}" :: Text)])]}
    templated.hashes `shouldBe` Just (V.singleton $ toXXHash $ testPid.toText <> "GET/orders/{id}")
    templatedAttrs <- attrsOf templated
    (Map.lookup "http" templatedAttrs, Map.lookup "url" templatedAttrs, Map.lookup "monoscope" templatedAttrs)
      `shouldBe` (Just (AE.object ["request" AE..= AE.object ["method" AE..= ("GET" :: Text)], "route" AE..= ("/orders/{id}" :: Text)]), Just (AE.object ["path" AE..= ("/orders/42" :: Text), "template" AE..= ("/orders/{id}" :: Text)]), endpointAttr "/orders/{id}" True)
    -- Django's named-group regex used to be truncated at "(?P<" into 162 bogus endpoints; it
    -- becomes its own canonical template, which discovery may never merge away.
    let django = route "api/v1/wallets/(?P<pk>[^/.]+)/initiate_bank_transfer/$" "/api/v1/wallets/75261113-6245-431c-a1d0-bb45bcb9f7e4/initiate_bank_transfer/" now
    (pathOf emptyPathClassifier django, fwHashOf emptyPathClassifier django) `shouldBe` (Just "/api/v1/wallets/{pk}/initiate_bank_transfer/", Just (toXXHash $ testPid.toText <> "GET" <> "/api/v1/wallets/{pk}/initiate_bank_transfer/"))
    -- A project already holding the flattened row keeps it rather than re-minting under the framework's name.
    let flattened = route "/v1/rates/:id/history" "/v1/rates/8821/history" now
        known = emptyPathClassifier{knownHashes = fromList [toXXHash $ testPid.toText <> "GET" <> "/v1/rates/{param}/history"]}
    (pathOf known flattened, fwHashOf known flattened) `shouldBe` (Just "/v1/rates/{param}/history", Nothing)
    -- Old SDKs sent concrete URLs as http.route: not authority, so id heuristics still run.
    let concrete = route "/users/550e8400-e29b-41d4-a716-446655440000" "/users/550e8400-e29b-41d4-a716-446655440000" now
    (pathOf emptyPathClassifier concrete, fwHashOf emptyPathClassifier concrete) `shouldBe` (Just "/users/{uuid}", Nothing)
    -- A catch-all with no literal segment would swallow every path of its length.
    fwHashOf emptyPathClassifier (route "/{path}" "/anything" now) `shouldBe` Nothing

  it "pure transforms: drain batch membership and trace-session cache eviction" do
    let batch = V.fromList [("first", "Connected to database primary"), ("second", "Connected to database replica"), ("third", "Connected to database <*> <*>")]
        build = Drain.buildDrainBatch (V.fromList . T.words . snd) fst (Just . snd)
        (tree, patterns) = build Drain.emptyDrainTree batch frozenTime
        merged = PatternMerge.mergeByJaccard PatternMerge.jaccardMergeThreshold patterns
        members = concatMap (V.toList . (.logIds)) . V.toList
        frequency = sum . map (.frequency) . V.toList
    (V.length merged, frequency merged) `shouldBe` (1, 3)
    members merged `shouldMatchList` ["first", "second", "third"]
    V.all (V.null . (.logIds)) (Drain.getAllLogGroups tree) `shouldBe` True
    frequency (snd $ build tree (V.singleton ("fourth", "Connected to database backup")) frozenTime) `shouldBe` 1
    let evictionBatch = V.fromList [(show n, "event a" <> show n <> " b" <> show n <> " c" <> show n) | n <- [1 .. 1002 :: Int]]
        (bounded, evicted) = build Drain.emptyDrainTree evictionBatch frozenTime
        repeated = V.fromList [(show n, "same event") | n <- [1 .. 600 :: Int]]
    V.length (Drain.getAllLogGroups bounded) `shouldBe` 1000
    members evicted `shouldMatchList` V.toList (V.map fst evictionBatch)
    members (snd $ build Drain.emptyDrainTree repeated frozenTime) `shouldMatchList` V.toList (V.map fst repeated)
    -- 300s idle evicts t0; then an LRU cap of 1 keeps only the newest.
    cache <- TSC.newTraceSessionCache
    let info t = TSC.TraceSessionInfo (Just "s") Nothing Nothing Nothing Nothing t
        now = addUTCTime 400 frozenTime
    atomicModifyIORef' cache $ const (HM.fromList [(("p", "tr" <> show i), info (addUTCTime (100 * fromIntegral i) frozenTime)) | i <- [0 .. 2 :: Int]], ())
    TSC.evictStaleEntries cache 300 10 now `shouldReturn` 1
    sort . HM.keys <$> readIORef cache `shouldReturn` [("p", "tr1"), ("p", "tr2")]
    TSC.evictStaleEntries cache 300 1 now `shouldReturn` 1
    HM.keys <$> readIORef cache `shouldReturn` [("p", "tr2")]

  around withTestResources do
    it "logs and traces: resource and header auth across keys, invalid keys rejected, minted ids, explorer and chart reads, bulk insert" \tr -> do
      pid <- createTestProject tr "otlp-ingest"
      other <- createTestProject tr "otlp-other"
      keys@(k1 : k2 : _) <- traverse (createTestAPIKey tr pid) ["key-1", "key-2", "key-3"]
      otherKey <- createTestAPIKey tr other "other-key"
      for_ (zip3 keys ["Log from key 1", "Log from key 2", "Log from key 3"] ["GET /api/users", "POST /api/posts", "DELETE /api/comments"]) \(key, body, name) ->
        ingestLog tr key body frozenTime >> ingestTrace tr key name frozenTime
      -- Header-only records carry no resource key; they land in the header key's project.
      ingestLogWithHeader tr k1 "Log via header auth" frozenTime
      ingestTraceWithHeader tr k1 "GET /api/header-auth" frozenTime
      -- Resource and header keys are both resolved; the record follows its resource key.
      runTestBg frozenTime tr $ OtlpServer.processLogsRequest (Just otherKey) (createOtelLogAtTime k2 ["Dual auth log"] frozenTime)
      for_ ([0 .. 5] :: [Int]) \i -> ingestLog tr k1 "Log entry" (addUTCTime (fromIntegral (i * 60)) frozenTime)
      let unauthenticated = \case GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False
      OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createOtelLogAtTime "invalid-key-that-does-not-exist" ["Should not be stored"] frozenTime) `shouldThrow` unauthenticated
      traceReq <- createOtelTraceAtTime "" "Test Span" frozenTime
      OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto traceReq) `shouldThrow` unauthenticated
      OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createGaugeMetricAtTime "definitely-not-a-valid-key" [] [] "rejected.metric" 99.9 frozenTime) `shouldThrow` unauthenticated
      runTestBg frozenTime tr (OtlpServer.processLogsRequest (Just "invalid-header-key") (createOtelLogAtTime "" ["Should not be stored"] frozenTime)) `shouldThrow` unauthenticated
      drainExtractionWorker tr
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      logRows <- queryLogs tr pid (Just "kind == \"log\"")
      traces <- queryLogs tr pid (Just "kind != \"log\"")
      everything <- queryLogs tr pid Nothing
      (logRows.error, V.length logRows.logsData, V.length traces.logsData, V.length everything.logsData) `shouldBe` (Nothing, 11, 4, 15)
      dbOne tr [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND kind = 'log' AND level IS NOT NULL AND status_code IS DISTINCT FROM level |] (Only pid) `shouldReturn` (0 :: Int)
      forM [(p, b) | p <- [pid, other], b <- ["%Dual auth log%", "%Log via header auth%" :: Text]] (dbOne tr [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND body::text LIKE ? |]) `shouldReturn` [1 :: Int, 1, 0, 0]
      -- Ids are minted upstream of bulkInsertOtelLogsAndSpansTF: a call site that skips
      -- mintOtelLogIds collides distinct rows or lands the nil UUID. (Repeated log content
      -- shares an id by design; timestamp is the other dedup key.)
      ids <- map fromOnly <$> dbQuery @_ @(Only Text) tr [sql| SELECT id::text FROM otel_logs_and_spans WHERE project_id = ? AND body::text IS DISTINCT FROM '"Log entry"' |] (Only pid)
      (length ids, length (ordNub ids), UUID.toText UUID.nil `elem` ids) `shouldBe` (9, 9, False)
      -- A backend query failure surfaces as LogResult.error, not an empty "no events" table.
      failed <- queryLogs tr pid (Just "notarealcolumn_xyz == \"x\"")
      failed.error `shouldSatisfy` isJust
      chart <- metricsQuery tr pid (Just Charts.DTMetric) "spans" "summarize count(*) by bin_auto(timestamp)"
      (V.null chart.dataset, "timestamp" `elem` chart.headers) `shouldBe` (False, True)
      -- The whole bulk request goes in one column-oriented INSERT…SELECT…unnest.
      void $ OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createOtelLogAtTime k1 ["bulk-8.1/" <> show i | i <- [1 .. 2000 :: Int]] frozenTime)
      dbOne tr [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND body::text LIKE '%bulk-8.1/%' |] (Only pid) `shouldReturn` (2000 :: Int)

    it "metrics: gauges, histograms, exponential histograms and summaries keep native columns; catalog and pages; replays dedupe; overflow poisoned; every write target idempotent" \tr -> do
      -- TimeFusion is one shared store, so every count here is over a project of this example's own.
      mpid <- createTestProject tr "otlp-metrics"
      keys@(key : _) <- traverse (createTestAPIKey tr mpid) ["metric-key-1", "metric-key-2", "metric-key-3", "metric-key-4"]
      for_ (zip3 keys ["cpu.usage", "cpu.limit", "memory.usage", "disk.usage"] [75.5, 82.3, 45.1, 10]) \(k, name, value) -> ingestMetric tr k [] [] name value frozenTime
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      -- Catalog writes are buffered and flushed at a threshold or hourly, not by background jobs.
      runTestBg frozenTime tr $ Telemetry.flushMetricCatalog tr.trATCtx.metricCatalogBuffer
      counted <- metricsQuery tr mpid (Just Charts.DTMetric) "metrics" "metrics | summarize count(*) by bin_auto(timestamp)"
      V.null counted.dataset `shouldBe` False
      tf tr [HI.sql| SELECT count(*)::bigint FROM otel_metrics WHERE project_id = #{mpid.toText} |] `shouldReturn` V.singleton (4 :: Int)
      dbOne tr [sql| SELECT count(*)::int FROM otel_metrics_meta WHERE project_id = ? |] (Only mpid) `shouldReturn` (4 :: Int)
      dbOne tr [sql| SELECT array_to_string(metric_labels, ',') FROM otel_metrics_meta WHERE project_id = ? AND metric_name = 'cpu.usage' |] (Only mpid) >>= (`shouldSatisfy` T.isInfixOf "resource.service.name")
      -- Metric cards query the native scalar column, not the removed telemetry.metrics JSON payload.
      overview <- renderT <$> call tr (TelemetryPage.metricsOverViewGetH mpid (Just "charts") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
      overview `shouldContainAll` ["metrics | where metric_name"]
      breakdown <- renderT <$> call tr (TelemetryPage.metricBreakdownGetH mpid "cpu.usage" (Just "resource.service.name"))
      breakdown `shouldContainAll` ["summarize avg(value) by bin_auto(timestamp),resource.service.name"]
      details <- renderT <$> call tr (TelemetryPage.metricDetailsGetH mpid "cpu.usage" Nothing Nothing Nothing Nothing Nothing)
      details `shouldContainAll` ["Available dimensions", "Reporting services", "resource.service.name", "Group chart by resource.service.name", "id=\"metric-related-content\""]
      -- Related metrics need the whole catalogue, so the tab defers them to metricRelatedGetH.
      for_ ([">Metric detail<", "Metric attributes", "Same cpu namespace"] :: [Text]) \gone -> details `shouldNotSatisfy` T.isInfixOf gone
      related <- renderT <$> call tr (TelemetryPage.metricRelatedGetH mpid "cpu.usage" Nothing)
      related `shouldContainAll` ["cpu.limit", "Same cpu namespace"]
      -- The datapoints link carries the cursor too, so a reload from a deep page returns there.
      -- With no time params the tab once ran an unbounded COUNT over the whole history (the
      -- shape that OOM-killed TimeFusion): it now shows its default window and counts need one.
      dataPoints <- renderT <$> call tr (TelemetryPage.metricsOverViewGetH mpid (Just "datapoints") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
      dataPoints `shouldContainAll` ["&amp;expand=cpu.usage", "tab=datapoints", "Last hour"]
      points <- runTestBg frozenTime tr $ Telemetry.getDataPointsData (Telemetry.CatalogWithCounts True (addUTCTime (-1) frozenTime, addUTCTime 1 frozenTime)) mpid
      find ((== "cpu.usage") . (.metricName)) points `shouldSatisfy` isJust
      totals <- runTestBg frozenTime tr $ Telemetry.getUsageTotals True mpid (addUTCTime (-1) frozenTime) (addUTCTime 1 frozenTime)
      totals.metrics `shouldSatisfy` (>= 4)
      let export resourceMetrics = void $ OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ defMessage & MSF.resourceMetrics .~ [resourceMetrics])
          resourceWith metric = defMessage & PMF.resource .~ mkResource key [] & PMF.scopeMetrics .~ [defMessage & PMF.metrics .~ [metric]]
          countOf :: Text -> IO (V.Vector Int)
          countOf name = tf tr [HI.sql| SELECT count(*)::bigint FROM otel_metrics WHERE project_id = #{mpid.toText} AND metric_name = #{name} |]
      ingestMetricWithHeader tr key [] [] "header.metric" 123.45 frozenTime
      countOf "header.metric" `shouldReturn` V.singleton 1
      -- Replayed exports dedupe on the deterministic raw-point id.
      replicateM_ 2 $ ingestMetric tr key [] [] "replayed.metric" 42 frozenTime
      countOf "replayed.metric" `shouldReturn` V.singleton 1
      let start = addUTCTime (-30) frozenTime
          gaugePoint = defMessage & PMF.timeUnixNano .~ toNanos frozenTime & PMF.startTimeUnixNano .~ toNanos start & PMF.asDouble .~ 42
      export $ defMessage & PMF.resource .~ (mkResource key [] & PRF.droppedAttributesCount .~ 7) & PMF.schemaUrl .~ "https://example.test/resource/1" & PMF.scopeMetrics .~ [defMessage & PMF.schemaUrl .~ "https://example.test/scope/1" & PMF.metrics .~ [defMessage & PMF.name .~ "metadata.gauge" & PMF.unit .~ "1" & PMF.gauge .~ (defMessage & PMF.dataPoints .~ [gaugePoint])]]
      tf tr [HI.sql| SELECT start_timestamp, resource_schema_url, scope_schema_url, dropped_attributes_count FROM otel_metrics WHERE project_id = #{mpid.toText} AND metric_name = 'metadata.gauge' |]
        `shouldReturn` V.singleton (start, "https://example.test/resource/1" :: Text, "https://example.test/scope/1" :: Text, 7 :: Int)
      export $ resourceWith $ defMessage & PMF.name .~ "request.duration" & PMF.histogram .~ (defMessage & PMF.dataPoints .~ [defMessage & PMF.timeUnixNano .~ toNanos frozenTime & PMF.count .~ 3 & PMF.bucketCounts .~ [0, 2, 1] & PMF.explicitBounds .~ [10, 20]])
      map parseNumArray . V.toList <$> tf tr [HI.sql| SELECT hist_bucket_counts::text FROM otel_metrics WHERE project_id = #{mpid.toText} AND metric_name = 'request.duration' |] `shouldReturn` [[0, 2, 1]]
      let exemplar = defMessage & PMF.timeUnixNano .~ toNanos frozenTime & PMF.asDouble .~ (5.5 :: Double) :: PM.Exemplar
          histPoint = defMessage & PMF.timeUnixNano .~ toNanos frozenTime & PMF.count .~ 7 & PMF.sum .~ 12.5 & PMF.min .~ 0.5 & PMF.max .~ 9.0 & PMF.bucketCounts .~ [1, 4, 2] & PMF.explicitBounds .~ [10, 20] & PMF.exemplars .~ [exemplar] :: PM.HistogramDataPoint
      export $ resourceWith $ defMessage & PMF.name .~ "request.duration.full" & PMF.histogram .~ (defMessage & PMF.aggregationTemporality .~ PM.AGGREGATION_TEMPORALITY_CUMULATIVE & PMF.dataPoints .~ [histPoint])
      runTestBg frozenTime tr $ Telemetry.flushMetricCatalog tr.trATCtx.metricCatalogBuffer
      renderT <$> call tr (TelemetryPage.metricDetailsGetH mpid "request.duration.full" Nothing Nothing Nothing Nothing Nothing) >>= (`shouldContainAll` ["sum(distribution_sum) / sum(distribution_count)"])
      renderT <$> call tr (TelemetryPage.metricCardGetH mpid "request.duration.full" Nothing Nothing) >>= (`shouldContainAll` ["request.duration.full · mean"])
      mean <- metricsQuery tr mpid (Just Charts.DTMetric) "metrics" "metrics | where metric_name == \"request.duration.full\" and distribution_count > 0 and distribution_sum != null | summarize sum(distribution_sum) / sum(distribution_count) by bin_auto(timestamp)"
      (mean.error, V.null mean.dataset) `shouldBe` (Nothing, False)
      hist <- tf tr [HI.sql| SELECT distribution_count, distribution_sum, distribution_min, distribution_max, hist_explicit_bounds::text, aggregation_temporality FROM otel_metrics WHERE project_id = #{mpid.toText} AND metric_name = 'request.duration.full' |]
      (\(c, s, mn, mx, bounds, temporality) -> (c, s, mn, mx, parseNumArray bounds, temporality)) <$> V.toList hist `shouldBe` [(7 :: Int64, 12.5 :: Double, 0.5 :: Double, 9.0 :: Double, [10, 20], "CUMULATIVE" :: Text)]
      -- exemplars is jsonb, which real TimeFusion's pgwire reports as jsonb even under ::text.
      exemplars <- tf tr [HI.sql| SELECT exemplars FROM otel_metrics WHERE project_id = #{mpid.toText} AND metric_name = 'request.duration.full' |]
      (decodeUtf8 . AE.encode <$> V.toList (exemplars :: V.Vector AE.Value)) `shouldSatisfy` any (T.isInfixOf "5.5")
      let buckets off counts = defMessage & PMF.offset .~ off & PMF.bucketCounts .~ counts :: PM.ExponentialHistogramDataPoint'Buckets
          expPoint = defMessage & PMF.timeUnixNano .~ toNanos frozenTime & PMF.count .~ 20 & PMF.sum .~ 55.0 & PMF.min .~ 0.1 & PMF.max .~ 12.0 & PMF.zeroCount .~ 3 & PMF.scale .~ 4 & PMF.zeroThreshold .~ 0.001 & PMF.positive .~ buckets 2 [1, 3, 5] & PMF.negative .~ buckets (-1) [2, 4] :: PM.ExponentialHistogramDataPoint
      export $ resourceWith $ defMessage & PMF.name .~ "request.size.exp" & PMF.exponentialHistogram .~ (defMessage & PMF.aggregationTemporality .~ PM.AGGREGATION_TEMPORALITY_DELTA & PMF.dataPoints .~ [expPoint])
      -- The int4 columns are cast to bigint so hasql's Int64 decoder matches the column OID.
      expRows <- tf tr [HI.sql| SELECT distribution_count, distribution_sum, exp_hist_scale::bigint, exp_hist_zero_count, exp_hist_zero_threshold, exp_hist_pos_offset::bigint, exp_hist_pos_buckets::text, exp_hist_neg_offset::bigint, exp_hist_neg_buckets::text, aggregation_temporality FROM otel_metrics WHERE project_id = #{mpid.toText} AND metric_name = 'request.size.exp' |]
      (\(c, s, scale, zc, zt, posOff, posB, negOff, negB, temporality) -> ((c, s, scale, zc, zt, posOff, negOff, temporality), parseNumArray posB, parseNumArray negB)) <$> V.toList expRows
        `shouldBe` [((20 :: Int64, 55.0 :: Double, 4 :: Int64, 3 :: Int64, 0.001 :: Double, 2 :: Int64, -1 :: Int64, "DELTA" :: Text), [1, 3, 5], [2, 4])]
      let quantile q v = defMessage & PMF.quantile .~ q & PMF.value .~ v :: PM.SummaryDataPoint'ValueAtQuantile
      export $ resourceWith $ defMessage & PMF.name .~ "request.latency.summary" & PMF.summary .~ (defMessage & PMF.dataPoints .~ [defMessage & PMF.timeUnixNano .~ toNanos frozenTime & PMF.count .~ 12 & PMF.sum .~ 640.0 & PMF.quantileValues .~ [quantile 0.5 100, quantile 0.99 250]])
      summaries <- tf tr [HI.sql| SELECT distribution_count, distribution_sum, summary_quantiles::text, summary_values::text FROM otel_metrics WHERE project_id = #{mpid.toText} AND metric_name = 'request.latency.summary' |]
      (\(c, s, qs, vs) -> (c, s, parseNumArray qs, parseNumArray vs)) <$> V.toList summaries `shouldBe` [(12 :: Int64, 640.0 :: Double, [0.5, 0.99], [100, 250])]
      -- The metric read path sums the ingested points (data_type omitted: the CLI --assert path).
      ingestMetric tr key [] [] "tf.read.match" 10 frozenTime
      ingestMetric tr key [] [] "tf.read.match" 32 (addUTCTime 1 frozenTime)
      scalar <- metricsQuery tr mpid Nothing "metrics" "metrics | where metric_name == \"tf.read.match\" | summarize sum(value)"
      (scalar.error, scalar.dataFloat) `shouldBe` (Nothing, Just 42)
      let metricsCeType = HM.singleton "ce-type" "org.opentelemetry.otlp.metrics.v1"
          processMetrics msgs attrs = runTestBg frozenTime tr $ OtlpServer.processList msgs attrs
      -- A uint64 count beyond BIGINT is poisoned, never a write failure.
      let overflowPoint = defMessage & PMF.timeUnixNano .~ toNanos frozenTime & PMF.count .~ (maxBound :: Word64) :: PM.HistogramDataPoint
          overflow = defMessage & MSF.resourceMetrics .~ [resourceWith $ defMessage & PMF.name .~ "overflow.histogram" & PMF.histogram .~ (defMessage & PMF.dataPoints .~ [overflowPoint])] :: MS.ExportMetricsServiceRequest
      processMetrics [("overflow-ack", encodeMessage overflow)] metricsCeType >>= \case
        Right (acks, poison) -> (acks, map (\(ack, _, reason) -> (ack, reason)) poison) `shouldBe` ([], [("overflow-ack", "OTLP metric count exceeds BIGINT maximum")])
        Left failure -> expectationFailure $ "expected poison, got write failure: " <> toString (Telemetry.writeFailureSummary failure)
      -- Each write target persists the point exactly once and a replay is idempotent. Counted
      -- in whichever store the target chose (max of PG and TF): with a real TimeFusion the
      -- single-leg markers land in one store; a shared-DB harness collapses both legs onto one.
      for_ ([("wt.pg.only", Just "pg-failed"), ("wt.tf.only", Just "tf-failed"), ("wt.both", Nothing)] :: [(Text, Maybe Text)]) \(name, failedLeg) -> do
        let send = processMetrics [("wt-ack", encodeMessage (createGaugeMetricAtTime key [] [] name 7 frozenTime))] (maybe id (HM.insert "monoscope-write-failure") failedLeg metricsCeType)
            stored = max <$> dbOne tr [sql| SELECT count(*)::int FROM otel_metrics WHERE project_id = ? AND metric_name = ? |] (mpid, name) <*> (V.head <$> countOf name)
        replicateM_ 2 (send >> (stored `shouldReturn` 1))

    it "sessions: aggregated by session and user identity with a reconciled summary, sorted, filtered, expanded, replay-flagged, isolated per project" \tr -> do
      -- Sessions derive from user identity, so project-wide totals are only meaningful over a
      -- project of this example's own (TimeFusion is shared).
      spid <- createTestProject tr "grpc-sessions"
      key <- createTestAPIKey tr spid "sessions-key"
      let alice = [("session.id", "abc"), ("user.id", "u1"), ("user.email", "alice@example.com")]
          bob = [("session.id", "xyz"), ("user.id", "u2"), ("user.email", "bob@example.com")]
          session name attrs isError = ingestSessionEvent tr key name attrs isError frozenTime
          rowsOf (Log.SessionsView _ rows _) = rows
          find' sid view = V.find ((== sid) . (.sessionId)) (rowsOf view)
          expand sessionKey q = do
            v <- call tr (Log.apiLogExpandH spid (Just "session") sessionKey Nothing q Nothing (Just $ fst testTimeRange) (Just $ snd testTimeRange))
            let field k = case v of AE.Object o -> KM.lookup k o; _ -> Nothing
                decoded :: AE.FromJSON a => AE.Key -> Maybe a
                decoded k = AE.decode . AE.encode =<< field k
            pure (fromMaybe [] (decoded "rows") :: [AE.Value], fromMaybe [] (decoded "cols") :: [Text], decoded "hasMore" :: Maybe Bool)
      for_ ([1 .. 3] :: [Int]) \i -> session ("GET /alice/" <> show i) alice False
      session "GET /alice/boom" alice True
      for_ ([1 .. 2] :: [Int]) \i -> session ("GET /bob/" <> show i) bob False
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      firstPage <- sessionsView tr spid Nothing Nothing
      let Log.SessionsView total rows summaryM = firstPage
      (total, V.length rows) `shouldBe` (2, 2)
      (find' "abc" firstPage <&> \s -> (s.eventCount, s.errorCount, s.userEmail, s.userId)) `shouldBe` Just (4, 1, Just "alice@example.com", Just "u1")
      (find' "xyz" firstPage <&> \s -> (s.eventCount, s.errorCount, s.userEmail)) `shouldBe` Just (2, 0, Just "bob@example.com")
      -- The summary comes from the same scan as the rows and reconciles with them.
      (summaryM <&> \s -> (s.totalSessions, s.erroredSessions, s.uniqueUsers, s.totalEvents, s.uniqueServices, s.medianEvents, (sum s.clean, sum s.errored))) `shouldBe` Just (2, 1, 2, 6, 1, 3, (1, 1))
      V.toList . fmap (.sessionId) . rowsOf <$> sessionsView tr spid Nothing (Just "errors") `shouldReturn` ["abc", "xyz"]
      session "GET /a" [("session.id", "s1-filter"), ("user.email", "alice-filter@example.com")] False
      session "GET /b" [("session.id", "s2-filter"), ("user.email", "bob-filter@example.com")] False
      for_ ([1 .. 3] :: [Int]) \i -> session ("GET /anon/" <> show i) [("session.id", "anon-sess")] False
      for_ ([1 .. 3] :: [Int]) \i -> session ("GET /expand/" <> show i) [("session.id", "expand-sess"), ("user.id", "eu1")] False
      -- SDKs without setSession emit identity but no session.id; sessions fall back to the user key.
      let derived = [("user.id", "derived-u1"), ("user.email", "derived@example.com")]
      for_ ([1 .. 2] :: [Int]) \i -> session ("GET /derived/" <> show i) derived False
      session "GET /derived/boom" derived True
      let ctxAttrs = [("session.id", "ctx-sess"), ("user.email", "ctx@example.com"), ("url.path", "/login"), ("user_agent.original", "Mozilla/5.0 ContextProbe")]
      session "GET /login" ctxAttrs False
      ingestSessionEvent tr key "POST /login" ctxAttrs True (addUTCTime 1 frozenTime)
      -- The Replay action shows only for sessions with a recording, matched on the parsed UUID
      -- whatever case the SDK chose.
      [recorded, plain, shouty] <- replicateM 3 UUIDV4.nextRandom
      for_ ([("GET /recorded", UUID.toText recorded), ("GET /plain", UUID.toText plain), ("GET /shouty", T.toUpper (UUID.toText shouty))] :: [(Text, Text)]) \(name, sid) -> session name [("session.id", sid)] False
      for_ ([recorded, shouty] :: [UUID.UUID]) \sid -> dbExec tr [sql| INSERT INTO projects.replay_sessions (session_id, project_id) VALUES (?, ?) |] (sid, spid)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      filtered <- sessionsView tr spid (Just "attributes.user.email == \"alice-filter@example.com\"") Nothing
      V.toList ((.sessionId) <$> rowsOf filtered) `shouldBe` ["s1-filter"]
      everything <- sessionsView tr spid Nothing Nothing
      (find' "anon-sess" everything <&> \s -> (s.eventCount, s.userId, s.userEmail, s.userName)) `shouldBe` Just (3, Nothing, Nothing, Nothing)
      let counts = V.toList $ (.eventCount) <$> rowsOf everything
      V.toList . fmap (.eventCount) . rowsOf <$> sessionsView tr spid Nothing (Just "events") `shouldReturn` sortBy (flip compare) counts
      let replayOf sid = (.hasReplay) <$> V.find ((== UUID.toText sid) . T.toLower . (.sessionId)) (rowsOf everything)
          sessionTag sid = "session;right-neutral\8658" <> UUID.toText sid
          wire = decodeUtf8 (AE.encode everything) :: Text
      (replayOf recorded, replayOf plain, replayOf shouty) `shouldBe` (Just True, Just False, Just True)
      (sessionTag recorded `T.isInfixOf` wire, sessionTag plain `T.isInfixOf` wire) `shouldBe` (True, False)
      -- Pins RawSessionRow's column order against fetchSessions' SELECT.
      ctx <- sessionsView tr spid (Just "attributes.session.id == \"ctx-sess\"") Nothing
      (find' "ctx-sess" ctx <&> \s -> (s.landingUrl, s.userAgent, (/= Just "") s.firstError && isJust s.firstError)) `shouldBe` Just (Just "/login", Just "Mozilla/5.0 ContextProbe", True)
      derivedView@(Log.SessionsView derivedTotal _ _) <- sessionsView tr spid (Just "attributes.user.email == \"derived@example.com\"") Nothing
      (derivedTotal, find' "derived-u1" derivedView <&> \s -> (s.eventCount, s.errorCount, s.userEmail)) `shouldBe` (1, Just (3, 1, Just "derived@example.com"))
      (expandRows, cols, hasMore) <- expand (Just "expand-sess") Nothing
      (length expandRows, "timestamp" `elem` cols, hasMore) `shouldBe` (3, True, Just False)
      (derivedExpand, _, _) <- expand (Just "derived-u1") Nothing
      length derivedExpand `shouldBe` 3
      let badRequest = \(ErrorCall msg) -> "400" `isInfixOf` msg
          expand400 kind sessionKey = (call tr (Log.apiLogExpandH spid kind sessionKey Nothing Nothing Nothing (Just $ fst testTimeRange) (Just $ snd testTimeRange)) >>= evaluate) `shouldThrow` badRequest
      expand400 (Just "session") Nothing
      expand400 (Just "invalid") (Just "some-key")
      -- A user filter must not erase project_id scoping in fetchSessions / fetchEventExamples.
      other <- createTestProject tr "sessions-other"
      dbExec tr [sql| INSERT INTO otel_logs_and_spans (project_id, timestamp, name, context___trace_id, attributes___session___id, attributes___user___email, attributes___user___id, summary)
                      VALUES (?, ?, 'GET /leak', 'trace-leak', 'cross-project-leak-sess', 'leak@external.com', 'leak-user', '{}'::TEXT[]) |] (other, frozenTime)
      session "GET /own" [("session.id", "cross-project-leak-sess"), ("user.email", "own@example.com")] False
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      Log.SessionsView leakTotal leakRows _ <- sessionsView tr spid (Just "attributes.user.email == \"leak@external.com\"") Nothing
      (leakTotal, V.length leakRows) `shouldBe` (0, 0)
      (leakExpand, _, _) <- expand (Just "cross-project-leak-sess") (Just "attributes.user.email == \"leak@external.com\"")
      leakExpand `shouldBe` []
      -- replaySessionGetH scopes session metadata by project too.
      shared <- UUIDV4.nextRandom
      dbExec tr [sql| INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at, user_id, user_email, user_name) VALUES (?, ?, ?, 'leaked-user', 'leaked@external.com', 'Leaked Name') |] (shared, other, frozenTime)
      replay <- call tr (Replay.replaySessionGetH spid shared)
      (replay.meta >>= \m -> m.userId <|> m.userEmail <|> m.userName) `shouldBe` Nothing

    it "trace sessions: identity spreads across batches via the cache, never overwrites, merges partial info, backfills rows the cache missed" \tr -> do
      key <- createTestAPIKey tr testPid "sess-prop"
      let send trId spanId name attrs ts = ingestSpanReq tr $ mkSpanRequest trId spanId Nothing name [] Nothing (uncurry mkAttr <$> attrs) (mkResource key []) ts
          columns trId = dbQuery tr [sql| SELECT name, attributes___session___id, attributes___user___id, attributes___user___email, attributes___user___name, attributes___user___full_name
                                         FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ? ORDER BY name |] (testPid, trId)
          sessionAndUser trId = map (\(n, s, u, _, _, _) -> (n, s, u)) <$> (columns trId :: IO [(Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)])
          fullIdentity = [("session.id", "sess-100"), ("user.id", "usr-200"), ("user.email", "u@test.com"), ("user.name", "uname"), ("user.full_name", "User Full")]
      send "aabbccdd11223344aabbccdd11223344" "span1aabbccdd" "frontend-span" fullIdentity frozenTime
      send "aabbccdd11223344aabbccdd11223344" "span2aabbccdd" "backend-span" [] frozenTime
      columns "aabbccdd11223344aabbccdd11223344"
        `shouldReturn` [(n, Just "sess-100", Just "usr-200", Just "u@test.com", Just "uname", Just "User Full") | n <- ["backend-span", "frontend-span" :: Text]]
      -- The backend span converted without identity still advertises the inherited one.
      backendSummary <- dbOne tr [sql| SELECT summary FROM otel_logs_and_spans WHERE project_id = ? AND name = 'backend-span' |] (Only testPid)
      backendSummary `shouldSatisfy` \s -> all (`V.elem` (s :: V.Vector Text)) ["session;right-badge-neutral⇒sess-100", "user email;right-badge-neutral⇒u@test.com"]
      send "11223344aabbccdd11223344aabbccdd" "spanaaaa1111" "span-A" [("session.id", "sess-A")] frozenTime
      send "11223344aabbccdd11223344aabbccdd" "spanbbbb2222" "span-B" [("session.id", "sess-B")] frozenTime
      sessionAndUser "11223344aabbccdd11223344aabbccdd" `shouldReturn` [("span-A", Just "sess-A", Nothing), ("span-B", Just "sess-B", Nothing)]
      send "deadbeef11223344deadbeef11223344" "span11112222" "has-session" [("session.id", "sess-merge")] frozenTime
      send "deadbeef11223344deadbeef11223344" "span33334444" "has-user" [("user.id", "usr-merge")] frozenTime
      send "deadbeef11223344deadbeef11223344" "span55556666" "has-neither" [] frozenTime
      find (\(n, _, _) -> n == "has-neither") <$> sessionAndUser "deadbeef11223344deadbeef11223344" `shouldReturn` Just ("has-neither", Just "sess-merge", Just "usr-merge")
      -- The sweep's window is relative to the DB's NOW(): spans aged 5 min on that clock land
      -- in its lagged 2–17 min window, which only rows the ingest cache missed reach.
      base <- dbOne tr [sql| SELECT now() |] ()
      send "cafebabe11223344cafebabe11223344" "spaneeee1111" "early-span" [] (addUTCTime (-300) base)
      send "cafebabe11223344cafebabe11223344" "spanffff2222" "late-span" [("session.id", "sess-backfill"), ("user.id", "usr-backfill")] (addUTCTime (-300) base)
      find (\(n, _, _) -> n == "early-span") <$> sessionAndUser "cafebabe11223344cafebabe11223344" `shouldReturn` Just ("early-span", Nothing, Nothing)
      runTestBg base tr (BackgroundJobs.backfillSessionAttributes tr.trATCtx) >>= (`shouldSatisfy` (> 0))
      find (\(n, _, _) -> n == "early-span") <$> sessionAndUser "cafebabe11223344cafebabe11223344" `shouldReturn` Just ("early-span", Just "sess-backfill", Just "usr-backfill")

    it "standard OTel spans: SDK conventions normalised, methods recovered, endpoints keyed without hosts, routes shared across a batch, one row per export" \tr -> do
      apiKey <- createTestAPIKey tr testPid "std-otel"
      let export kind name attrs resource = do
            trId <- show <$> UUIDV4.nextRandom
            spanId' <- show <$> UUIDV4.nextRandom
            ingestSpanReq tr $ withSpanKind kind $ mkSpanRequest trId spanId' Nothing name [] Nothing attrs resource frozenTime
          -- An auto-instrumented HTTP span (not our SDK).
          stdSpan name method path status host =
            export PT.Span'SPAN_KIND_SERVER name [mkAttr "http.request.method" method, mkAttr "http.response.status_code" (show (status :: Int)), mkAttr "url.path" path, mkAttr "server.address" host] (mkResource apiKey [mkAttr "telemetry.sdk.name" "opentelemetry", mkAttr "telemetry.sdk.language" "nodejs"])
          countWhere :: Query -> Text -> IO Int
          countWhere q v = dbOne tr q (testPid, v)
      for_
        ( [ ("java", PT.Span'SPAN_KIND_SERVER, [mkAttr "http.method" "GET", mkAttr "http.target" "/orders/42?expand=true", mkAttr "net.host.name" "api.java.test", mkAttr "http.route" "/orders/{id}"], "/orders/42", "api.java.test", "GET", "/orders/{id}")
          , ("python", PT.Span'SPAN_KIND_SERVER, [mkAttr "http.request.method" "GET", mkAttr "url.path" "/orders/43", mkAttr "server.address" "api.python.test"], "/orders/43", "api.python.test", "GET", "/orders/{number}")
          , ("nodejs", PT.Span'SPAN_KIND_CLIENT, [mkAttr "http.request.method" "POST", mkAttr "url.full" "https://api.node.test/v1/send/44?expand=true"], "/v1/send/44", "api.node.test", "POST", "/v1/send/{number}")
          , ("go", PT.Span'SPAN_KIND_CLIENT, [mkAttr "http.request.method" "GET", mkAttr "url.full" "https://api.go.test/orders/45", mkAttr "url.template" "/orders/:id"], "/orders/45", "api.go.test", "GET", "/orders/{id}")
          ]
            :: [(Text, PT.Span'SpanKind, [PC.KeyValue], Text, Text, Text, Text)]
        )
        \(language, kind, attrs, path, host, method, endpointPath) -> do
          export kind (language <> " HTTP") attrs (mkResource apiKey [mkAttr "telemetry.sdk.language" language, mkAttr "service.name" "fallback-svc"])
          dbQuery tr [sql| SELECT attributes___url___path, attributes___http___route, attributes___server___address, attributes___http___request___method, attributes->'monoscope'->'endpoint'->>'route'
                           FROM otel_logs_and_spans WHERE project_id = ? AND name = ? |] (testPid, language <> " HTTP")
            `shouldReturn` [(Just path, Just endpointPath, Just host, Just method, Just endpointPath)]
      -- A missing method is recovered from the span name, never assumed to be GET.
      for_ ([("POST /orders/{id}", "POST", "/orders/{id}", True, True), ("HTTP /orders/{id}", "_OTHER", "/orders/{id}", True, True), ("HTTP /orders/42", "_OTHER", "/orders/{number}", False, True), ("PATCH /orders/{id}", "PATCH", "/orders/{id}", False, False)] :: [(Text, Text, Text, Bool, Bool)]) \(name, method, endpointRoute, hasRoute, hasPath) -> do
        export PT.Span'SPAN_KIND_SERVER name ([mkAttr "url.path" "/orders/42" | hasPath] <> [mkAttr "http.route" "/orders/{id}" | hasRoute]) (mkResource apiKey [])
        dbQuery tr [sql| SELECT attributes___http___request___method, attributes___http___route FROM otel_logs_and_spans WHERE project_id = ? AND name = ? |] (testPid, name)
          `shouldReturn` [(Just method, Just endpointRoute)]
      -- #631 re-keyed every endpoint of a project whose server spans carry no host, and minted
      -- `_OTHER` endpoints for Express middleware and browser documentFetch spans.
      for_
        ( [ (PT.Span'SPAN_KIND_SERVER, "POST", [mkAttr "http.request.method" "POST", mkAttr "url.path" "/identity/login"])
          , (PT.Span'SPAN_KIND_INTERNAL, "request middleware - /identity/:id", [mkAttr "http.route" "/identity/:id", mkAttr "express.type" "middleware"])
          , (PT.Span'SPAN_KIND_INTERNAL, "request handler - /identity/:id", [mkAttr "http.request.method" "_OTHER", mkAttr "http.route" "/identity/:id", mkAttr "server.address" "", mkAttr "express.type" "request_handler"])
          , (PT.Span'SPAN_KIND_INTERNAL, "documentFetch", [mkAttr "http.request.method" "GET", mkAttr "url.full" "https://identity.example.com/identity/page"])
          ]
            :: [(PT.Span'SpanKind, Text, [PC.KeyValue])]
        )
        \(kind, name, attrs) -> export kind name attrs (mkResource apiKey [mkAttr "service.name" "identity-svc"])
      replicateM_ 3 $ stdSpan "GET /api/orders" "GET" "/api/orders/12345" 200 "orders.example.com"
      replicateM_ 3 $ stdSpan "POST /api/payments" "POST" "/api/payments" 201 "orders.example.com"
      replicateM_ 3 $ stdSpan "GET" "GET" "/admin/companies/ec8213d0-20e6-4225-bf05-5d8215193d9b/employee-details" 200 "admin.example.com"
      stdSpan "GET /api/users" "GET" "/api/users/550e8400-e29b-41d4-a716-446655440000" 200 "api.example.com"
      stdSpan "GET /nodup" "GET" "/nodup" 200 "nodup.example.com"
      stdSpan "GET /health" "GET" "/health" 200 "mixed.example.com"
      ingestTrace tr apiKey "apitoolkit-http-span" frozenTime
      export PT.Span'SPAN_KIND_SERVER "db.query" [] (mkResource apiKey [])
      -- A batch shares a span's declared route with its unrouted siblings, in either order.
      let storesPath = "/api/stores/6a42926ba6838426a09a58c0/business-hours"
          declared = route "/api/stores/:id/business-hours" storesPath frozenTime
          unrouted = route "" storesPath frozenTime
          caches = one (testPid, Projects.defaultProjectCache)
          storesHash = toXXHash $ testPid.toText <> "GET" <> "/api/stores/{id}/business-hours"
          canonical = "/api/stores/{id}/business-hours" :: Text
      for_ ([[declared, unrouted], [unrouted, declared]] :: [[Telemetry.OtelLogsAndSpans]]) \spans -> V.map (.hashes) (stampHashesAtIngest caches $ V.fromList spans) `shouldBe` V.replicate 2 (Just $ V.singleton storesHash)
      let stamped = stampHashesAtIngest caches $ V.fromList [unrouted, declared]
      for_ (V.zip stamped (V.fromList [True, False])) \(sp, inferred) -> do
        AesonText attrs <- maybe (fail "missing stamped attributes") pure sp.attributes
        (Map.lookup "url" attrs, Map.lookup "http" attrs, Map.lookup "monoscope" attrs)
          `shouldBe` (Just (AE.object ["path" AE..= storesPath]), Just (AE.object ["request" AE..= AE.object ["method" AE..= ("GET" :: Text)], "route" AE..= canonical]), Just (AE.object ["endpoint" AE..= AE.object ["route" AE..= canonical, "route_inferred" AE..= inferred]]))
      runTestBg frozenTime tr $ Telemetry.handOffBatches tr.trATCtx.extractionWorker caches stamped
      drainExtractionWorker tr
      let endpointsLike :: Text -> IO [(Text, Text, Text)]
          endpointsLike p = dbQuery tr [sql| SELECT method, host, url_path FROM apis.endpoints WHERE project_id = ? AND url_path LIKE ? ORDER BY url_path |] (testPid, p)
      endpointsLike "/identity%" `shouldReturn` [("POST", "identity-svc", "/identity/login")]
      dbQuery tr [sql| SELECT attributes___server___address FROM otel_logs_and_spans WHERE project_id = ? AND name IN ('POST', 'request handler - /identity/:id') ORDER BY name |] (Only testPid)
        `shouldReturn` replicate 2 (Only (Just ("identity-svc" :: Text)))
      map (\(_, _, p) -> p) <$> dbQuery @_ @(Text, Text, Text) tr [sql| SELECT method, host, url_path FROM apis.endpoints WHERE project_id = ? AND host = 'orders.example.com' ORDER BY url_path |] (Only testPid)
        `shouldReturn` ["/api/orders/{number}", "/api/payments"]
      dbQuery tr [sql| SELECT url_path FROM apis.endpoints WHERE project_id = ? AND host = 'admin.example.com' |] (Only testPid) `shouldReturn` [Only ("/admin/companies/{uuid}/employee-details" :: Text)]
      dbQuery tr [sql| SELECT url_path, hash FROM apis.endpoints WHERE project_id = ? AND url_path LIKE '/api/stores%' |] (Only testPid) `shouldReturn` [(canonical, storesHash)]
      -- Standard spans keep their names (not renamed to monoscope.http); SDK HTTP spans are;
      -- non-HTTP spans are not. With no real TimeFusion, WriteBoth once inserted every span twice.
      countWhere [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND name = ? |] `traverse` ["GET /api/users", "GET /health", "monoscope.http", "db.query"] `shouldReturn` [1, 1, 1, 1]
      countWhere [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND attributes___server___address = ? |] "nodup.example.com" `shouldReturn` 1
      -- A single-span trace reports its own duration (the end-time fold once ignored the first span).
      trId <- dbOne tr [sql| SELECT context___trace_id FROM otel_logs_and_spans WHERE project_id = ? AND name = 'db.query' |] (Only testPid)
      details <- runTestBg frozenTime tr $ Telemetry.getTraceDetails False testPid trId (Just frozenTime) frozenTime
      fmap (\(t, ss) -> (t.traceDurationNs > 0, length ss)) details `shouldBe` Just (True, 1)

    it "SDK request messages: acked, secrets redacted before storage, hashes and routes stamped at ingest, endpoints minted" \tr -> do
      nowTxt <- toText . formatTime defaultTimeLocale "%FT%T%QZ" <$> getCurrentTime
      let encoded = toStrict . AE.encode . Unsafe.fromJust . convert
          process msgs = runTestBg frozenTime tr (processMessages msgs HM.empty)
          acks msgs = process msgs >>= either (const $ fail "processMessages returned Left WriteFailure") (pure . fst)
      -- Hashes once arrived only via the extraction worker's UPDATE, minting a duplicate row
      -- version per span in TimeFusion's merge-on-read store; rows carry them before any job runs.
      acks [("m1", encoded $ testRequestMsgs.reqMsg1 nowTxt)] `shouldReturn` ["m1"]
      dbQuery tr [sql| SELECT unnest(hashes) FROM otel_logs_and_spans WHERE project_id = ? |] (Only testPid) `shouldReturn` [Only (toXXHash $ testPid.toText <> "172.31.29.11" <> "GET" <> "/")]
      dbQuery tr [sql| SELECT attributes___http___route, attributes->'monoscope'->'endpoint'->>'route' FROM otel_logs_and_spans WHERE project_id = ? |] (Only testPid) `shouldReturn` [(Just "/" :: Maybe Text, Just "/" :: Maybe Text)]
      let secret = "Bearer must-not-reach-storage" :: Text
          withSecret = case testRequestMsgs.reqMsg1 nowTxt of
            AE.Object fields -> AE.Object $ KM.insert "request_headers" (AE.object ["authorization" AE..= ([secret] :: [Text])]) fields
            value -> value
      acks [("redaction", toStrict $ AE.encode withSecret)] `shouldReturn` ["redaction"]
      dbQuery tr [sql| SELECT attributes #>> '{http,request,headers,authorization}', attributes::text LIKE ? FROM otel_logs_and_spans WHERE project_id = ? AND attributes #>> '{http,request,headers,authorization}' IS NOT NULL |] ("%" <> secret <> "%", testPid)
        `shouldReturn` [("[\"[REDACTED]\"]" :: Text, False)]
      let rawMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"00000000-0000-0000-0000-000000000000\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"e30=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"" <> nowTxt <> "\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"
      acks [("raw", encodeUtf8 rawMsg)] `shouldReturn` ["raw"]
      acks [("m1", encoded $ testRequestMsgs.reqMsg1 nowTxt), ("m2", encoded $ testRequestMsgs.reqMsg2 nowTxt)] `shouldReturn` ["m1", "m2"]
      drainExtractionWorker tr
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      endpoints <- fmap fst $ runTestBg frozenTime tr $ Endpoints.endpointRequestStatsPageByProject Endpoints.WithStats False testPid Endpoints.EndpointQuery{direction = Endpoints.Incoming, archived = False, host = Nothing, search = Nothing, sort = Endpoints.SortEvents, page = 0, perPage = 200, period = Endpoints.Window24h, service = Nothing}
      sort (V.toList $ (.urlPath) <$> endpoints) `shouldBe` ["/", "/api/v1/user/login"]

    it "extraction worker: flushed patterns tag every event, eager batches derive endpoints and hashes, the safety net re-drives unprocessed rows exactly once" \tr -> do
      worker <- ExtractionWorker.initWorkerState 1 100
      let ctx = tr.trATCtx
          withConfig f = tr{trATCtx = ctx{config = f ctx.config}}
          service = "pattern-membership-regression"
          spans =
            [ ExtractionWorker.BufferedSpan "10000000-0000-4000-8000-000000000001" "pattern-first" "pattern-trace" frozenTime "Connected to database primary" True
            , ExtractionWorker.BufferedSpan "10000000-0000-4000-8000-000000000002" "pattern-second" "pattern-trace" frozenTime "Connected to database replica" False
            , ExtractionWorker.BufferedSpan "10000000-0000-4000-8000-000000000003" "" "" frozenTime "Connected to database <*> <*>" False
            ]
      for_ spans \s ->
        dbExec tr [sql| INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, kind, context___span_id, context___trace_id, summary)
                        VALUES (?::uuid, ?, ?, ?, 'SERVER', NULLIF(?::text, ''), NULLIF(?::text, ''), ARRAY[?]::text[]) |] (s.eventId, testPid.toText, frozenTime, frozenTime, s.spanCtxId, s.traceId, s.summary)
      for_ worker.shards \shard -> runTestBg frozenTime (withConfig \c -> c{enableTimefusionWrites = False, enableHashUpdates = True}) $ BackgroundJobs.flushDrainTask shard ExtractionWorker.DrainFlushTask{projectId = testPid, serviceName = service, spans, flushedAt = frozenTime}
      dbQuery tr [sql| SELECT lp.occurrence_count::int, lp.is_error, count(*)::int FROM apis.log_patterns lp JOIN otel_logs_and_spans o ON o.project_id = lp.project_id::text AND o.hashes @> ARRAY['pat:' || lp.pattern_hash]
                       WHERE lp.project_id = ? AND lp.service_name = ? GROUP BY lp.pattern_hash, lp.occurrence_count, lp.is_error |] (testPid, service)
        `shouldReturn` [(3 :: Int, True, 3 :: Int)]
      apiKey <- createTestAPIKey tr testPid "extraction-worker"
      replicateM_ 3 $ ingestTrace tr apiKey "GET /api/parity/check" frozenTime
      replicateM_ 3 $ ingestTrace tr apiKey "POST /api/parity/submit" frozenTime
      let trTf = tr{trATCtx = ctx{env = ctx.env{enableTimefusionReads = True}, config = ctx.config{enableTimefusionWrites = True}}}
          countWhere q = dbOne tr q (Only testPid) :: IO Int
      drainExtractionWorker trTf
      countWhere [sql| SELECT count(*)::int FROM apis.endpoints WHERE project_id = ? AND url_path LIKE '/api/parity/%' |] `shouldReturn` 2
      countWhere [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND processed_at IS NOT NULL AND name LIKE '% /api/parity/%' |] `shouldReturn` 6
      tfHashes <- runTestBg frozenTime trTf $ Hasql.withHasqlTimefusion True $ Hasql.interp [HI.sql| SELECT array_to_string(hashes, chr(31)) FROM otel_logs_and_spans WHERE project_id = #{testPid.toText} AND name = 'GET /api/parity/check' |]
      (tfHashes :: V.Vector Text) `shouldSatisfy` any (T.isInfixOf "pat:")
      -- Rows the worker dropped (not accepting) stay processed_at IS NULL until the safety net
      -- re-drives them; it selects rows between 10m and 24h old on the DB clock.
      dbNow <- dbOne tr [sql| SELECT now() |] ()
      let shards = V.toList ctx.extractionWorker.shards
          unprocessed name = dbOne tr [sql| SELECT count(*)::int FROM otel_logs_and_spans WHERE project_id = ? AND processed_at IS NULL AND name = ? |] (testPid, name :: Text) :: IO Int
          dropped act = atomically (writeTVar ctx.extractionWorker.acceptingBatches False) >> act >> atomically (writeTVar ctx.extractionWorker.acceptingBatches True)
          safetyNet res = runTestBg dbNow res $ BackgroundJobs.processBackgroundJob res.trATCtx (BackgroundJobs.SafetyNetReprocess testPid)
      dropped $ ingestTrace tr apiKey "GET /api/safetynet/test" (addUTCTime (-3600) dbNow)
      unprocessed "GET /api/safetynet/test" `shouldReturn` 1
      safetyNet tr >> drainExtractionWorker tr
      unprocessed "GET /api/safetynet/test" `shouldReturn` 0
      -- processed_at was stamped only by the hash UPDATE, gated by enableHashUpdates and
      -- hashUpdateMaxAgeSecs; with that gate closed the safety net re-drove the same rows every
      -- tick forever (2026-07-29/30). The 3h row is past the 2h age cutoff: never re-driven.
      let trNoHash = withConfig \c -> c{enableHashUpdates = False}
          -- Driven at dbNow, not frozenTime, so the age cutoff sees the rows' real age.
          drainAt = do
            for_ shards \shard -> do
              batches <- atomically $ flushTBQueue shard.ingressQ >>= \bs -> modifyTVar' shard.queueDepth (subtract (length bs)) $> bs
              for_ batches \b -> runTestBg dbNow trNoHash $ BackgroundJobs.processEagerBatch b shard
            ExtractionWorker.forceFlushAllBuffers ctx.extractionWorker dbNow
            for_ shards \shard -> atomically (flushTBQueue shard.drainFlushQ) >>= traverse_ (runTestBg dbNow trNoHash . BackgroundJobs.flushDrainTask shard)
      dropped do
        ingestTrace tr apiKey "GET /api/loop/test" (addUTCTime (-3600) dbNow)
        ingestTrace tr apiKey "GET /api/loop/stale" (addUTCTime (-(3 * 3600)) dbNow)
      safetyNet trNoHash >> drainAt
      countWhere [sql| SELECT count(*)::int FROM apis.endpoints WHERE project_id = ? AND url_path LIKE '/api/loop/%' |] >>= (`shouldSatisfy` (>= 1))
      unprocessed "GET /api/loop/test" `shouldReturn` 0
      safetyNet trNoHash
      atomically (sum <$> mapM (fmap length . flushTBQueue . (.ingressQ)) shards) `shouldReturn` 0

    -- processList surfaces dual-write outcomes as 'Either WriteFailure', never a swallowed
    -- exception or a silent ack: Pkg.Queue routes Left to the DLQ with side-tracking headers so
    -- the replay tool rewrites only the missing side.
    it "write retries: transient resets retried within the budget, poison fails fast, TuplesOk swallowed, slow failures stop at the time budget" \tr -> do
      -- B1 of the 2026-06-21 fix: the per-batch project-id / cache lookups retry a transient
      -- reset — an EMPTY SQLSTATE, as pgdog / datafusion-postgres send when resetting a backend
      -- mid-statement — instead of dead-lettering a batch the write path would accept. A real
      -- SQLSTATE stays poison: fail fast → DLQ.
      let retryRead sqlState = do
            counter <- newIORef (0 :: Int)
            res <- try @_ @SomeException $ runTestBg frozenTime tr $ Hasql.retryTransientEff 2_000_000 "read-gate" do
              n <- atomicModifyIORef' counter \c -> (c + 1, c + 1)
              maybe (pure n) (throwIO . serverError) (sqlState n)
            (rightToMaybe res,) <$> readIORef counter
      retryRead (\n -> "" <$ guard (n < 3)) `shouldReturn` (Just 3, 3)
      retryRead (const $ Just "23505") `shouldReturn` (Nothing, 1)
      -- TimeFusion's PGWire answers an INSERT with TuplesOk; retryHasqlWrite swallows that into
      -- a 0-row success without reading back. That fabricated success made the 2026-06-12 loss
      -- silent; the under-persist cross-check below is what catches it now.
      runTestBg frozenTime tr (Telemetry.retryHasqlWrite 500_000 "tf" (throwIO (ErrorCall "TimeFusion PGWire: Unexpected result status: TuplesOk")))
        >>= either (\e -> expectationFailure $ "expected the TuplesOk swallow; got Left " <> show (e :: SomeException)) (\(bir :: Telemetry.BulkInsertResult) -> bir.rowsInserted `shouldBe` 0)
      -- An attempt count only bounds the sleeps: with 30s acquisition timeouts TF's 42 attempts
      -- held one batch ~23 minutes past the 300s stall alarm.
      for_ ([(Telemetry.tfWriteBudgetMicros, 6), (Telemetry.pgWriteBudgetMicros, 1)] :: [(Int, Int)]) \(budget, attempts) -> do
        counter <- newIORef (0 :: Int)
        res <- runTestBg frozenTime tr $ Telemetry.retryHasqlWrite budget "tf" do
          atomicModifyIORef' counter \c -> (c + 1, ())
          sleepMicros 30_000_000
          throwIO (Hasql.HasqlException HP.AcquisitionTimeoutUsageError)
        isLeft (res :: Either SomeException Telemetry.BulkInsertResult) `shouldBe` True
        readIORef counter `shouldReturn` attempts

    it "processList outcomes: acks with one batch identity, TF down or silent → Left That, PG down throws, poison isolated, unresolvable keys never silently acked, replays rewrite one leg" \tr -> do
      key <- createTestAPIKey tr testPid "tf-write-failure-spec"
      let send res msgs attrs = runTestBg frozenTime res $ OtlpServer.processList msgs attrs
          right = either (\wf -> expectationFailure ("expected Right; got Left " <> toString (Telemetry.writeFailureSummary wf)) >> pure ([], [])) (pure . second (map (\(a, _, r) -> (a, r))))
          expectThat what = \case
            Left wf | isThat wf -> pass
            Left wf -> expectationFailure $ "expected " <> what <> "; got " <> toString (Telemetry.writeFailureSummary wf)
            Right (acks, poison) -> expectationFailure $ "expected " <> what <> "; got Right acks=" <> show acks <> " poison=" <> show (map (\(a, _, _) -> a) poison)
          retried = logsNamed "retryHasqlWrite: transient error, retrying"
      -- A batch identified by project id alone, no API key, is persisted.
      let ackProjectOnly = "ack-project-id-only"
          projectOnly = createOtelLogAtTime "" ["valid record from " <> ackProjectOnly] frozenTime & LSF.resourceLogs %~ map (PLF.resource . PRF.attributes .~ [mkAttr "at-project-id" testPid.toText])
      before <- countDemoSpans tr ackProjectOnly
      send tr [(ackProjectOnly, encodeMessage projectOnly)] logsAttrs >>= right >>= (`shouldBe` ([ackProjectOnly], []))
      countDemoSpans tr ackProjectOnly `shouldReturn` (before + 1)
      -- Two TF write attempts fail like a draining TimeFusion; both batches still ack under one
      -- batch id shared by the retries and the acknowledgment.
      let msgs = [validLogMsg key "ack-good-1", validLogMsg key "ack-good-2"]
          retryDDL =
            [ "CREATE SEQUENCE test_tf_batch_attempt"
            , -- The volatile counter sits behind its own IF: SQL may reorder AND operands.
              "CREATE FUNCTION test_tf_batch_retry() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF current_setting('test.retry_tf', true) = 'on' THEN IF nextval('test_tf_batch_attempt') <= 2 THEN RAISE EXCEPTION 'TimeFusion is draining for deployment; retry on the replacement' USING ERRCODE = 'XX000'; END IF; END IF; RETURN NEW; END $$"
            , "CREATE TRIGGER test_tf_batch_retry BEFORE INSERT ON otel_logs_and_spans FOR EACH ROW EXECUTE FUNCTION test_tf_batch_retry()"
            ]
      withResource tr.trPool \c -> for_ (retryDDL :: [Query]) (execute_ c)
      (results, entries) <-
        withCapturedLogs tr (\logged -> withPgBackedTf logged " options='-c test.retry_tf=on'" \retrying -> forM [msgs, reverse msgs] \batch -> send retrying batch logsAttrs)
          `finally` withResource tr.trPool \c -> for_ (["DROP FUNCTION IF EXISTS test_tf_batch_retry() CASCADE", "DROP SEQUENCE IF EXISTS test_tf_batch_attempt"] :: [Query]) (execute_ c)
      for_ results $ right >=> \(acks, poison) -> (L.sort acks, poison) `shouldBe` (["ack-good-1", "ack-good-2"], [])
      let completed = logsNamed "telemetry.write_acknowledged" entries
          ids = map batchIdOf completed
      (length (retried entries), map (logField "store") (retried entries), map batchIdOf (retried entries)) `shouldBe` (2, replicate 2 (Just "tf"), replicate 2 (join $ listToMaybe ids))
      (length completed, length (L.nub ids), all isJust ids) `shouldBe` (2, 1, True)
      map (\e -> (logField "record_count" e, logField "write.table" e)) completed `shouldBe` replicate 2 (Just (AE.Number 2), Just "otel_logs_and_spans")
      -- Metric acknowledgments carry identity, never payload (direct exports may lack trace context).
      (_, metricEntries) <- withCapturedLogs tr \logged -> ingestMetric logged key [] [] "private.metric.canary" 1 frozenTime
      [metricAck] <- pure $ logsNamed "telemetry.write_acknowledged" metricEntries
      batchIdOf metricAck `shouldSatisfy` isJust
      (case metricAck.lmData of AE.Object o -> L.sort (KM.keys $ KM.delete "span_id" $ KM.delete "trace_id" o); _ -> [])
        `shouldBe` ["elapsed_us", "record_count", "write.batch_id", "write.table", "write_target"]
      (logField "write.table" metricAck, logField "record_count" metricAck) `shouldBe` (Just "otel_metrics", Just (AE.Number 1))
      -- TF down → Left That, with one batch identity across retries and the terminal failure.
      (_, downEntries) <- withCapturedLogs tr \logged -> breakPools False True logged >>= \down -> send down [validLogMsg key "ack-1"] logsAttrs >>= expectThat "TF-only failure"
      let failed = logsNamed "TF_WRITE_FAILED" downEntries
      (null (retried downEntries), length failed, logsNamed "telemetry.write_acknowledged" downEntries) `shouldBe` (False, 1, [])
      for_ (retried downEntries <> failed) \e -> (batchIdOf e, logField "write.table" e) `shouldBe` (listToMaybe failed >>= batchIdOf, Just "otel_logs_and_spans")
      (listToMaybe failed >>= batchIdOf) `shouldSatisfy` isJust
      -- 2026-06-12: a reachable TF that accepts inserts yet persists nothing (a row trigger keyed
      -- on a GUC only this pool's connection carries) was acked as success; the rows-persisted
      -- cross-check now turns it into Left That → DLQ.
      withResource tr.trPool \c -> mapM_ (execute_ c) silentTfDDL
      (_, silentEntries) <-
        withCapturedLogs tr (\logged -> withPgBackedTf logged " options='-c test.silent_tf=on'" \silent -> send silent [validLogMsg key "ack-silent-tf"] logsAttrs >>= expectThat "TF silent under-persist")
          `finally` withResource tr.trPool \c -> void (execute_ c "DROP TRIGGER IF EXISTS test_silent_tf ON otel_logs_and_spans")
      (map batchIdOf (logsNamed "WRITE_UNDERPERSIST_DETECTED" silentEntries) <&> isJust, logsNamed "telemetry.write_acknowledged" silentEntries) `shouldBe` ([True], [])
      -- PG down trips the project-key lookup before any write (a key not yet looked up misses
      -- the cache): processList throws and Pkg.Queue's outer tryAny routes it via DLQ-or-redeliver.
      uncachedKey <- createTestAPIKey tr testPid "pg-down"
      for_ ([(True, False), (True, True)] :: [(Bool, Bool)]) \(pgDown, tfDown) -> do
        broken <- breakPools pgDown tfDown tr
        try @_ @SomeException (send broken [validLogMsg uncachedKey "ack-1"] logsAttrs) >>= (`shouldSatisfy` isLeft)
      breakPools False True tr >>= \down -> send down [validLogMsg key "ack-good", corruptMsg "ack-poison"] logsAttrs >>= expectThat "TF-only failure on a mixed batch"
      send tr [corruptMsg "ack-poison"] logsAttrs >>= right <&> second (map fst) >>= (`shouldBe` ([], ["ack-poison"]))
      send tr [validLogMsg key "ack-good", corruptMsg "ack-poison"] logsAttrs >>= right <&> second (map fst) >>= (`shouldBe` (["ack-good"], ["ack-poison"]))
      -- An unsupported ce-type once returned Right ([], []): never committed, a hot redelivery loop.
      send tr [validLogMsg key "ack-unsup-1", corruptMsg "ack-unsup-2"] (HM.singleton "ce-type" "org.rrweb.events.v1") >>= right
        >>= (`shouldBe` ([], [("ack-unsup-1", "unsupported ce-type"), ("ack-unsup-2", "unsupported ce-type")]))
      -- 2026-07-27: during a 9-minute TimeFusion OOM ~200k rows vanished with no DLQ entry. A
      -- key projectIdsByProjectApiKeys can't resolve drops out of keyToId, convert yields zero
      -- records for a payload that carried spans, and the allEmpty branch acked every offset.
      -- Converted to zero rows is a DROP: it must surface as poison or a failure, never a clean ack.
      unresolvedBefore <- countDemoSpans tr "ack-unresolvable"
      unresolved <- send tr [validLogMsg "no-such-project-key-2026-07-27" "ack-unresolvable"] logsAttrs
      countDemoSpans tr "ack-unresolvable" `shouldReturn` unresolvedBefore
      case unresolved of
        Right (acks, []) -> expectationFailure $ "SILENT DROP: zero rows written and processList acked " <> show acks <> " with no poison — the offsets commit and the data is gone"
        Right (_, poison) -> length poison `shouldBe` 1
        Left _ -> pass
      -- DLQ replay rewrites ONLY the failed leg: the durable leg already holds the row and PG
      -- inserts aren't idempotent. Both pools hit the shared test DB, so a dual-write replay
      -- would add the row twice.
      tfOn <- breakPools False False tr
      tfReplayBefore <- countDemoSpans tfOn "ack-tf-replay"
      send tfOn [validLogMsg key "ack-tf-replay"] (HM.insert "monoscope-write-failure" "tf-failed" logsAttrs) >>= right >>= (`shouldBe` (["ack-tf-replay"], []))
      countDemoSpans tfOn "ack-tf-replay" `shouldReturn` (tfReplayBefore + 1)
      -- A pg-failed replay skips even a DOWN TimeFusion leg, where a normal message DLQs.
      breakPools False True tr >>= \down -> send down [validLogMsg key "ack-pg-replay"] (HM.insert "monoscope-write-failure" "pg-failed" logsAttrs) >>= right <&> fst >>= (`shouldBe` ["ack-pg-replay"])

    it "kafka polling: a count-based poll is bounded by bytes, an oversized record is admitted alone, the count limit holds, nothing is committed" \tr -> do
      let poll bVar n = runBackground tr.trLogger tr.trATCtx tr.trTracerProvider $ runErrorNoCallStack @K.KafkaError $ runMockProducer bVar $ runMockConsumer ["otlp_logs"] bVar $ Queue.pollKafkaBatch n
          seeded n value hdrs = newTVarIO $ foldl' (\b _ -> appendRecord "otlp_logs" (Just value) hdrs b) emptyBroker [1 .. n :: Int]
      big <- seeded 32 (BS.replicate (8 * 1024 * 1024) 0) (K.headersFromList [])
      fmap (map (fmap (.crOffset))) <$> poll big 5000 `shouldReturn` Right (map (Right . K.Offset) [0 .. 7])
      readTVarIO big >>= \b -> (Map.lookup ("otlp_logs", 0) b.cursor, b.committed) `shouldBe` (Just 8, mempty)
      for_ ([(K.headersFromList [("large", BS.replicate (64 * 1024 * 1024) 0)], 5000, 1), (K.headersFromList [], 2, 2)] :: [(K.Headers, Int, Int)]) \(hdrs, countLimit, expected) -> do
        bVar <- seeded 3 "x" hdrs
        fmap length <$> poll bVar countLimit `shouldReturn` Right expected
        (Map.lookup ("otlp_logs", 0) . (.cursor) <$> readTVarIO bVar) `shouldReturn` Just expected

    it "kafka commits: contiguous watermark, DLQ flushed before commit, no-ack chunks reseeked, chunks concurrent, capacity shared across consumers" \tr -> do
      let committedTo n bVar = readTVar bVar >>= \b -> check (Map.lookup ("otlp_logs", 0) b.committed == Just n)
          ackAll tuples _ = pure (Right (map fst tuples, []))
          seeded = newTVarIO emptyBroker >>= \bVar -> seedTopic bVar "otlp_logs" [0 .. 4] $> bVar
      bVar <- seeded
      receivedVar <- newTVarIO ([] :: [ByteString])
      b <- drive tr bVar ["otlp_logs"] Queue.KafkaPrimary (\tuples attrs -> atomically (modifyTVar' receivedVar (<> map snd tuples)) >> ackAll tuples attrs) (committedTo 5 bVar)
      sort <$> readTVarIO receivedVar `shouldReturn` sort (map (encodeUtf8 . show) [0 .. 4 :: Int64])
      Map.lookup ("otlp_logs", 0) b.committed `shouldBe` Just 5
      -- Every record poisoned: the DLQ records become durable (leave `pending`) only when the
      -- committer flushes, right before it commits. Without the gate they stay buffered.
      dlqVar <- seeded
      dlq <- drive tr dlqVar ["otlp_logs"] Queue.KafkaPrimary (\tuples _ -> pure (Right ([], [(t, raw, "forced poison") | (t, raw) <- tuples]))) (committedTo 5 dlqVar)
      let deadLetters = Map.findWithDefault [] "otlp_deadletter" dlq.logs
      (length deadLetters, null dlq.pending, Map.lookup ("otlp_logs", 0) dlq.committed) `shouldBe` (5, True, Just 5)
      (snd <$> find ((== "monoscope-attempt-count") . fst) (foldMap (K.headersToList . (.crHeaders)) (take 1 deadLetters))) `shouldBe` Just "1"
      -- 2026-06-25 commit-starvation wedge: a no-ack chunk pinned the commit base while the poll
      -- cursor moved on, and librdkafka never redelivers in-session (a 7.6M-lag stall). The loop
      -- seeks back to the un-acked base; ≥2 calls proves the retry fired rather than a first-call ack.
      reseekVar <- seeded
      attemptVar <- newTVarIO (0 :: Int)
      healed <- drive tr reseekVar ["otlp_logs"] Queue.KafkaPrimary (\tuples _ -> atomically (stateTVar attemptVar \k -> (k, k + 1)) <&> \n -> Right (if n == 0 then ([], []) else (map fst tuples, []))) (committedTo 5 reseekVar)
      Map.lookup ("otlp_logs", 0) healed.committed `shouldBe` Just 5
      readTVarIO attemptVar >>= (`shouldSatisfy` (>= 2))
      -- Four normal 8MB chunks run concurrently within the shared byte budget.
      let tr4 = tr{trATCtx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.kafkaGroupConcurrency = 4}}}
      fourVar <- newTVarIO $ foldl' (\acc _ -> appendRecord "otlp_logs" (Just $ BS.replicate (8 * 1024 * 1024) 0) (K.headersFromList []) acc) emptyBroker [1 .. 8 :: Int]
      entered <- newTVarIO (0 :: Int)
      four <- drive tr4 fourVar ["otlp_logs"] Queue.KafkaPrimary (\tuples attrs -> atomically (modifyTVar' entered (+ 1)) >> atomically (readTVar entered >>= check . (== 4)) >> ackAll tuples attrs) (committedTo 8 fourVar)
      (,Map.lookup ("otlp_logs", 0) four.committed) <$> readTVarIO entered `shouldReturn` (4, Just 8)
      -- A 64MB chunk holding the shared processing capacity stalls a second consumer, which
      -- resumes once it is released; both commit without losing offsets.
      large <- newTVarIO $ appendRecord "otlp_logs" (Just $ BS.replicate (64 * 1024 * 1024) 0) (K.headersFromList []) emptyBroker
      small <- newTVarIO emptyBroker
      seedTopic small "otlp_logs" [0]
      (largeEntered, resumed, release) <- (,,) <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar
      withAsync (drive tr large ["otlp_logs"] Queue.KafkaPrimary (\tuples attrs -> liftIO (putMVar largeEntered () >> takeMVar release) >> ackAll tuples attrs) (committedTo 1 large)) \largeWorker -> do
        timeout 2_000_000 (takeMVar largeEntered) `shouldReturn` Just ()
        withAsync (drive tr small ["otlp_logs"] Queue.KafkaPrimary (\tuples attrs -> liftIO (putMVar resumed ()) >> ackAll tuples attrs) (committedTo 1 small)) \smallWorker -> do
          timeout 2_000_000 (atomically $ readTVar small >>= \s -> check (Map.lookup ("otlp_logs", 0) s.cursor == Just 1)) `shouldReturn` Just ()
          timeout 100_000 (takeMVar resumed) `shouldReturn` Nothing
          putMVar release ()
          timeout 2_000_000 (takeMVar resumed) `shouldReturn` Just ()
          for_ ([largeWorker, smallWorker] :: [Async Broker]) $ wait >=> \w -> Map.lookup ("otlp_logs", 0) w.committed `shouldBe` Just 1

    -- 2026-07-09: during a 7h TimeFusion outage the DLQ replay consumer polled retry-tier
    -- records before their next-due-at; subChunksFor skipped them without seeding a tracker
    -- entry, a later due chunk seeded the commit base PAST them, and ~1.9M messages were
    -- committed over — never written nor escalated. A skipped record must also be processed once
    -- due within the session, not stranded until the next rebalance.
    it "kafka DLQ replay: not-yet-due records are redelivered once due, never committed over" \tr -> do
      now <- floor . toRational <$> getPOSIXTime
      let committedAtLeast n bVar = readTVar bVar >>= \s -> check (maybe False (>= n) (Map.lookup ("otlp_deadletter", 0) s.committed))
      overtaken <- newTVarIO emptyBroker
      alone <- newTVarIO emptyBroker
      for_ ([overtaken, alone] :: [TVar Broker]) \bVar -> seedTopicDue bVar "otlp_deadletter" (now + 2) [0 .. 4]
      receivedVar <- newTVarIO ([] :: [ByteString])
      -- 1s in, after the first poll skipped 0..4, already-due records land behind them.
      _ <- forkIO $ threadDelay 1_000_000 >> seedTopicDue overtaken "otlp_deadletter" (now - 1) [5 .. 9]
      withAsync (drive tr alone ["otlp_deadletter"] Queue.KafkaDlqReplay (\tuples _ -> pure (Right (map fst tuples, []))) (committedAtLeast 5 alone)) \aloneWorker -> do
        b <- drive tr overtaken ["otlp_deadletter"] Queue.KafkaDlqReplay (\tuples _ -> atomically (modifyTVar' receivedVar (<> map snd tuples)) $> Right (map fst tuples, [])) (committedAtLeast 10 overtaken)
        -- Committing 10 declares 0..9 durable and survives restarts: anything unprocessed is lost.
        Map.lookup ("otlp_deadletter", 0) b.committed `shouldBe` Just 10
        sort <$> readTVarIO receivedVar `shouldReturn` sort (map (encodeUtf8 . show) [0 .. 9 :: Int64])
        -- Due 2s in with a 15s backstop: Nothing here is the stranding bug.
        wait aloneWorker >>= \a -> Map.lookup ("otlp_deadletter", 0) a.committed `shouldBe` Just 5


testTimeRange :: (Text, Text)
testTimeRange = (toText $ iso8601Show $ addUTCTime (-3600) frozenTime, toText $ iso8601Show $ addUTCTime 3600 frozenTime)


queryLogs :: TestResources -> Projects.ProjectId -> Maybe Text -> IO Log.LogResult
queryLogs tr pid q = call tr (Log.logExplorerDataH pid def{Log.query = q, Log.from = Just (fst testTimeRange), Log.to = Just (snd testTimeRange)})


sessionsView :: TestResources -> Projects.ProjectId -> Maybe Text -> Maybe Text -> IO Log.SessionsView
sessionsView tr pid q order = call tr (Log.logSessionsH pid q Nothing (Just $ fst testTimeRange) (Just $ snd testTimeRange) Nothing order Nothing)


metricsQuery :: TestResources -> Projects.ProjectId -> Maybe Charts.DataType -> Text -> Text -> IO Charts.MetricsData
metricsQuery tr pid dataType source q = runQueryEffect tr $ Charts.queryMetrics Nothing dataType (Just pid) (Just q) Nothing Nothing (Just $ fst testTimeRange) (Just $ snd testTimeRange) (Just source) Nothing []


tf :: HI.DecodeResult a => TestResources -> HI.Sql -> IO a
tf tr = runTestBg frozenTime tr . Hasql.withHasqlTimefusion True . Hasql.interp


-- | An array column's @::text@ as numbers, in both the Postgres (@{1,3,5}@) and real
-- TimeFusion (@[1, 3, 5]@) renderings.
parseNumArray :: Text -> [Double]
parseNumArray = mapMaybe (readMaybe . toString) . filter (not . T.null) . T.split (`elem` (",{}[] " :: [Char]))


emptySpan :: UTCTime -> Telemetry.OtelLogsAndSpans
emptySpan now = Telemetry.OtelLogsAndSpans{project_id = testPid.toText, id = "", timestamp = now, observed_timestamp = Nothing, context = Nothing, level = Nothing, severity = Nothing, body = Nothing, attributes = Nothing, resource = Nothing, hashes = Nothing, kind = Nothing, status_code = Nothing, status_message = Nothing, start_time = now, end_time = Nothing, events = Nothing, links = Nothing, duration = Nothing, name = Nothing, parent_id = Nothing, summary = V.empty, date = now, errors = Nothing, message_size_bytes = 0}


-- | A server span declaring http.route @r@ for url.path @p@.
route :: Text -> Text -> UTCTime -> Telemetry.OtelLogsAndSpans
route r p now = (emptySpan now){Telemetry.kind = Just "server", Telemetry.attributes = Just $ AesonText $ fromList [("http", AE.object ["request" AE..= AE.object ["method" AE..= ("GET" :: Text)], "route" AE..= r]), ("url", AE.object ["path" AE..= p])]}


logsAttrs :: HM.HashMap Text Text
logsAttrs = HM.singleton "ce-type" "org.opentelemetry.otlp.logs.v1"


-- | The ack id in the body is what makes a row attributable to one message.
validLogMsg :: Text -> Text -> (Text, ByteString)
validLogMsg apiKey ackId = (ackId, encodeMessage $ createOtelLogAtTime apiKey ["valid record from " <> ackId] frozenTime)


corruptMsg :: Text -> (Text, ByteString)
corruptMsg ackId = (ackId, "this is not a valid protobuf payload")


-- | Rows one message wrote to the demo project, in the store the write leg targets. Scoped to
-- the ack id, not the project: CI shards share ONE TimeFusion and 31 spec files write to the
-- nil demo project, which is what made a project-wide count fail in CI but pass locally. The
-- hasql TF pool is read when TF writes are on (trPool always points at the main DB); a genuine
-- duplicate of THIS message still counts 2.
countDemoSpans :: TestResources -> Text -> IO Int
countDemoSpans tr ackId =
  fromIntegral @Int64
    <$> runTestBg frozenTime tr (Hasql.withHasqlTimefusion tr.trATCtx.config.enableTimefusionWrites (Hasql.interpScalar (rawSql $ "SELECT count(*)::int8 FROM otel_logs_and_spans WHERE project_id = '" <> testPid.toText <> "' AND body::text LIKE '%valid record from " <> ackId <> "%'")))


-- | TF writes on, with the main and/or TimeFusion hasql pool pointed at a closed loopback
-- port, so acquisition fails with ECONNREFUSED and write failures need no second DB.
breakPools :: Bool -> Bool -> TestResources -> IO TestResources
breakPools pgDown tfDown tr = do
  let ctx = tr.trATCtx
      broken down pool = if down then mkHasqlPool 1 "postgresql://postgres:postgres@127.0.0.1:1/postgres" else pure pool
  pg <- broken pgDown ctx.hasqlPool
  tfPool <- broken tfDown ctx.hasqlTimefusionPool
  pure tr{trATCtx = ctx{hasqlPool = pg, hasqlTimefusionPool = tfPool, env = ctx.env{enableTimefusionWrites = True}, config = ctx.config{enableTimefusionWrites = True}}}


-- | A TimeFusion pool on the test DB whose connections carry @options@ (e.g. a GUC a test trigger keys on).
withPgBackedTf :: TestResources -> ByteString -> (TestResources -> IO a) -> IO a
withPgBackedTf tr options action = bracket (mkHasqlPool 1 $ tr.trConnStr <> options) OHasql.release \pool ->
  let ctx = tr.trATCtx
   in action tr{trATCtx = ctx{hasqlTimefusionPool = pool, hasqlTimefusionUsesPgTypes = True, env = ctx.env{enableTimefusionWrites = True}, config = ctx.config{enableTimefusionWrites = True}}}


silentTfDDL :: [Query]
silentTfDDL =
  [ "CREATE OR REPLACE FUNCTION test_silent_tf_discard() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF current_setting('test.silent_tf', true) = 'on' THEN RETURN NULL; END IF; RETURN NEW; END $$"
  , "DROP TRIGGER IF EXISTS test_silent_tf ON otel_logs_and_spans"
  , "CREATE TRIGGER test_silent_tf BEFORE INSERT ON otel_logs_and_spans FOR EACH ROW EXECUTE FUNCTION test_silent_tf_discard()"
  ]


-- | A hasql server error with the given SQLSTATE.
serverError :: Text -> Hasql.HasqlException
serverError code = Hasql.HasqlException $ HP.SessionUsageError (HE.StatementSessionError 1 0 "select 1" [] True (HE.ServerStatementError (HE.ServerError code "Server error" Nothing Nothing Nothing)))


withCapturedLogs :: TestResources -> (TestResources -> IO a) -> IO (a, [LogMessage])
withCapturedLogs tr action = do
  messages <- LogList.newLogList
  result <- LogList.withLogListLogger messages \logger -> action tr{trLogger = logger, trATCtx = tr.trATCtx{config = tr.trATCtx.config{logLevel = LogBase.LogTrace}}}
  (result,) <$> LogList.getLogList messages


logField :: AE.Key -> LogMessage -> Maybe AE.Value
logField key message = case message.lmData of AE.Object fields -> KM.lookup key fields; _ -> Nothing


batchIdOf :: LogMessage -> Maybe UUID.UUID
batchIdOf message = logField "write.batch_id" message >>= \case AE.String v -> UUID.fromText v; _ -> Nothing


logsNamed :: Text -> [LogMessage] -> [LogMessage]
logsNamed name = filter ((== name) . (.lmMessage))


-- | An in-memory Kafka shared by the producer and consumer interpreters: each topic is an
-- append-only log (offset = index). Produced records wait in @pending@ (librdkafka's async send
-- queue) until 'FlushProducer' makes them durable, so the consumer's flush-before-commit gate is
-- exercised end to end.
data Broker = Broker
  { logs :: Map Text [K.ConsumerRecord (Maybe ByteString) (Maybe ByteString)]
  , pending :: [(Text, Maybe ByteString, K.Headers)]
  , cursor :: Map (Text, Int) Int
  , committed :: Map (Text, Int) Int64
  }


emptyBroker :: Broker
emptyBroker = Broker mempty mempty mempty mempty


appendRecord :: Text -> Maybe ByteString -> K.Headers -> Broker -> Broker
appendRecord topic val hdrs b =
  let existing = Map.findWithDefault [] topic b.logs
   in b{logs = Map.insert topic (existing <> [K.ConsumerRecord (K.TopicName topic) (K.PartitionId 0) (K.Offset $ fromIntegral $ length existing) K.NoTimestamp hdrs Nothing val]) b.logs}


runMockProducer :: IOE :> es => TVar Broker -> Eff (KafkaProducer ': es) a -> Eff es a
runMockProducer bVar = interpret \_ -> \case
  ProduceMessage KP.ProducerRecord{KP.prTopic = topic, KP.prValue = val, KP.prHeaders = hdrs} -> atomically $ modifyTVar' bVar \b -> b{pending = b.pending <> [(topic.unTopicName, val, hdrs)]}
  FlushProducer -> atomically $ modifyTVar' bVar \b -> (foldl' (\acc (t, v, h) -> appendRecord t v h acc) b b.pending){pending = []}
  -- Spelled out so a kafka-effectful bump that adds an op fails to compile here too (see cabal.project).
  AskProducerHandle -> error "runMockProducer: AskProducerHandle"
  ProduceMessage' _ _ -> error "runMockProducer: ProduceMessage'"
  ProduceMessageSync _ -> error "runMockProducer: ProduceMessageSync"
  ProduceMessageBatch _ -> error "runMockProducer: ProduceMessageBatch"
  InitTransactions _ -> error "runMockProducer: InitTransactions"
  BeginTransaction -> error "runMockProducer: BeginTransaction"
  CommitTransaction _ -> error "runMockProducer: CommitTransaction"
  AbortTransaction _ -> error "runMockProducer: AbortTransaction"
  SendOffsetsToTransaction{} -> error "runMockProducer: SendOffsetsToTransaction"


-- | Reads the subscribed @topics@ in offset order; commit records the high-water offset per (topic, 0).
runMockConsumer :: IOE :> es => [Text] -> TVar Broker -> Eff (KafkaConsumer ': es) a -> Eff es a
runMockConsumer topics bVar = interpret \_ -> \case
  PollMessage _ -> viaNonEmpty head <$> pullBatch 1
  PollMessageBatch _ (K.BatchSize n) -> map Right <$> pullBatch n
  CommitPartitionsOffsets _ tps -> atomically $ modifyTVar' bVar \b -> b{committed = foldl' recordTp b.committed tps}
  SeekPartitions tps _ -> atomically $ modifyTVar' bVar \b -> b{cursor = foldl' seekCursor b.cursor tps}
  PausePartitions _ -> pass
  ResumePartitions _ -> pass
  Assignment -> pure $ Map.fromList [(K.TopicName t, [K.PartitionId 0]) | t <- topics]
  _ -> error "runMockConsumer: unmocked consumer op"
  where
    pullBatch n = do
      batch <- atomically do
        b <- readTVar bVar
        let pull (acc, st) topic
              | length acc >= n = (acc, st)
              | otherwise =
                  let c = Map.findWithDefault 0 (topic, 0) st
                      taken = take (n - length acc) (drop c (Map.findWithDefault [] topic b.logs))
                   in (acc <> taken, Map.insert (topic, 0) (c + length taken) st)
            (recs, cursor') = foldl' pull ([], b.cursor) topics
        writeTVar bVar b{cursor = cursor'}
        pure recs
      when (null batch) (threadDelay 20_000)
      pure batch
    recordTp m (K.TopicPartition (K.TopicName t) (K.PartitionId p) (K.PartitionOffset o)) = Map.insertWith max (t, p) o m
    recordTp m _ = m
    seekCursor c (K.TopicPartition (K.TopicName t) (K.PartitionId p) (K.PartitionOffset o)) = Map.insert (t, fromIntegral p) (fromIntegral o) c
    seekCursor c _ = c


-- | Run decoupledLoop against the in-memory broker until @done@ fires (15s backstop); returns the final broker.
drive :: TestResources -> TVar Broker -> [Text] -> Queue.KafkaRole -> ([(Text, ByteString)] -> HashMap Text Text -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))) -> STM () -> IO Broker
drive tr bVar topics role fn done = do
  let loop = runBackground tr.trLogger tr.trATCtx tr.trTracerProvider $ void $ runErrorNoCallStack @K.KafkaError $ runMockProducer bVar $ runMockConsumer topics bVar $ Queue.decoupledLoop tr.trLogger tr.trATCtx tr.trTracerProvider role 1000 "test-client" "otlp_deadletter" fn
  void $ timeout 15_000_000 $ race_ loop (atomically done)
  readTVarIO bVar


seedTopic :: TVar Broker -> Text -> [Int64] -> IO ()
seedTopic bVar topic = atomically . modifyTVar' bVar . flip (foldl' (\acc o -> appendRecord topic (Just (encodeUtf8 (show o))) (K.headersFromList []) acc))


-- | Records stamped @monoscope-next-due-at@ (epoch seconds) as every retry-tier publish is;
-- KafkaDlqReplay's subChunksFor skips them while now < due.
seedTopicDue :: TVar Broker -> Text -> Int -> [Int64] -> IO ()
seedTopicDue bVar topic due = atomically . modifyTVar' bVar . flip (foldl' (\acc o -> appendRecord topic (Just (encodeUtf8 (show o))) (K.headersFromList [("monoscope-next-due-at", encodeUtf8 (show due))]) acc))
