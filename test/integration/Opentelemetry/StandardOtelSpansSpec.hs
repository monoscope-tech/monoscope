module Opentelemetry.StandardOtelSpansSpec (spec) where

import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..), fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- | Ingest a standard OTel HTTP span (as produced by auto-instrumentation, NOT our SDK).
ingestStdOtelSpan :: TestResources -> Text -> Text -> Text -> Text -> Int -> Text -> IO ()
ingestStdOtelSpan tr apiKey spanName method urlPath statusCode serverAddr = do
  trId <- show <$> nextRandom
  spanId' <- show <$> nextRandom
  let attrs =
        [ mkAttr "http.request.method" method
        , mkAttr "http.response.status_code" (show statusCode)
        , mkAttr "url.path" urlPath
        , mkAttr "server.address" serverAddr
        ]
      resource = mkResource apiKey [mkAttr "telemetry.sdk.name" "opentelemetry", mkAttr "telemetry.sdk.language" "nodejs"]
      req = mkSpanRequest trId spanId' Nothing spanName [] Nothing attrs resource frozenTime
  void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto req)


spec :: Spec
spec = around withTestResources do
  describe "Standard OpenTelemetry HTTP Spans" do
    it "normalizes HTTP spans from legacy and current language SDK conventions" \tr -> do
      apiKey <- createTestAPIKey tr pid "otel-language-conventions"
      let cases :: [(Text, PT.Span'SpanKind, [PC.KeyValue], Text, Text, Text, Text)]
          cases =
            [ ("java", PT.Span'SPAN_KIND_SERVER, [mkAttr "http.method" "GET", mkAttr "http.target" "/orders/42?expand=true", mkAttr "net.host.name" "api.java.test", mkAttr "http.route" "/orders/{id}"], "/orders/42", "api.java.test", "GET", "/orders/{id}")
            , ("python", PT.Span'SPAN_KIND_SERVER, [mkAttr "http.request.method" "GET", mkAttr "url.path" "/orders/43", mkAttr "server.address" "api.python.test"], "/orders/43", "api.python.test", "GET", "/orders/{number}")
            , ("nodejs", PT.Span'SPAN_KIND_CLIENT, [mkAttr "http.request.method" "POST", mkAttr "url.full" "https://api.node.test/v1/send/44?expand=true"], "/v1/send/44", "api.node.test", "POST", "/v1/send/{number}")
            , ("go", PT.Span'SPAN_KIND_CLIENT, [mkAttr "http.request.method" "GET", mkAttr "url.full" "https://api.go.test/orders/45", mkAttr "url.template" "/orders/:id"], "/orders/45", "api.go.test", "GET", "/orders/{id}")
            ]
      forM_ cases \(language, kind, attrs, path, host, method, endpointPath) -> do
        trId <- show <$> nextRandom
        spanId' <- show <$> nextRandom
        let name = language <> " HTTP"
            resource = mkResource apiKey [mkAttr "telemetry.sdk.language" language, mkAttr "service.name" "fallback-svc"]
            req = withSpanKind kind $ mkSpanRequest trId spanId' Nothing name [] Nothing attrs resource frozenTime
        void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto req)
        rows <- withPool tr.trPool $ DBT.query [sql|
          SELECT attributes___url___path, attributes___http___route, attributes___server___address, attributes___http___request___method,
                 attributes->'monoscope'->'endpoint'->>'route'
          FROM otel_logs_and_spans WHERE project_id = ? AND name = ?
        |] (pid, name) :: IO (V.Vector (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text))
        rows `shouldBe` V.singleton (Just path, Just endpointPath, Just host, Just method, Just endpointPath)

    it "recovers a missing HTTP method from the span name without assuming GET" \tr -> do
      apiKey <- createTestAPIKey tr pid "otel-method-from-name"
      forM_ ([("POST /orders/{id}", "POST", "/orders/{id}", True, True), ("HTTP /orders/{id}", "_OTHER", "/orders/{id}", True, True), ("HTTP /orders/42", "_OTHER", "/orders/{number}", False, True), ("PATCH /orders/{id}", "PATCH", "/orders/{id}", False, False)] :: [(Text, Text, Text, Bool, Bool)]) \(name, expectedMethod, expectedRoute, hasRoute, hasPath) -> do
        trId <- show <$> nextRandom
        spanId' <- show <$> nextRandom
        let attrs = [mkAttr "url.path" "/orders/42" | hasPath] <> [mkAttr "http.route" "/orders/{id}" | hasRoute]
            req = withSpanKind PT.Span'SPAN_KIND_SERVER $ mkSpanRequest trId spanId' Nothing name [] Nothing attrs (mkResource apiKey []) frozenTime
        void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto req)
        rows <- withPool tr.trPool $ DBT.query [sql|
          SELECT attributes___http___request___method, attributes___http___route FROM otel_logs_and_spans
          WHERE project_id = ? AND name = ?
        |] (pid, name) :: IO (V.Vector (Maybe Text, Maybe Text))
        rows `shouldBe` V.singleton (Just expectedMethod, Just expectedRoute)

    -- #631 re-keyed every endpoint of a project whose server spans carry no host
    -- (Talstack's `POST /login` was announced "new" again), and minted `_OTHER`
    -- endpoints for Express middleware and browser documentFetch spans.
    it "hostlessServerSpan_keepsServiceNameIdentity_andInternalSpansMintNoEndpoint" \tr -> do
      apiKey <- createTestAPIKey tr pid "otel-endpoint-identity"
      let resource = mkResource apiKey [mkAttr "service.name" "identity-svc"]
          spans :: [(PT.Span'SpanKind, Text, [PC.KeyValue])]
          spans =
            [ (PT.Span'SPAN_KIND_SERVER, "POST", [mkAttr "http.request.method" "POST", mkAttr "url.path" "/identity/login"])
            , (PT.Span'SPAN_KIND_INTERNAL, "request middleware - /identity/:id", [mkAttr "http.route" "/identity/:id", mkAttr "express.type" "middleware"])
            , (PT.Span'SPAN_KIND_INTERNAL, "request handler - /identity/:id", [mkAttr "http.request.method" "_OTHER", mkAttr "http.route" "/identity/:id", mkAttr "server.address" "", mkAttr "express.type" "request_handler"])
            , (PT.Span'SPAN_KIND_INTERNAL, "documentFetch", [mkAttr "http.request.method" "GET", mkAttr "url.full" "https://identity.example.com/identity/page"])
            ]
      forM_ spans \(kind, name, attrs) -> do
        trId <- show <$> nextRandom
        spanId' <- show <$> nextRandom
        void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ withSpanKind kind $ mkSpanRequest trId spanId' Nothing name [] Nothing attrs resource frozenTime)
      drainExtractionWorker tr
      endpoints <- withPool tr.trPool $ DBT.query [sql|
        SELECT method, host, url_path FROM apis.endpoints WHERE project_id = ? AND url_path LIKE '/identity%'
      |] (Only pid) :: IO (V.Vector (Text, Text, Text))
      endpoints `shouldBe` V.singleton ("POST", "identity-svc", "/identity/login")
      hosts <- withPool tr.trPool $ DBT.query [sql|
        SELECT attributes___server___address FROM otel_logs_and_spans
        WHERE project_id = ? AND name IN ('POST', 'request handler - /identity/:id')
        ORDER BY name
      |] (Only pid) :: IO (V.Vector (Only (Maybe Text)))
      hosts `shouldBe` V.fromList [Only (Just "identity-svc"), Only (Just "identity-svc")]

    it "ingests standard OTel HTTP span preserving original name" \tr -> do
      apiKey <- createTestAPIKey tr pid "std-otel-key"
      ingestStdOtelSpan tr apiKey "GET /api/users" "GET" "/api/users/550e8400-e29b-41d4-a716-446655440000" 200 "api.example.com"

      -- Original span name is preserved (not renamed to monoscope.http)
      spans <- withPool tr.trPool $ DBT.query [sql|
        SELECT name FROM otel_logs_and_spans
        WHERE project_id = ? AND attributes___http___request___method IS NOT NULL AND name = 'GET /api/users'
        ORDER BY timestamp DESC LIMIT 5
      |] (Only pid) :: IO (V.Vector (Only Text))
      V.length spans `shouldSatisfy` (>= 1)

    it "creates endpoints from standard OTel spans after processing" \tr -> do
      apiKey <- createTestAPIKey tr pid "std-otel-endpoint-key"
      replicateM_ 3 $ ingestStdOtelSpan tr apiKey "GET /api/orders" "GET" "/api/orders/12345" 200 "orders.example.com"
      replicateM_ 3 $ ingestStdOtelSpan tr apiKey "POST /api/payments" "POST" "/api/payments" 201 "orders.example.com"

      drainExtractionWorker tr

      endpoints <- withPool tr.trPool $ DBT.query [sql|
        SELECT url_path, method, host FROM apis.endpoints
        WHERE project_id = ? AND host = 'orders.example.com'
        ORDER BY url_path
      |] (Only pid) :: IO (V.Vector (Text, Text, Text))

      V.length endpoints `shouldSatisfy` (>= 2)
      let paths = V.toList $ V.map (\(p, _, _) -> p) endpoints
      paths `shouldSatisfy` elem "/api/payments"
      paths `shouldSatisfy` elem "/api/orders/{number}"

    it "normalizes UUID path segments from standard OTel spans" \tr -> do
      apiKey <- createTestAPIKey tr pid "std-otel-uuid-key"
      replicateM_ 3 $ ingestStdOtelSpan tr apiKey "GET" "GET" "/admin/companies/ec8213d0-20e6-4225-bf05-5d8215193d9b/employee-details" 200 "admin.example.com"

      drainExtractionWorker tr

      endpoints <- withPool tr.trPool $ DBT.query [sql|
        SELECT url_path FROM apis.endpoints
        WHERE project_id = ? AND host = 'admin.example.com'
      |] (Only pid) :: IO (V.Vector (Only Text))

      V.toList (fmap fromOnly endpoints) `shouldSatisfy` elem "/admin/companies/{uuid}/employee-details"

    -- Regression: with both write flags off, writeTargetFor yields WriteBoth, and the
    -- test harness's "timefusion" pool is the same Postgres — so one export inserted the
    -- span twice. Exact count (not >= 1) is the point of this guard.
    it "singleExport_noRealTimefusion_insertsExactlyOneRow" \tr -> do
      apiKey <- createTestAPIKey tr pid "std-otel-nodup-key"
      ingestStdOtelSpan tr apiKey "GET /nodup" "GET" "/nodup" 200 "nodup.example.com"

      Only rowCount <- V.head <$> (withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*) FROM otel_logs_and_spans
        WHERE project_id = ? AND attributes___server___address = 'nodup.example.com'
      |] (Only pid) :: IO (V.Vector (Only Int)))
      rowCount `shouldBe` 1

    it "SDK spans still work correctly alongside standard OTel spans" \tr -> do
      apiKey <- createTestAPIKey tr pid "std-otel-mixed-key"
      ingestStdOtelSpan tr apiKey "GET /health" "GET" "/health" 200 "mixed.example.com"
      ingestTrace tr apiKey "apitoolkit-http-span" frozenTime

      -- Standard OTel span keeps original name (filter by unique host to isolate)
      Only otelCount <- V.head <$> (withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*) FROM otel_logs_and_spans
        WHERE project_id = ? AND name = 'GET /health'
          AND attributes___server___address = 'mixed.example.com'
      |] (Only pid) :: IO (V.Vector (Only Int)))
      otelCount `shouldBe` 1

      -- SDK span becomes monoscope.http (at least one exists globally — SDK has no unique host to filter by)
      Only sdkCount <- V.head <$> (withPool tr.trPool $ DBT.query [sql|
        SELECT COUNT(*) FROM otel_logs_and_spans
        WHERE project_id = ? AND name = 'monoscope.http'
      |] (Only pid) :: IO (V.Vector (Only Int)))
      sdkCount `shouldSatisfy` (>= 1)

    it "non-HTTP spans are NOT renamed to monoscope.http" \tr -> do
      apiKey <- createTestAPIKey tr pid "std-otel-nonhttp-key"
      trId <- show <$> nextRandom
      spanId' <- show <$> nextRandom
      let req = mkSpanRequest trId spanId' Nothing "db.query" [] Nothing [] (mkResource apiKey []) frozenTime
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto req)

      spans <- withPool tr.trPool $ DBT.query [sql|
        SELECT name FROM otel_logs_and_spans
        WHERE project_id = ? AND name = 'db.query'
      |] (Only pid) :: IO (V.Vector (Only Text))
      V.length spans `shouldSatisfy` (>= 1)

    it "getTraceDetails: single-span trace reports the span's own duration, not zero" \tr -> do
      -- Regression: the trace end-time fold used to ignore the first span's
      -- end_time, so a single-span trace reported duration 0.
      apiKey <- createTestAPIKey tr pid "std-otel-singlespan-key"
      trId <- show <$> nextRandom
      spanId' <- show <$> nextRandom
      let req = mkSpanRequest trId spanId' Nothing "single.span" [] Nothing [] (mkResource apiKey []) frozenTime
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto req)
      Only storedTrId <- V.head <$> (withPool tr.trPool $ DBT.query [sql|
        SELECT context___trace_id FROM otel_logs_and_spans
        WHERE project_id = ? AND name = 'single.span'
      |] (Only pid) :: IO (V.Vector (Only Text)))
      trDetailsM <- runTestBg frozenTime tr $ Telemetry.getTraceDetails False pid storedTrId (Just frozenTime) frozenTime
      fmap (\(t, ss) -> (t.traceDurationNs > 0, length ss)) trDetailsM `shouldBe` Just (True, 1)
