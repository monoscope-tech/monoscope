module Pages.TelemetrySpec (spec) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Lucid qualified
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.Telemetry (metricDetailUrl, metricExpandUrl)
import Pages.Telemetry qualified as Trace
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Relude
import Test.Hspec


spec :: Spec
spec = do
  describe "metric chart drawer" do
    it "preserves the selected metric source in the detail request" do
      metricDetailUrl testPid "system.cpu.utilization" "accounting" Nothing
        `shouldBe` "/p/" <> testPid.toText <> "/metrics/details/system.cpu.utilization/?metric_source=accounting"

    it "preserves the selected label in the detail request" do
      metricDetailUrl testPid "container.cpu.time" "accounting" (Just "attributes.service.name")
        `shouldBe` "/p/" <> testPid.toText <> "/metrics/details/container.cpu.time/?metric_source=accounting&label=attributes.service.name"

    it "shares an expanded chart through the metrics overview" do
      metricExpandUrl testPid "container.cpu.time" "accounting" (Just "attributes.service.name")
        `shouldBe` "/p/" <> testPid.toText <> "/metrics?tab=charts&metric_source=accounting&expand=container.cpu.time&label=attributes.service.name"

  -- Regression guard: the trace-overlay projection selected neither `kind` nor
  -- `status_code`, and 'traceSpanRecord' hard-coded both to Nothing. That silently pinned
  -- the trace header's Errors stat to 0 for every trace, and leaves a service graph with
  -- no way to tell a client hop from a server hop.
  around withTestResources $ describe "trace overlay projection" do
    it "traceSpanRecord_clientAndErrorSpans_decodeKindAndStatus" \tr -> do
      apiKey <- createTestAPIKey tr testPid "trace-kind-key"
      let hexId = T.replace "-" "" . UUID.toText <$> nextRandom
      (trId, rootSid, childSid) <- (,,) <$> hexId <*> hexId <*> hexId
      ingestSpanReq tr
        $ withSpanKind PT.Span'SPAN_KIND_SERVER
        $ mkSpanRequest trId rootSid Nothing "GET /checkout" [] Nothing [] (mkResource apiKey []) frozenTime
      ingestSpanReq tr
        $ withSpanStatus PT.Status'STATUS_CODE_ERROR
        $ withSpanKind PT.Span'SPAN_KIND_CLIENT
        $ mkSpanRequest trId childSid (Just rootSid) "pg.query" [] Nothing [] (mkResource apiKey []) frozenTime

      traceM' <- runTestBg frozenTime tr $ Telemetry.getTraceDetailsForView False testPid trId (Just frozenTime) frozenTime Nothing
      spans <- maybe (fail "expected the ingested trace to be found") (\(_, sps, _) -> pure sps) traceM'
      let spanNamed n = V.find (\s -> s.spanName == n) spans
      (show . (.kind) <$> spanNamed "GET /checkout") `shouldBe` Just ("Just SKServer" :: Text)
      (show . (.kind) <$> spanNamed "pg.query") `shouldBe` Just ("Just SKClient" :: Text)
      ((.status) <$> spanNamed "pg.query") `shouldBe` Just (Just Telemetry.SSError)

      -- ...and the user-visible consequence. This is the exact filter the trace header's
      -- Errors stat counts with; it was pinned to 0 for every trace before the fix.
      V.length (V.filter (\s -> s.status == Just Telemetry.SSError) spans) `shouldBe` 1

      -- The page still renders end-to-end with the widened projection.
      (_, page) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) Nothing Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      for_ ["Waterfall", "GET /checkout", "pg.query", ">Errors</", ">1<"] \text ->
        html `shouldSatisfy` T.isInfixOf text

    -- A 1300-span trace was 4.4MB of HTML and ~8s of browser parse, on top of a
    -- 56s cold read. The view now takes a page at a time and offers the next one.
    it "traceView_pagesSpans_andOffersTheNextPage" \tr -> do
      trId <- T.replace "-" "" . UUID.toText <$> nextRandom
      -- 60 spans under one root: enough to overflow the smallest page the handler
      -- honours (the limit clamps up to 50), cheap enough to insert directly.
      void $ withPool tr.trPool $ DBT.execute
        [sql| INSERT INTO otel_logs_and_spans
                (id, project_id, timestamp, start_time, name, context___trace_id, context___span_id, parent_id,
                 context, kind, status_code, summary)
              SELECT gen_random_uuid(), ?, ?, ?::timestamptz + (i || ' milliseconds')::interval, 'span-' || i, ?,
                     lpad(to_hex(i), 16, '0'), CASE WHEN i = 0 THEN NULL ELSE lpad(to_hex(0), 16, '0') END,
                     jsonb_build_object('trace_id', ?::text, 'span_id', lpad(to_hex(i), 16, '0')),
                     'SERVER', '200', '{}'
              FROM generate_series(0, 59) i |]
        (testPid, frozenTime, frozenTime, trId, trId)

      (_, firstPage) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) Nothing Nothing Nothing (Just 50)
      case firstPage of
        Trace.TraceDetails _ _ spans _ moreUrl -> do
          V.length spans `shouldBe` 50
          -- The next page is offered, and it asks for a bigger one — not the same one again.
          moreUrl `shouldSatisfy` maybe False (T.isInfixOf "spans=100")
        _ -> expectationFailure "expected a paged trace view"

      -- The count on screen advertises itself as partial, and the control is there to act on.
      let firstHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml firstPage
      firstHtml `shouldSatisfy` T.isInfixOf ">50+<"
      firstHtml `shouldSatisfy` T.isInfixOf "Load more spans"

      -- Following it yields the whole trace, and the offer goes away.
      (_, fullPage) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) Nothing Nothing Nothing (Just 100)
      case fullPage of
        Trace.TraceDetails _ _ spans _ moreUrl -> do
          V.length spans `shouldBe` 60
          moreUrl `shouldBe` Nothing
        _ -> expectationFailure "expected the full trace on the larger page"
      LT.toStrict (Lucid.renderText $ Lucid.toHtml fullPage) `shouldSatisfy` not . T.isInfixOf "Load more spans"

      -- A span past the first page is still findable: nav looks a span up by id,
      -- so capping it at the render page would anchor the panel on the root.
      (_, navToLate) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) (Just "000000000000003b") (Just "next") Nothing (Just 50)
      case navToLate of
        Trace.SpanDetails _ target _ -> target.name `shouldBe` Just "span-59"
        _ -> expectationFailure "expected the 59th span, not a fallback to the root"

      -- ...and a Load more pull doesn't re-arm the auto-open marker, which would
      -- drag the detail panel back to the first span on every click.
      let embeddedHtml p' = LT.toStrict $ Lucid.renderText $ Lucid.toHtml p'
      (_, firstEmbedded) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) Nothing Nothing (Just "true") Nothing
      embeddedHtml firstEmbedded `shouldSatisfy` T.isInfixOf "load[window.innerWidth"
      (_, moreEmbedded) <- testServant tr $ Trace.traceH testPid trId (Just frozenTime) Nothing Nothing (Just "true") (Just 100)
      embeddedHtml moreEmbedded `shouldSatisfy` not . T.isInfixOf "load[window.innerWidth"

  -- 'getMetricChartListData' dominates the /metrics page (640ms of ~850ms before the
  -- de-duplicate-then-aggregate rewrite). The rewrite splices the source/prefix filters into
  -- two CTEs instead of one query, so this pins both the shape of the result and that the
  -- filters still apply to every branch.
  around withTestResources $ describe "metric chart list" do
    it "getMetricChartListData_collapsesDuplicateLabelsAndHonoursFilters" \tr -> do
      -- Seeded directly: the unit under test is the aggregation, and one metric spread over
      -- several services with overlapping label sets is exactly the shape the rewrite changed.
      let seed name svc labels =
            withPool tr.trPool
              $ void
              $ DBT.execute
                [sql| INSERT INTO otel_metrics_meta
                        (project_id, metric_name, metric_type, metric_unit, metric_description,
                         service_name, scope_name, metric_labels, first_seen_at, last_seen_at,
                         first_timestamp, last_timestamp)
                      VALUES (?, ?, 'GAUGE', '1', 'seeded', ?, 'test', ?, now(), now(), now(), now())
                      ON CONFLICT DO NOTHING |]
                (testPid, name :: Text, svc :: Text, PGArray (labels :: [Text]))
      seed "http.server.duration" "gateway" ["http.method", "http.status"]
      seed "http.server.duration" "checkout" ["http.status", "http.route"]
      seed "process.cpu.time" "gateway" ["state"]

      rows <- runTestBg frozenTime tr $ Telemetry.getMetricChartListData testPid Nothing Nothing
      -- One row per metric name (not one per service, and not one per (metric, label) pair),
      -- with the union of that metric's labels, deduplicated and sorted.
      map (\r -> (r.metricName, V.toList r.metricLabels)) rows
        `shouldBe` [ ("http.server.duration", ["http.method", "http.route", "http.status"])
                   , ("process.cpu.time", ["state"])
                   ]

      -- Both filters are spliced into two separate CTEs by the rewrite, so check each applies.
      prefixed <- runTestBg frozenTime tr $ Telemetry.getMetricChartListData testPid Nothing (Just "process.")
      map (.metricName) prefixed `shouldBe` ["process.cpu.time"]

      scoped <- runTestBg frozenTime tr $ Telemetry.getMetricChartListData testPid (Just "checkout") Nothing
      map (\r -> (r.metricName, V.toList r.metricLabels)) scoped
        `shouldBe` [("http.server.duration", ["http.route", "http.status"])]

    -- The overview page renders 20 active metrics but used to read the whole catalogue to do
    -- it. These pin the three answers the paged query has to give at once.
    it "getMetricCatalogPage_pagesActiveMetrics_countsThemAll_andSplitsOffTheInactiveTail" \tr -> do
      let seedAt name lastSeen =
            withPool tr.trPool
              $ void
              $ DBT.execute
                [sql| INSERT INTO otel_metrics_meta
                        (project_id, metric_name, metric_type, metric_unit, metric_description,
                         service_name, scope_name, metric_labels, first_seen_at, last_seen_at,
                         first_timestamp, last_timestamp)
                      VALUES (?, ?, 'GAUGE', '1', 'seeded', 'gateway', 'test', '{}', ?, ?, ?, ?)
                      ON CONFLICT DO NOTHING |]
                (testPid, name :: Text, lastSeen :: UTCTime, lastSeen, lastSeen, lastSeen)
          cutoff = addUTCTime (-(7 * 24 * 3600)) frozenTime
          fresh = addUTCTime (-3600) frozenTime
          stale = addUTCTime (-(30 * 24 * 3600)) frozenTime
      forM_ ["a.one", "a.two", "a.three", "a.four", "a.five"] (`seedAt` fresh)
      forM_ ["z.old.one", "z.old.two"] (`seedAt` stale)

      -- Page 1: two of five active, the full active count, and every inactive metric.
      p1 <- runTestBg frozenTime tr $ Telemetry.getMetricCatalogPage testPid Nothing Nothing cutoff 2 0 True
      map (.metricName) (V.toList p1.active) `shouldBe` ["a.five", "a.four"]
      p1.activeTotal `shouldBe` 5
      map (.metricName) (V.toList p1.inactive) `shouldBe` ["z.old.one", "z.old.two"]

      -- Page 2 continues the same ordering and reports the same total...
      p2 <- runTestBg frozenTime tr $ Telemetry.getMetricCatalogPage testPid Nothing Nothing cutoff 2 2 False
      map (.metricName) (V.toList p2.active) `shouldBe` ["a.one", "a.three"]
      p2.activeTotal `shouldBe` 5
      -- ...but must NOT drag the inactive tail along on every scroll request.
      map (.metricName) (V.toList p2.inactive) `shouldBe` []

    -- The service filter used to render every service as an <option> (2.3k of them, ~117KB
    -- of HTML, on the demo project). It is now searched and capped server-side.
    it "metricServices_areSearchedAndCappedServerSide_andNotInlinedIntoThePage" \tr -> do
      let seedSvc name svc =
            withPool tr.trPool
              $ void
              $ DBT.execute
                [sql| INSERT INTO otel_metrics_meta
                        (project_id, metric_name, metric_type, metric_unit, metric_description,
                         service_name, scope_name, metric_labels, first_seen_at, last_seen_at,
                         first_timestamp, last_timestamp)
                      VALUES (?, ?, 'GAUGE', '1', 'seeded', ?, 'test', '{}', now(), now(), now(), now())
                      ON CONFLICT DO NOTHING |]
                (testPid, name :: Text, svc :: Text)
      forM_ ([1 .. 80] :: [Int]) \i -> seedSvc "svc.metric" ("checkout-" <> show i)
      seedSvc "svc.metric" "billing-api"

      -- Capped, so a project with thousands of services can't blow up the response.
      allSvcs <- runTestBg frozenTime tr $ Telemetry.getMetricServiceNames testPid Nothing 50
      length allSvcs `shouldBe` 50
      -- ...and searchable, so the one you want is reachable despite the cap.
      matched <- runTestBg frozenTime tr $ Telemetry.getMetricServiceNames testPid (Just "billing") 50
      matched `shouldBe` ["billing-api"]

      (_, opts) <- testServant tr $ Trace.metricServicesGetH testPid (Just "billing") Nothing
      let optsHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml opts
      optsHtml `shouldSatisfy` T.isInfixOf "billing-api"
      optsHtml `shouldSatisfy` T.isInfixOf "All Services"

      -- The page itself ships the picker, not the services.
      (_, page) <- testServant tr $ Trace.metricsOverViewGetH testPid (Just "charts") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      let pageHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      pageHtml `shouldSatisfy` T.isInfixOf "/metrics/services"
      T.count "checkout-" pageHtml `shouldBe` 0
