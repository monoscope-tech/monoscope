-- | Metric <-> trace correlation through OTLP exemplars.
--
-- Ingestion has parsed exemplars since the metrics pipeline landed, but nothing
-- read them back: the trace id sitting in @otel_metrics.exemplars@ reached no
-- surface in the product. These tests pin both directions of the link that closes
-- that gap, end to end from an OTLP export to the rendered handler response.
module Pages.ExemplarsSpec (spec) where

import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.ByteString.Base16 qualified as B16
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid qualified
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Telemetry qualified as TelemetryPage
import Pkg.TestUtils
import Utils (formatUTC)
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Relude
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldNotSatisfy, shouldSatisfy)


-- | The trace the exemplar points at. Ingestion hex-decodes span/trace ids on the
-- way in and hex-encodes exemplar ids on the way out, so these literals are what
-- both sides end up storing — which is exactly what makes the join work.
traceIdHex :: Text
traceIdHex = "a1b2c3d4e5f60718293a4b5c6d7e8f90"


spanIdHex :: Text
spanIdHex = "0102030405060708"


metricName :: Text
metricName = "test.exemplar.gauge"


exemplarValue :: Double
exemplarValue = 42.5


-- | The exemplar carries raw bytes, not hex — @Exemplar.trace_id@ is @bytes@ in the
-- OTLP proto. Getting this wrong is invisible: ingestion would hex-encode the hex
-- string and the join would silently never match.
hexBytes :: Text -> ByteString
hexBytes = either (\e -> error $ "bad hex literal: " <> show e) id . B16.decode . encodeUtf8


nanosOf :: UTCTime -> Word64
nanosOf = floor . (* 1000000000) . utcTimeToPOSIXSeconds


isoAt :: Double -> Text
isoAt offset = toText $ iso8601Show $ addUTCTime (realToFrac offset) frozenTime


-- | Export one gauge datapoint carrying one exemplar that names the trace above.
ingestMetricWithExemplar :: TestResources -> Text -> IO ()
ingestMetricWithExemplar tr apiKey = do
  let exemplar :: PM.Exemplar
      exemplar =
        defMessage
          & PMF.timeUnixNano
          .~ nanosOf frozenTime
            & PMF.asDouble
          .~ exemplarValue
            & PMF.traceId
          .~ hexBytes traceIdHex
            & PMF.spanId
          .~ hexBytes spanIdHex
      point :: PM.NumberDataPoint
      point = defMessage & PMF.timeUnixNano .~ nanosOf frozenTime & PMF.asDouble .~ exemplarValue & PMF.exemplars .~ [exemplar]
      metric :: PM.Metric
      metric = defMessage & PMF.name .~ metricName & PMF.gauge .~ (defMessage & PMF.dataPoints .~ [point])
      request :: MS.ExportMetricsServiceRequest
      request =
        defMessage
          & MSF.resourceMetrics
          .~ [defMessage & PMF.resource .~ mkResource apiKey [] & PMF.scopeMetrics .~ [defMessage & PMF.metrics .~ [metric]]]
  void $ OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto request)


spec :: Spec
spec = around withTestResources $ describe "Metric/trace correlation via exemplars" do
  it "links a metric to the trace it was recorded in, and that trace back to the metric" \tr -> do
    apiKey <- createTestAPIKey tr testPid "exemplar-key"
    ingestSpanLinked tr apiKey traceIdHex spanIdHex Nothing "GET /exemplar" [] frozenTime
    ingestMetricWithExemplar tr apiKey

    -- Direction 1: metric -> trace. The Exemplars tab lists representative traces
    -- and deep-links each into the log explorer's trace overlay. The timestamp on
    -- that link is load-bearing: traceH without one widens to a 3-day scan.
    (_, exemplars) <- testServant tr $ TelemetryPage.metricExemplarsGetH testPid metricName (Just $ isoAt (-3600)) (Just $ isoAt 3600) Nothing
    let exemplarHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml exemplars
    exemplarHtml `shouldSatisfy` T.isInfixOf traceIdHex
    exemplarHtml `shouldSatisfy` T.isInfixOf "showTrace="
    exemplarHtml `shouldSatisfy` T.isInfixOf "timestamp"

    -- The chart overlay reads the same handler as JSON, so the diamonds and the
    -- rows beneath them can never disagree about which traces exist.
    let exemplarJson = decodeUtf8 $ AE.encode exemplars :: Text
    exemplarJson `shouldSatisfy` T.isInfixOf traceIdHex
    exemplarJson `shouldSatisfy` T.isInfixOf spanIdHex
    exemplarJson `shouldSatisfy` T.isInfixOf "\"value\":42.5"

    -- An exemplar whose own timestamp falls outside the window is dropped even
    -- though its row is inside it — cumulative histograms re-export exemplars that
    -- are weeks old, and a link to a trace past retention is worse than no link.
    (_, stale) <- testServant tr $ TelemetryPage.metricExemplarsGetH testPid metricName (Just $ isoAt 600) (Just $ isoAt 3600) Nothing
    LT.toStrict (Lucid.renderText $ Lucid.toHtml stale) `shouldNotSatisfy` T.isInfixOf traceIdHex

    -- Direction 2: span -> metrics. The detail panel's Metrics tab names the metric
    -- that recorded a datapoint inside this very trace, plus the emitting service's
    -- metrics as charts.
    rows <-
      withPool tr.trPool
        $ DBT.query
          [sql| SELECT id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ? |]
          (testPid, traceIdHex)
        :: IO (V.Vector (UUID.UUID, UTCTime))
    (rid, ts) <- maybe (error "ingested span missing from otel_logs_and_spans") pure (rows V.!? 0)

    -- The service tier reads the metric catalogue, which OTLP ingestion fills through
    -- an async buffer this test does not drive — so seed it directly. Without a row
    -- here the section renders its empty state and there is no chart to assert on.
    void
      $ withPool tr.trPool
      $ DBT.execute
        [sql| INSERT INTO otel_metrics_meta
                (project_id, metric_name, metric_type, metric_unit, metric_description,
                 service_name, scope_name, scope_version, first_seen_at, last_seen_at,
                 first_timestamp, last_timestamp, metric_labels)
              VALUES (?, ?, 'GAUGE', 'ms', '', 'test-service', '', '', ?, ?, ?, ?, '{}')
              ON CONFLICT DO NOTHING |]
        (testPid, metricName, ts, ts, ts, ts)

    (_, related) <- testServant tr $ TelemetryPage.relatedMetricsGetH testPid rid ts
    let relatedHtml = LT.toStrict $ Lucid.renderText related
    relatedHtml `shouldSatisfy` T.isInfixOf "Recorded in this trace"
    relatedHtml `shouldSatisfy` T.isInfixOf metricName
    relatedHtml `shouldSatisfy` T.isInfixOf "Metrics from test-service"

    -- Those charts are scoped to the span rather than to whatever range the page
    -- happens to show, and shade the span's own extent inside that window. Every
    -- vendor surveyed draws the interval; without it the chart cannot answer
    -- "was my request inside the spike?" and is decoration. `timeFrom` pins the
    -- query window, `highlightFrom` the band, and they are deliberately different
    -- values — the window is padded around the span.
    relatedHtml `shouldSatisfy` T.isInfixOf "timeFrom"
    relatedHtml `shouldSatisfy` T.isInfixOf "highlightFrom"
    let windowStart = formatUTC $ addUTCTime (-120) ts
        spanStart = formatUTC ts
    relatedHtml `shouldSatisfy` T.isInfixOf windowStart
    relatedHtml `shouldSatisfy` T.isInfixOf spanStart

    -- The panel offers the tab but pays for nothing: opening a span must not run a
    -- metrics lookup. Two levels of deferral, and the test pins both — the panel
    -- ships a placeholder, and the placeholder's own content is another shell.
    (_, item) <- testServant tr $ LogItem.expandAPIlogItemH testPid rid ts Nothing Nothing Nothing False
    let itemHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml item
    itemHtml `shouldSatisfy` T.isInfixOf "group-has-[.tab-metrics:checked]/dtab:block"
    itemHtml `shouldSatisfy` T.isInfixOf "tab=tab-metrics&amp;partial=true"
    itemHtml `shouldNotSatisfy` T.isInfixOf metricName
    (_, metricsTab) <- testServant tr $ LogItem.expandAPIlogItemH testPid rid ts Nothing (Just "tab-metrics") Nothing True
    let metricsTabHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml metricsTab
    metricsTabHtml `shouldSatisfy` T.isInfixOf "/related_metrics"
    metricsTabHtml `shouldNotSatisfy` T.isInfixOf metricName

  it "reports no exemplars for a metric that carries none, rather than failing" \tr -> do
    apiKey <- createTestAPIKey tr testPid "exemplar-empty-key"
    ingestMetric tr apiKey [] "test.no.exemplars" 1.0 frozenTime
    (_, exemplars) <- testServant tr $ TelemetryPage.metricExemplarsGetH testPid "test.no.exemplars" (Just $ isoAt (-3600)) (Just $ isoAt 3600) Nothing
    case exemplars of
      TelemetryPage.MetricExemplarsGet _ _ [] -> pass
      TelemetryPage.MetricExemplarsGet _ _ xs -> expectationFailure $ "expected no exemplars, got " <> show (length xs)
    LT.toStrict (Lucid.renderText $ Lucid.toHtml exemplars) `shouldSatisfy` T.isInfixOf "No exemplars"

  it "renders the service tier for a log, which carries no trace context at all" \tr -> do
    apiKey <- createTestAPIKey tr testPid "exemplar-log-key"
    ingestLog tr apiKey "a log with no trace" frozenTime
    ingestMetricWithExemplar tr apiKey
    rows <-
      withPool tr.trPool
        $ DBT.query [sql| SELECT id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND kind = 'log' |] (Only testPid)
        :: IO (V.Vector (UUID.UUID, UTCTime))
    (rid, ts) <- maybe (error "ingested log missing from otel_logs_and_spans") pure (rows V.!? 0)
    (_, related) <- testServant tr $ TelemetryPage.relatedMetricsGetH testPid rid ts
    let relatedHtml = LT.toStrict $ Lucid.renderText related
    relatedHtml `shouldSatisfy` T.isInfixOf "No metric reported a datapoint from this trace"
    relatedHtml `shouldSatisfy` T.isInfixOf "Metrics from test-service"
