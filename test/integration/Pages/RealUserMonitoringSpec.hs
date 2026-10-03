module Pages.RealUserMonitoringSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Pool (withResource)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Effectful (Eff, (:>))
import Effectful.Dispatch.Dynamic (interpose, send)
import Effectful.Labeled (Labeled (..))
import Hasql.Interpolate qualified as HI
import Hasql.Pool qualified as HP
import Lucid qualified
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.RUM qualified as RUMData
import Network.GRPC.Common.Protobuf (Proto (..))
import Network.HTTP.Types.URI (parseQueryText)
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Charts.Types (MetricsData)
import Pages.Components (Deferred (..))
import Pages.LogExplorer.Log qualified as Log
import Pages.RealUserMonitoring qualified as RUM
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Timeout (timeout)
import System.Types (ATAuthCtx, RespHeaders)
import Test.Hspec
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Exception qualified as E
import Utils (toXXHash)


-- TimeFusion's partition date is absent from the PostgreSQL mirror.
nativeDateColumn :: TestResources -> HI.Sql
nativeDateColumn tr = if tr.trATCtx.env.enableTimefusionWrites then [HI.sql|,date|] else mempty


replayUuid :: UUID.UUID
replayUuid = [uuid|00000000-0000-0000-0000-000000000042|]


emptyReplayUuid :: UUID.UUID
emptyReplayUuid = [uuid|00000000-0000-0000-0000-000000000043|]


mergedReplayUuid :: UUID.UUID
mergedReplayUuid = [uuid|00000000-0000-0000-0000-000000000044|]


sessionId :: Text
sessionId = UUID.toText replayUuid


browserSpan :: Text -> Text -> Text -> [(Text, Text)] -> Text -> Text -> Maybe Text -> Text -> TestResources -> IO ()
browserSpan apiKey trId spId extras name sid parentM service = browserSpanAt apiKey trId spId extras name sid parentM service frozenTime


browserSpanAt :: Text -> Text -> Text -> [(Text, Text)] -> Text -> Text -> Maybe Text -> Text -> UTCTime -> TestResources -> IO ()
browserSpanAt apiKey trId spId extras name sid parentM service at tr =
  ingestSpanReq tr $ mkSpanRequest trId spId parentM name [] Nothing (map (uncurry mkAttr) $ ("session.id", sid) : extras) (mkResource apiKey [mkAttr "telemetry.sdk.language" "webjs", mkAttr "service.name" service]) at


-- | A page load exactly as the OpenTelemetry browser SDK sends it, which is what production
-- actually looks like: span named @documentLoad@, the page in @url.full@ rather than
-- @url.path@, a user agent on the resource, and — the part that broke RUM — no
-- @telemetry.sdk.language@ at all.
otelBrowserSpan :: Text -> Text -> Text -> Text -> Text -> TestResources -> IO ()
otelBrowserSpan apiKey trId spId sid service tr =
  ingestSpanReq tr
    $ mkSpanRequest
      trId
      spId
      Nothing
      "documentLoad"
      []
      Nothing
      [mkAttr "session.id" sid, mkAttr "url.full" "https://shop.example/cart"]
      (mkResource apiKey [mkAttr "service.name" service, mkAttr "user_agent.original" "Mozilla/5.0 (X11; Linux x86_64) Chrome/151"])
      frozenTime


renderPage :: TestResources -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
renderPage tr tab query sessionFilterM selected = renderScoped tr tab query sessionFilterM selected Nothing


-- | Every panel fetches itself, so the page a user ends up looking at is the concatenation of
-- the panel responses. Asserting against that keeps these tests about what is on screen rather
-- than about which request delivered it.
renderScoped :: TestResources -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
renderScoped tr tab query sessionFilterM selected service =
  fmap fold . forM ["pulse", "pages", "vitals", "errors", "sessions", "audience"] $ \panel ->
    renderPanel tr tab query sessionFilterM selected service (Just panel)


renderPanel :: TestResources -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Text
renderPanel tr tab query sessionFilterM selected service panel =
  shellHtml (withService service tr) $ RUM.rumGetH testPid tab query sessionFilterM Nothing Nothing (Just "24H") selected Nothing panel (Just "1") Nothing


withService :: Maybe Text -> TestResources -> TestResources
withService service tr = tr{trSessAndHeader = fmap (\session -> session{Projects.service = service}) tr.trSessAndHeader}


htmlOf :: Lucid.ToHtml a => a -> Text
htmlOf = toStrict . Lucid.renderText . Lucid.toHtml


-- | The loaded panel body of a RUM response.
rumBody :: TestResources -> ATAuthCtx (RespHeaders RUM.RumGet) -> IO RUM.RumData
rumBody tr action = testServant tr action >>= \(_, RUM.RumGet (PageCtx _ body)) -> deferredBody body


-- | Every TimeFusion read fails to acquire a connection.
tfUnavailable :: Labeled "timefusion" Hasql.Hasql :> es => Eff es a -> Eff es a
tfUnavailable = interpose @(Labeled "timefusion" Hasql.Hasql) \_ (Labeled effect) -> case effect of
  Hasql.UseStatement{} -> pure $ Left HP.AcquisitionTimeoutUsageError
  Hasql.UseSession{} -> pure $ Left HP.AcquisitionTimeoutUsageError
  Hasql.UseLabeledSession{} -> pure $ Left HP.AcquisitionTimeoutUsageError


execSql :: TestResources -> PG.Query -> IO ()
execSql tr sql = withResource tr.trPool $ \conn -> void $ PG.execute_ conn sql


-- | Counts rum_panel_cache inserts made while the action runs.
withCacheWriteCounter :: TestResources -> Text -> (IO Int64 -> IO a) -> IO a
withCacheWriteCounter tr name body = E.bracket_ (execSql tr create) (execSql tr remove) (body count)
  where
    sequenceName = name <> "_writes"
    function = "count_" <> name <> "_write"
    create = fromString $ toString $ "CREATE SEQUENCE " <> sequenceName <> "; CREATE FUNCTION " <> function <> "() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM nextval('" <> sequenceName <> "'); RETURN NEW; END $$; CREATE TRIGGER " <> function <> " BEFORE INSERT ON rum_panel_cache FOR EACH ROW EXECUTE FUNCTION " <> function <> "()"
    remove = fromString $ toString $ "DROP TRIGGER " <> function <> " ON rum_panel_cache; DROP FUNCTION " <> function <> "(); DROP SEQUENCE " <> sequenceName
    count = withResource tr.trPool \conn -> do
      [(lastValue, isCalled)] <- PG.query_ conn (fromString $ toString $ "SELECT last_value, is_called FROM " <> sequenceName) :: IO [(Int64, Bool)]
      pure $ if isCalled then lastValue else 0


loadVitalPanel :: TestResources -> Projects.ProjectId -> Maybe Text -> Text -> IO RUM.RumData
loadVitalPanel tr projectId refresh = loadPerformance tr projectId Nothing Nothing (Just "24H") refresh . Just


-- | The loaded Performance tab body for a window, or for one panel of it.
loadPerformance :: TestResources -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO RUM.RumData
loadPerformance tr projectId from to since refresh panel = rumBody tr $ RUM.rumGetH projectId (Just "performance") Nothing Nothing from to since Nothing Nothing panel (Just "1") refresh


measured :: RUMData.VitalMeasurement -> (Maybe Double, Natural)
measured measurement = (RUMData.measurementValue measurement, RUMData.measurementSamples measurement)


-- | Both vital panels report LCP unavailable, in the summary and in every trend/page point.
lcpUnavailable :: TestResources -> Projects.ProjectId -> Natural -> RUMData.VitalCoverage -> Expectation
lcpUnavailable tr projectId count issue = forM_ ["vitals", "vital_trend"] \panel -> do
  page <- loadVitalPanel tr projectId Nothing panel
  page.degradedPanels `shouldBe` []
  (if panel == "vitals" then [vital.measurement | vital <- page.vitals, vital.name == "lcp"] else map (.measurement) page.vitalTrend <> map (.measurement) page.pageVitals)
    `shouldBe` replicate (if panel == "vitals" then 1 else 2) (RUMData.Unavailable count issue)


nanos :: UTCTime -> Word64
nanos = floor . (* 1000000000) . utcTimeToPOSIXSeconds


histogramPoint :: Text -> UTCTime -> UTCTime -> Word64 -> [Word64] -> [Double] -> PM.HistogramDataPoint
histogramPoint url end start count counts bounds = defMessage & PMF.timeUnixNano .~ nanos end & PMF.startTimeUnixNano .~ nanos start & PMF.attributes .~ [mkAttr "page.url" url] & PMF.count .~ count & PMF.bucketCounts .~ counts & PMF.explicitBounds .~ bounds


lcpHistogram :: PM.AggregationTemporality -> [PM.HistogramDataPoint] -> PM.Metric
lcpHistogram temporality points = defMessage & PMF.name .~ "browser.web_vital.lcp" & PMF.histogram .~ (defMessage & PMF.aggregationTemporality .~ temporality & PMF.dataPoints .~ points)


exportMetrics :: TestResources -> Text -> [PC.KeyValue] -> [PM.Metric] -> IO ()
exportMetrics tr apiKey resourceAttrs metrics =
  void $ OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ defMessage & MSF.resourceMetrics .~ [defMessage & PMF.resource .~ mkResource apiKey resourceAttrs & PMF.scopeMetrics .~ [defMessage & PMF.metrics .~ metrics]])


-- | Both cache layers: the shared rum_panel_cache table outlives a memory purge by design,
-- which is exactly what these examples must not inherit from each other.
purgeRumCaches :: TestResources -> IO ()
purgeRumCaches tr = do
  Cache.purge tr.trATCtx.rumCache
  withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM rum_panel_cache"


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Real User Monitoring" do
    it "emptyProject_explainsBrowserTelemetryAndOffersTheDashboard" \tr -> do
      shell <- shellHtml tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing Nothing
      shell `shouldContainAll` ["hx-trigger=\"load\"", "deferred=1", "skeleton-shimmer", "tabs tabs-box tabs-outline"]
      -- The shell stands in for the tab it is loading, so switching tabs does not reflow.
      sessionsShell <- shellHtml tr $ RUM.rumGetH testPid (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing Nothing
      sessionsShell `shouldContainAll` ["Loading sessions", "skeleton-shimmer"]
      html <- renderPage tr Nothing Nothing Nothing Nothing
      html `shouldContainAll` ["No browser telemetry yet", "Install the browser SDK", "Open RUM dashboard", "empty-state"]

    it "scalarVitals_summaryAndDetail_shareTheContinuousPopulationQuantile" \tr -> do
      projectId <- createTestProject tr "RUM scalar quantile"
      apiKey <- createTestAPIKey tr projectId "rum-scalar-key"
      forM_ (zip [1, 10, 20, 30] [-10, -9, -8, -7]) \(value, seconds) ->
        ingestMetric tr apiKey [] [mkAttr "page.url" "/scalar"] (if value == 30 then "k6.browser_web_vital_lcp" else "browser.web_vital.lcp") value (addUTCTime seconds frozenTime)
      detail <- loadVitalPanel tr projectId Nothing "vital_trend"
      summary <- loadVitalPanel tr projectId Nothing "vitals"
      (mapMaybe (RUMData.measurementValue . (.measurement)) detail.vitalTrend, [measured point.measurement | point <- detail.pageVitals], [measured vital.measurement | vital <- summary.vitals, vital.name == "lcp"])
        `shouldBe` ([22.5], [(Just 22.5, 4)], [(Just 22.5, 4)])

    it "syntheticHistograms_doNotOverwhelmRealUserVitals" \tr -> do
      projectId <- createTestProject tr "RUM synthetic histogram isolation"
      apiKey <- createTestAPIKey tr projectId "rum-synthetic-key"
      let at = (`addUTCTime` frozenTime)
          point count buckets = histogramPoint "/real" (at (-10)) (at (-60)) count buckets [100, 200]
      exportMetrics tr apiKey []
        [ lcpHistogram PM.AGGREGATION_TEMPORALITY_DELTA [point 10 [10, 0, 0]]
        , lcpHistogram PM.AGGREGATION_TEMPORALITY_DELTA [point 1000 [0, 1000, 0]] & PMF.name .~ "k6.browser_web_vital_lcp"
        ]
      page <- loadVitalPanel tr projectId Nothing "vitals"
      [measured vital.measurement | vital <- page.vitals, vital.name == "lcp"] `shouldBe` [(Just 75, 10)]

    it "vitalTrend_partialFirstBucket_staysInsideTheSelectedAxis" \tr -> do
      projectId <- createTestProject tr "RUM partial trend bucket"
      apiKey <- createTestAPIKey tr projectId "rum-partial-bucket-key"
      ingestMetric tr apiKey [] [mkAttr "page.url" "/partial-bucket"] "browser.web_vital.lcp" 175 (addUTCTime (-10) frozenTime)
      let fromQuery = Just $ toText $ iso8601Show $ addUTCTime (-31.125) frozenTime
          toQuery = Just $ toText $ iso8601Show $ addUTCTime (-1.25) frozenTime
      forM_ ([(Nothing, Nothing, Just "30S", -30, 0), (fromQuery, toQuery, Nothing, -31.125, -1.25), (Nothing, Nothing, Just "1H", -3600, 0)] :: [(Maybe Text, Maybe Text, Maybe Text, NominalDiffTime, NominalDiffTime)]) \(from, to, since, start, end) -> do
        page <- loadPerformance tr projectId from to since Nothing (Just "vital_trend")
        page.degradedPanels `shouldBe` []
        [measured vital.measurement | vital <- page.vitals, vital.name == "lcp"] `shouldBe` [(Just 175, 1)]
        widget <- renderedWidget $ htmlOf page
        let millis seconds = floor (1000 * utcTimeToPOSIXSeconds (addUTCTime seconds frozenTime)) :: Int
            plottedStart = max (millis start) (millis (-300))
        [(dataset.from, dataset.to, dataset.source) | dataset <- maybeToList widget.dataset]
          `shouldBe` [(Just $ millis start, Just $ millis end, AE.toJSON ([AE.toJSON (["timestamp", "P75"] :: [Text]), AE.toJSON (plottedStart, 175 :: Int)] :: [AE.Value]))]

    it "histogramVitals_useObservationBuckets_insteadOfExportMeans" \tr -> do
      let point end start count total low high counts = histogramPoint "/histogram" end start count counts [100, 200, 10000] & PMF.sum .~ total & PMF.min .~ low & PMF.max .~ high
          at = (`addUTCTime` frozenTime)
          delta = [point (at (-10)) (at (-60)) 100 7000 0 200 [80, 20, 0, 0], point (at (-9)) (at (-10)) 1 9000 9000 9000 [0, 0, 1, 0]]
          -- The previous export is outside the selected 24H window. The repeated export
          -- contributes nothing; a new start timestamp starts a fresh cumulative epoch.
          cumulative =
            [ point (at (-86401)) (at (-90000)) 100 7000 0 200 [80, 20, 0, 0]
            , point (at (-10)) (at (-90000)) 101 16000 0 9000 [80, 20, 1, 0]
            , point (at (-9)) (at (-90000)) 101 16000 0 9000 [80, 20, 1, 0]
            , point (at (-7)) (at (-8)) 100 7000 0 200 [80, 20, 0, 0]
            ]
          schemaDelta =
            [ point (at (-10)) (at (-60)) 100 7000 0 200 [80, 20, 0] & PMF.explicitBounds .~ [100, 200]
            , point (at (-9)) (at (-10)) 100 12000 0 200 [10, 90, 0] & PMF.explicitBounds .~ [50, 200]
            ]
          schemaCumulative =
            [ point (at (-86401)) (at (-90000)) 100 7000 0 200 [40, 60, 0] & PMF.explicitBounds .~ [50, 200]
            , point (at (-10)) (at (-90000)) 120 10000 0 200 [80, 40, 0] & PMF.explicitBounds .~ [100, 200]
            ]
          mixed = [point (at (-10)) (at (-60)) 100 7000 0 200 [80, 20, 0, 0]]
          mixedInside = [point (at (-10)) (at (-60)) 20 1000 0 100 [20, 0, 0, 0]]
          mixedOverflow = [point (at (-10)) (at (-60)) 20 1000 0 100 [20] & PMF.explicitBounds .~ []]
          disjoint = [point (at (-10)) (at (-60)) 100 7000 0 200 [80, 20] & PMF.explicitBounds .~ [100], point (at (-9)) (at (-10)) 100 7000 0 200 [80, 20] & PMF.explicitBounds .~ [200]]
          invalidReset = [point (at (-86401)) (at (-90000)) 100 7000 0 200 [80, 20, 0, 0], point (at (-10)) (at (-90000)) 90 6000 0 200 [70, 20, 0, 0]]
          missing = [point (at (-10)) (at (-90000)) 100 7000 0 200 [80, 20, 0, 0], point (at (-9)) (at (-90000)) 101 16000 0 9000 [80, 20, 1, 0]]
          unknownMarker = [point (at (-10)) (at (-10)) 100 7000 0 200 [80, 20, 0, 0], point (at (-9)) (at (-10)) 101 7100 0 200 [81, 20, 0, 0]]
          overflow = [point (at (-10)) (at (-60)) 100 1500000 10001 20000 [0, 0, 0, 100]]
          unknownStart = [point (at (-10)) (at (-60)) 100 7000 0 200 [80, 20, 0, 0] & PMF.startTimeUnixNano .~ 0]
          staleResume =
            [ point (at (-86401)) (at (-90000)) 100 7000 0 200 [80, 20, 0, 0]
            , point (at (-10)) (at (-90000)) 101 7100 0 200 [81, 20, 0, 0]
            , point (at (-9)) (at (-90000)) 1000 70000 0 200 [980, 20, 0, 0] & PMF.flags .~ 1
            , point (at (-8)) (at (-90000)) 102 7200 0 200 [82, 20, 0, 0]
            , point (at (-7)) (at (-90000)) 103 7300 0 200 [83, 20, 0, 0]
            ]
          staleNoEpoch = take 2 staleResume <> [point (at (-9)) (at (-90000)) 1000 70000 0 200 [980, 20, 0, 0] & PMF.flags .~ 1 & PMF.startTimeUnixNano .~ 0] <> drop 3 staleResume
          staleBeforeWindow =
            [ point (at (-86403)) (at (-90000)) 100 7000 0 200 [80, 20, 0, 0]
            , point (at (-86401)) (at (-90000)) 1000 70000 0 200 [980, 20, 0, 0] & PMF.flags .~ 1 & PMF.startTimeUnixNano .~ 0
            , point (at (-10)) (at (-90000)) 101 7100 0 200 [81, 20, 0, 0]
            , point (at (-9)) (at (-90000)) 102 7200 0 200 [82, 20, 0, 0]
            ]
          staleReset = take 3 staleNoEpoch <> [point (at (-7)) (at (-8)) 100 7000 0 200 [80, 20, 0, 0]]
          staleUnknownReset = take 3 staleNoEpoch <> [point (at (-8)) (at (-8)) 100 7000 0 200 [80, 20, 0, 0], point (at (-7)) (at (-8)) 101 7100 0 200 [81, 20, 0, 0]]
          cases =
            [ (PM.AGGREGATION_TEMPORALITY_DELTA, delta, [], (Just 94.6875, 101))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, cumulative, [], (Just 94.6875, 101))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, schemaDelta, [], (Just 150, 200))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, schemaCumulative, [], (Just 150, 20))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, mixed, [9000], (Just 94.6875, 101))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, missing, [], (Nothing, 1))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, unknownMarker, [], (Just 75, 1))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, overflow, [], (Nothing, 100))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, unknownStart, [], (Nothing, 0))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, staleResume, [], (Nothing, 2))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, staleNoEpoch, [], (Nothing, 2))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, staleBeforeWindow, [], (Nothing, 1))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, staleReset, [], (Just (75.75 / 81 * 100), 101))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, staleUnknownReset, [], (Just 75, 2))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, mixedInside, replicate 80 90, (Just 90, 100))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, mixedOverflow, replicate 70 90 <> replicate 10 100, (Nothing, 100))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, disjoint, [], (Nothing, 200))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, invalidReset, [], (Nothing, 0))
            , (PM.AGGREGATION_TEMPORALITY_UNSPECIFIED, mixed, [], (Nothing, 0))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, [point (at (-10)) (at (-60)) 100 7000 0 200 [80, 10, 0, 0]], [], (Nothing, 0))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, [point (at (-10)) (at (-60)) 100 7000 0 200 [80, 20, 0, 0], point (at (-9)) (at (-20)) 100 7000 0 200 [80, 20, 0, 0]], [], (Nothing, 100))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, [point (at (-10)) (at (-60)) 0 0 0 0 [0, 0, 0, 0]], [], (Nothing, 0))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, [point (at (-10)) (at (-60)) 1 50 0 100 [1, 0] & PMF.explicitBounds .~ [100]], [9000], (Just 9000, 2))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, schemaDelta, [150], (Just 150, 201))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, unknownStart, [], (Just 93.75, 100))
            , (PM.AGGREGATION_TEMPORALITY_DELTA, [point (at (-10)) (at (-9)) 100 7000 0 200 [80, 20, 0, 0]], [], (Nothing, 0))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, take 1 invalidReset <> [point (at (-10)) (at (-90000)) 0 0 0 0 [0, 0, 0, 0]], [], (Nothing, 0))
            , (PM.AGGREGATION_TEMPORALITY_CUMULATIVE, [point (at (-86401)) (at (-90000)) 0 0 0 0 [0, 0, 0, 0], point (at (-11)) (at (-90000)) 0 0 0 0 [0, 0, 0, 0], point (at (-10)) (at (-90000)) 4000000000000000000 4000000000000000000 0 100 [4000000000000000000, 0, 0, 0]], [], (Just 75, 4000000000000000000))
            ]
      pages <- forM cases \(temporality, points, scalars, _) -> do
        projectId <- createTestProject tr "RUM histogram"
        apiKey <- createTestAPIKey tr projectId "rum-histogram-key"
        let scoped = withService (Just "histogram-ui") tr
        exportMetrics tr apiKey [mkAttr "service.name" "histogram-ui"] [lcpHistogram temporality points]
        when (temporality == PM.AGGREGATION_TEMPORALITY_UNSPECIFIED)
          $ runTestBg frozenTime tr
          $ Hasql.withHasqlTimefusion True
          $ Hasql.interpExecute_
          $ [HI.sql|INSERT INTO otel_metrics(project_id,timestamp,ingested_at,metric_unit,dropped_attributes_count,message_size_bytes,id,series_id,start_timestamp,metric_name,metric_type,
              distribution_count,hist_bucket_counts,hist_explicit_bounds,attributes,resource,resource___service___name,flags|]
          <> nativeDateColumn tr
          <> [HI.sql|)
            SELECT project_id,timestamp-INTERVAL '1 second',ingested_at,metric_unit,dropped_attributes_count,message_size_bytes,id,series_id,start_timestamp,metric_name,metric_type,
              distribution_count,hist_bucket_counts,hist_explicit_bounds,attributes,resource,resource___service___name,flags|]
          <> nativeDateColumn tr
          <> [HI.sql|
            FROM otel_metrics WHERE project_id=#{projectId.toText}|]
        forM_ (zip scalars [0 :: Int ..]) \(value, index) -> ingestMetric tr apiKey [mkAttr "service.name" "histogram-ui"] [mkAttr "page.url" "/histogram"] "browser.web_vital.lcp" value (at (-6 + fromIntegral index / 1000))
        purgeRumCaches tr
        detail <- loadVitalPanel scoped projectId Nothing "vital_trend"
        summary <- loadVitalPanel scoped projectId Nothing "vitals"
        pure (detail, summary)
      map (\(detail, summary) -> (detail.degradedPanels, summary.degradedPanels)) pages `shouldBe` replicate (length cases) ([], [])
      forM_ (zip cases pages) \((_, _, _, (value, count)), (_, summary)) ->
        forM_ (lookup (isJust value, count) [((False, 1), "1 known observation"), ((True, 1), "1 observation"), ((True, 100), "100 observations")]) \label ->
          htmlOf summary `shouldContainAll` [">" <> label <> "</td>"]
      -- Explicit/custom buckets use uniform within-bucket interpolation; scalar observations
      -- remain exact. Missing baselines retain known counts without a full-population P75.
      map (\(detail, summary) -> (map (measured . (.measurement)) detail.vitalTrend, [(value.page, measured value.measurement) | value <- detail.pageVitals], [measured value.measurement | value <- summary.vitals, value.name == "lcp"])) pages
        `shouldBe` [([expected], [("/histogram", expected)], [expected]) | (_, _, _, expected) <- cases]
      let lcpMeasurements = [value.measurement | (_, summary) <- pages, value <- summary.vitals, value.name == "lcp"]
      [issue | RUMData.Unavailable _ issue <- lcpMeasurements] `shouldBe` [RUMData.MissingBaseline, RUMData.UnboundedBucket, RUMData.UnknownStart, RUMData.InterruptedSeries, RUMData.InterruptedSeries, RUMData.InterruptedSeries, RUMData.UnboundedBucket, RUMData.UnboundedBucket, RUMData.InvalidReset, RUMData.UnknownTemporality, RUMData.InvalidBuckets, RUMData.OverlappingIntervals, RUMData.UnknownStart, RUMData.InvalidReset]
      [estimate.bounds | RUMData.Measured _ estimate <- lcpMeasurements]
        `shouldBe` [Just (0, 100), Just (0, 100), Just (0, 200), Just (0, 200), Just (0, 100), Just (0, 100), Just (0, 100), Just (0, 100), Just (0, 100), Nothing, Just (0, 200), Just (0, 100), Just (0, 100)]

    it "vitalPopulation_gaugeAndDelta_useOneCurrentWindowRead" \tr -> do
      projectId <- createTestProject tr "RUM one-read population"
      apiKey <- createTestAPIKey tr projectId "rum-one-read-key"
      let at = (`addUTCTime` frozenTime)
      exportMetrics tr apiKey [] [lcpHistogram PM.AGGREGATION_TEMPORALITY_DELTA [histogramPoint "/one-read" (at (-10)) (at (-60)) 100 [80, 20, 0] [100, 200]]]
      forM_ (zip [10, 30] [-11, -10]) \(value, seconds) -> ingestMetric tr apiKey [] [mkAttr "page.url" "/one-read"] "browser.web_vital.fcp" value (at seconds)
      queries <- newIORef (0 :: Int)
      let database = interpose @(Labeled "timefusion" Hasql.Hasql) \_ (Labeled effect) -> do
            modifyIORef' queries (+ 1)
            case effect of
              Hasql.UseStatement params statement -> send $ Labeled @"timefusion" $ Hasql.UseStatement params statement
              Hasql.UseSession session -> send $ Labeled @"timefusion" $ Hasql.UseSession session
              Hasql.UseLabeledSession label attributes session -> send $ Labeled @"timefusion" $ Hasql.UseLabeledSession label attributes session
      -- Unrelated native traffic must not affect the handler-local count.
      when tr.trATCtx.env.enableTimefusionReads
        $ void
        $ runQueryEffect tr
        $ Hasql.withHasqlTimefusion True
        $ Hasql.interpOne @(HI.OneColumn Int64) [HI.sql|SELECT 1::bigint|]
      page <- rumBody tr $ database $ RUM.rumGetH projectId (Just "performance") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "vital_trend") (Just "1") Nothing
      page.degradedPanels `shouldBe` []
      let expected = [("fcp", (Just 25, 2)), ("lcp", (Just 93.75, 100))]
      sort [(vital.name, measured vital.measurement) | vital <- page.vitals, vital.name `elem` ["fcp", "lcp"]] `shouldBe` expected
      sort [(vital.metricName, measured vital.measurement) | vital <- page.vitalTrend] `shouldBe` expected
      sort [(vital.metricName, measured vital.measurement) | vital <- page.pageVitals] `shouldBe` expected
      when tr.trATCtx.env.enableTimefusionReads $ readIORef queries `shouldReturn` 1

    it "histogramVitals_missingCount_retainsUnavailableCoverage" \tr -> do
      projectId <- createTestProject tr "RUM missing histogram count"
      apiKey <- createTestAPIKey tr projectId "rum-missing-count-key"
      exportMetrics tr apiKey [] [lcpHistogram PM.AGGREGATION_TEMPORALITY_DELTA [histogramPoint "/missing-count" (addUTCTime (-10) frozenTime) (addUTCTime (-60) frozenTime) 100 [80, 20, 0] [100, 200]]]
      runTestBg frozenTime tr
        $ Hasql.withHasqlTimefusion True
        $ Hasql.interpExecute_
        $ [HI.sql|
        INSERT INTO otel_metrics(project_id,timestamp,ingested_at,metric_unit,dropped_attributes_count,message_size_bytes,id,series_id,start_timestamp,metric_name,metric_type,aggregation_temporality,
          hist_bucket_counts,hist_explicit_bounds,attributes,resource,flags|]
        <> nativeDateColumn tr
        <> [HI.sql|)
        SELECT project_id,timestamp-INTERVAL '1 minute',ingested_at,metric_unit,dropped_attributes_count,message_size_bytes,id,series_id,start_timestamp-INTERVAL '1 minute',metric_name,metric_type,aggregation_temporality,
          hist_bucket_counts,hist_explicit_bounds,attributes,resource,flags|]
        <> nativeDateColumn tr
        <> [HI.sql| FROM otel_metrics WHERE project_id=#{projectId.toText}
      |]
      lcpUnavailable tr projectId 100 RUMData.InvalidBuckets

    it "denseHistogramVitals_keepCompleteCounts_andBoundCachedPages" \tr -> do
      projectId <- createTestProject tr "RUM dense histogram"
      apiKey <- createTestAPIKey tr projectId "rum-dense-key"
      let at = (`addUTCTime` frozenTime)
          points =
            [ histogramPoint ("https://dense.example/" <> show series) (at end) (at (-90000)) (100 + fromIntegral ordinal) [80 + fromIntegral ordinal, 20, 0] [100, 200]
            | series <- [0 .. 199 :: Int]
            , (ordinal, end) <- [(0, -86401)] <> [(n, -4800 + 30 * fromIntegral n) | n <- [1 .. 160 :: Int]]
            ]
          scalar =
            defMessage & PMF.name
              .~ "browser.web_vital.fcp"
                & PMF.gauge
              .~ (defMessage & PMF.dataPoints .~ [defMessage & PMF.timeUnixNano .~ nanos (at (-10)) & PMF.attributes .~ [mkAttr "page.url" ("https://dense.example/" <> show series)] & PMF.asDouble .~ 25 | series <- [0 .. 199 :: Int]])
      exportMetrics tr apiKey [] [lcpHistogram PM.AGGREGATION_TEMPORALITY_CUMULATIVE points, scalar]
      forM_ ([Just "1", Nothing] :: [Maybe Text]) \refresh -> do
        when (isNothing refresh) $ Cache.purge tr.trATCtx.rumCache
        page <- loadVitalPanel tr projectId refresh "vital_trend"
        page.degradedPanels `shouldBe` []
        forM_ ([("lcp", 75, 32000), ("fcp", 25, 200)] :: [(Text, Double, Natural)]) \(name, value, count) -> do
          [measured vital.measurement | vital <- page.vitals, vital.name == name] `shouldBe` [(Just value, count)]
          let trend = filter ((== name) . (.metricName)) page.vitalTrend
          sum (map (RUMData.measurementSamples . (.measurement)) trend) `shouldBe` count
          map (RUMData.measurementValue . (.measurement)) trend `shouldSatisfy` all (== Just value)
        let selectedPages = take 150 (sort ["https://dense.example/" <> show series | series <- [0 .. 199 :: Int]])
        forM_ ([("lcp", 160), ("fcp", 1)] :: [(Text, Natural)]) \(name, count) -> do
          let metricPages = filter ((== name) . (.metricName)) page.pageVitals
          map (RUMData.measurementSamples . (.measurement)) metricPages `shouldBe` replicate 150 count
          map (.page) metricPages `shouldBe` selectedPages

    it "invalidScalarVitals_areUnavailableWithoutHistogramBucketErrors" \tr -> do
      forM_ [-1, 1 / 0, 0 / 0] \value -> do
        projectId <- createTestProject tr "RUM invalid scalar"
        apiKey <- createTestAPIKey tr projectId "rum-invalid-key"
        ingestMetric tr apiKey [] [mkAttr "page.url" "/invalid"] "browser.web_vital.lcp" value (addUTCTime (-10) frozenTime)
        lcpUnavailable tr projectId 0 RUMData.InvalidValue

    it "unsupportedVitals_doNotPresentSumOrOtherDistributionMeansAsObservations" \tr -> do
      let end = nanos $ addUTCTime (-10) frozenTime
          start = nanos $ addUTCTime (-60) frozenTime
          attributes = [mkAttr "page.url" "/unsupported"]
          number :: PM.NumberDataPoint
          number = defMessage & PMF.timeUnixNano .~ end & PMF.startTimeUnixNano .~ start & PMF.attributes .~ attributes & PMF.asDouble .~ 2200
          distribution :: PM.ExponentialHistogramDataPoint
          distribution = defMessage & PMF.timeUnixNano .~ end & PMF.startTimeUnixNano .~ start & PMF.attributes .~ attributes & PMF.count .~ 100 & PMF.sum .~ 220000
          summaryPoint :: PM.SummaryDataPoint
          summaryPoint = defMessage & PMF.timeUnixNano .~ end & PMF.attributes .~ attributes & PMF.count .~ 100 & PMF.sum .~ 220000
          base :: PM.Metric
          base = defMessage & PMF.name .~ "browser.web_vital.lcp"
          metrics =
            [ base & PMF.sum .~ (defMessage & PMF.aggregationTemporality .~ PM.AGGREGATION_TEMPORALITY_CUMULATIVE & PMF.dataPoints .~ [number])
            , base & PMF.exponentialHistogram .~ (defMessage & PMF.aggregationTemporality .~ PM.AGGREGATION_TEMPORALITY_DELTA & PMF.dataPoints .~ [distribution])
            , base & PMF.summary .~ (defMessage & PMF.dataPoints .~ [summaryPoint])
            ]
      forM_ metrics \metric -> do
        projectId <- createTestProject tr "RUM unsupported populations"
        apiKey <- createTestAPIKey tr projectId "rum-unsupported-key"
        exportMetrics tr apiKey [] [metric]
        lcpUnavailable tr projectId 0 RUMData.UnsupportedPopulation

    it "histogramVitals_exclusiveBucketLowerBounds_preserveKnownRatingBands" \tr -> do
      ratings <- forM [[2500, 4000], [4000, 5000], [2000, 4000]] \bounds -> do
        projectId <- createTestProject tr "RUM histogram bands"
        apiKey <- createTestAPIKey tr projectId "rum-band-key"
        exportMetrics tr apiKey [] [lcpHistogram PM.AGGREGATION_TEMPORALITY_DELTA [histogramPoint "/bands" (addUTCTime (-10) frozenTime) (addUTCTime (-60) frozenTime) 100 [0, 100, 0] bounds & PMF.sum .~ 300000]]
        page <- loadVitalPanel tr projectId Nothing "vitals"
        pure $ fst $ T.breakOn "</tr>" $ snd $ T.breakOn "Largest Contentful Paint" $ htmlOf page
      zipWithM_ shouldContainAll ratings [["Needs improvement"], ["Poor"], ["Not assessed"]]

    it "pageVitals_distinctExactUrls_doNotMergeRoutePercentiles" \tr -> do
      projectId <- createTestProject tr "RUM exact page populations"
      apiKey <- createTestAPIKey tr projectId "rum-page-key"
      let low = "https://shop.example/item/ABCDEF12?variant=low"
          high = "https://shop.example/item/DEADBEEF?variant=high"
      forM_ ([(low, 10, -10), (low, 20, -9), (high, 1000, -8), (high, 2000, -7)] :: [(Text, Double, Int)]) \(url, value, seconds) ->
        ingestMetric tr apiKey [] [mkAttr "page.url" url] "browser.web_vital.lcp" value (addUTCTime (fromIntegral seconds) frozenTime)
      page <- shellHtml tr $ RUM.rumGetH projectId (Just "performance") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "vital_trend") (Just "1") Nothing
      page `shouldContainAll` [low, high, "17.5 ms", "1.8 s", "attributes.url.full%20%3D%3D%20%22https"]
      page `shouldSatisfy` (not . T.isInfixOf "/item/{hex}")

    it "explicitTimeWindow_withoutSince_survivesSharedNavigation" \tr -> do
      let from = addUTCTime (-5400) frozenTime
          to = addUTCTime (-5) frozenTime
          fromQuery = Just $ toText $ iso8601Show from
          toQuery = Just $ toText $ iso8601Show to
      (_, RUM.RumGet (PageCtx _ shell)) <- testServant tr $ RUM.rumGetH testPid (Just "performance") Nothing Nothing fromQuery toQuery Nothing Nothing Nothing Nothing Nothing Nothing
      case shell of
        DeferredShell _ url _ -> url `shouldNotSatisfy` T.isInfixOf "since=24H"
        DeferredBody{} -> fail "Expected initial shared-window shell"
      forM_ [(fromQuery, toQuery, from, to, Nothing), (fromQuery, Nothing, from, frozenTime, Nothing), (Nothing, toQuery, addUTCTime (-300) frozenTime, to, Nothing), (Nothing, Nothing, addUTCTime (-86400) frozenTime, frozenTime, Just ("24H" :: Text))] \(fromM, toM, expectedFrom, expectedTo, expectedSince) -> do
        page <- loadPerformance tr testPid fromM toM Nothing Nothing Nothing
        let window :: TimePicker.TimeWindow
            window = page.links.window
        (window.fromTime, window.toTime, window.sinceQuery) `shouldBe` (expectedFrom, expectedTo, expectedSince)

    it "singletonVitalTrend_keepsTheSelectedTimeWindow" \tr -> do
      projectId <- createTestProject tr "RUM singleton time window"
      apiKey <- createTestAPIKey tr projectId "rum-singleton-window-key"
      ingestMetric tr apiKey [] [] "browser.web_vital.lcp" 1200 (addUTCTime (-10) frozenTime)
      let millis :: UTCTime -> Int
          millis = floor . (* 1000) . utcTimeToPOSIXSeconds
          explicitFrom = addUTCTime (-5400) frozenTime
          explicitTo = addUTCTime (-5) frozenTime
      forM_ [(Nothing, Nothing, Just ("1H" :: Text), addUTCTime (-3600) frozenTime, frozenTime), (Just $ toText $ iso8601Show explicitFrom, Just $ toText $ iso8601Show explicitTo, Just "", explicitFrom, explicitTo)] \(from, to, since, expectedFrom, expectedTo) -> do
        page <- loadPerformance tr projectId from to since Nothing (Just "vital_trend")
        length page.vitalTrend `shouldBe` 1
        htmlOf page `shouldContainAll` ["&quot;from&quot;:" <> show (millis expectedFrom), "&quot;to&quot;:" <> show (millis expectedTo)]

    it "browserTelemetry_correlatesExperienceVitalsErrorsAndReplaySessions" \tr -> do
      apiKey <- createTestAPIKey tr testPid "rum-browser-key"
      browserSpan apiKey "10000000000000000000000000000001" "1000000000000001" [("url.path", "/checkout"), ("user.id", "usr-42"), ("user.full_name", "Ada Lovelace")] "Pageview · /checkout" sessionId Nothing "storefront" tr
      browserSpan apiKey "10000000000000000000000000000001" "1000000000000002" [("url.path", "/checkout"), ("error.type", "TypeError"), ("error.message", "Cannot read cart")] "TypeError · /checkout" sessionId (Just "1000000000000001") "storefront" tr
      browserSpan apiKey "20000000000000000000000000000002" "2000000000000001" [("url.path", "/search"), ("user.id", "usr-9")] "Pageview · /search" "session-search" Nothing "storefront" tr
      ingestTrace tr apiKey "GET /backend-only" frozenTime
      ingestMetric tr apiKey [] [] "browser.web_vital.lcp" 2200 frozenTime
      ingestMetric tr apiKey [] [] "browser.web_vital.cls" 0.08 frozenTime

      withResource tr.trPool \conn -> do
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_id, user_name) VALUES (?, ?, ?, ?, 1, ?, ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at" (replayUuid, testPid, frozenTime, addUTCTime 60 frozenTime, "usr-42" :: Text, "Ada Lovelace" :: Text)
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?, ?, ?, ?, 0, ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at, event_file_count = 0, file_keys = '{}', shard_keys = '{}'" (emptyReplayUuid, testPid, frozenTime, addUTCTime 60 frozenTime, "No recording" :: Text)
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, shard_keys, user_name) VALUES (?, ?, ?, ?, 0, ARRAY['00000000-0000-0000-0000-000000000044/merged.json.gz'], ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at, event_file_count = 0, shard_keys = EXCLUDED.shard_keys" (mergedReplayUuid, testPid, frozenTime, addUTCTime 60 frozenTime, "Merged replay" :: Text)

      -- The summary widgets send raw SQL (so TimeFusion can serve them from rollup measures);
      -- run it through the chart pipeline exactly as the widgets do. Two page views and two
      -- sessions from the browser spans; the backend span is in neither.
      let widget dataType sql = runQueryEffect tr $ Charts.queryMetrics Nothing (Just dataType) (Just testPid) Nothing (Just sql) Nothing (Just $ stamp (-3600)) (Just $ stamp 3600) (Just "spans") Nothing []
          stamp s = toText $ iso8601Show $ addUTCTime s frozenTime
          series :: Text -> MetricsData -> Double
          series name m = sum $ maybe [] (\i -> mapMaybe (join . (V.!? i)) (V.toList m.dataset)) (V.elemIndex name m.headers)
      pageViews <- widget Charts.DTMetric $ RUM.binnedSql "count(*)" RUM.pageViewSql
      sessionCount <- widget Charts.DTFloat RUM.sessionsSql
      errors <- widget Charts.DTMetric $ RUM.binnedSql "count(*)" (RUM.browserSql <> " AND " <> RUM.errorSql)
      activity <- widget Charts.DTMetric RUM.activitySql
      p75 <- widget Charts.DTFloat RUM.p75Sql
      forM_ (zip ["pageViews", "sessions", "errors", "activity", "p75" :: Text] [pageViews, sessionCount, errors, activity, p75]) \(name, m) -> (name, m.error) `shouldBe` (name, Nothing)
      (series "value" pageViews, sessionCount.dataFloat) `shouldBe` (2, Just (2 :: Double))
      (series "Page views" activity, series "Errors" activity) `shouldBe` (2 :: Double, series "value" errors)
      isJust p75.dataFloat `shouldBe` True

      overviewData <- rumBody tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pulse") (Just "1") Nothing
      (\p -> (p.sessions, isJust p.p75LoadMs)) <$> overviewData.pulse `shouldBe` Just (2, True)
      overviewData.degradedPanels `shouldBe` []
      overview <- renderPage tr Nothing Nothing Nothing Nothing
      -- The unscoped read caches under an unscoped key; `service` is part of that key so a
      -- scoped page can never be served these rows.
      isJust <$> Cache.lookup tr.trATCtx.rumCache (RUMData.RumCacheKey testPid (RUMData.VitalPopulationQuery RUMData.OneHour) Nothing Nothing Nothing Nothing (Just "24H")) `shouldReturn` True
      -- The numbers and activity chart are dashboard Widget components that fetch their own
      -- data through the chart pipeline; the page ships their queries. The session and P75
      -- tiles also arrive with their value from the pulse row above.
      overview `shouldContainAll` ["Page views", "Browser errors", "{{query_ast_filters}}", "rum-activity", "Largest Contentful Paint", "2.2 s", "/checkout", "Ada Lovelace"]
      -- The LIVE badge is only honest if something listens for the time transport's tick, and
      -- the panels hold every number on this page. Each re-fetches itself in place.
      overview `shouldContainAll` ["hx-trigger=\"update-query[", "from:window\"", "hx-sync=\"#rum-panel-pages:replace\""]
      -- The tab strip and time picker must not wait on six 24-hour scans: the request that
      -- paints the page answers with a skeleton that fetches the panels itself. Panel data
      -- appearing here again would mean a tab click is back to seconds of blank page.
      shell <- shellHtml tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing Nothing Nothing
      shell `shouldContainAll` ["Real User Monitoring", "tabs tabs-box tabs-outline", "id=\"rum-page\"", "hx-trigger=\"load\"", "deferred=1"]
      T.isInfixOf "Ada Lovelace" shell `shouldBe` False

      sessions <- renderPage tr (Just "sessions") Nothing (Just "errors") (Just sessionId)
      sessions `shouldContainAll` ["With errors", "Ada Lovelace", "Watch replay", "initialSession=\"00000000-0000-0000-0000-000000000042\"", "hx-target=\"#rum-replay-workspace\"", "hx-sync=\"#rum-replay-workspace:replace\""]
      T.isInfixOf "/search" sessions `shouldBe` False
      -- A row click must fetch the sessions panel: a deferred request without a panel renders
      -- only shells, hx-select finds no workspace in it, and the outerHTML swap deletes the
      -- workspace from the page.
      sessions `shouldContainAll` ["panel=sessions&amp;deferred=1"]
      -- And the shells a deep link renders must carry its filter and selection, or the panel
      -- they fetch comes back unfiltered with nothing selected.
      deepShell <- shellHtml tr $ RUM.rumGetH testPid (Just "sessions") Nothing (Just "errors") Nothing Nothing (Just "24H") (Just sessionId) Nothing Nothing (Just "1") Nothing
      deepShell `shouldContainAll` ["filter=errors", "session=00000000-0000-0000-0000-000000000042"]

      replaySessions <- renderPage tr (Just "sessions") Nothing (Just "replays") Nothing
      T.isInfixOf "No recording" replaySessions `shouldBe` False
      T.isInfixOf "Merged replay" replaySessions `shouldBe` True

    it "deferredRUMPanel_omitsPageChrome_preservesPanelMarkup" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-panel-body-key"
      otelBrowserSpan apiKey "88000000000000000000000000000008" "8800000000000001" "session-panel-body" "panel-body-browser" tr
      let scoped = withService (Just "panel-body-browser") tr
      (_, page@(RUM.RumGet (PageCtx _ body))) <- testServant scoped $ RUM.rumGetH testPid (Just "performance") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pages") (Just "1") Nothing
      let rendered = htmlOf page
      (rendered == htmlOf body) `shouldBe` True
      rendered `shouldContainAll` ["id=\"rum-page\"", "id=\"rum-panel-pages\"", "Top pages", "/cart"]
      T.isInfixOf "<html" rendered `shouldBe` False
      fullPage <- shellHtml scoped $ RUM.rumGetH testPid (Just "performance") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing Nothing (Just "1") Nothing
      T.isInfixOf "<html" fullPage `shouldBe` True

    it "panelQueryFailure_survivesHTMXSelection_insteadOfClaimingNoData" \tr -> do
      purgeRumCaches tr
      projectId <- createTestProject tr "RUM failed panel"
      let scoped = tr{trATCtx = tr.trATCtx{env = tr.trATCtx.env{enableTimefusionReads = True}}}
      forM_ ([("vitals", "rum-panel-vitals", [RUMData.VitalPopulationQuery RUMData.OneHour]), ("vital_trend", "rum-panel-vital_trend", [RUMData.VitalPopulationQuery RUMData.OneHour]), ("sessions", "rum-sessions-list", [RUMData.SessionSearchQuery Nothing RUMData.AllSessionRows, RUMData.SessionDetailQuery "selected-session"]), ("session_detail", "rum-replay-workspace", [RUMData.SessionDetailQuery "selected-session"])] :: [(Text, Text, [RUMData.RumQuery])]) \(panel, target, queries) -> do
        (_, payload@(RUM.RumGet (PageCtx _ body))) <- testServant scoped $ tfUnavailable $ RUM.rumGetH projectId (Just $ if panel `elem` ["sessions", "session_detail"] then "sessions" else "performance") Nothing Nothing Nothing Nothing (Just "24H") (Just "selected-session") Nothing (Just panel) (Just "1") Nothing
        page <- deferredBody body
        page.degradedPanels `shouldBe` queries
        let selected = snd $ T.breakOn ("id=\"" <> target <> "\"") $ htmlOf payload
        T.isPrefixOf "<div role=\"alert\"" (T.drop 1 $ snd $ T.breakOn ">" selected) `shouldBe` True
        selected `shouldContainAll` ["Some RUM data could not be loaded.", ">Retry</button>", "refresh=1"]
        any (`T.isInfixOf` selected) ["No data", "No web vital samples", "Choose a session", "No sessions match"] `shouldBe` False
      let sid = "00000000-0000-0000-0000-000000000091" :: Text
          -- refresh: a cold wide list otherwise answers with the uncached newest-3h slice.
          sessions resources panel selected = testServant resources $ RUM.rumGetH projectId (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") selected Nothing (Just panel) (Just "1") (Just "1")
          firstContent target = T.drop 1 . snd . T.breakOn ">" . snd . T.breakOn ("id=\"" <> target <> "\"") . htmlOf
      withResource tr.trPool $ \conn ->
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?::uuid, ?, ?, ?, 1, 'Failure survivor')" (sid, projectId, frozenTime, addUTCTime 60 frozenTime)
      -- A healthy cached counterpart survives the other query's acquisition failure.
      forM_ ([("session_detail", Just sid, "rum-sessions-list", "rum-replay-workspace", [RUMData.SessionSearchQuery Nothing RUMData.AllSessionRows]), ("sessions", Nothing, "rum-replay-workspace", "rum-sessions-list", [RUMData.SessionDetailQuery sid])] :: [(Text, Maybe Text, Text, Text, [RUMData.RumQuery])]) $ \(warmPanel, warmSelection, failedTarget, healthyTarget, queries) -> do
        purgeRumCaches tr
        void $ sessions tr warmPanel warmSelection
        (_, response@(RUM.RumGet (PageCtx _ body))) <- testServant scoped $ tfUnavailable $ RUM.rumGetH projectId (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") (Just sid) Nothing (Just "sessions") (Just "1") Nothing
        page <- deferredBody body
        page.degradedPanels `shouldBe` queries
        T.isPrefixOf "<div role=\"alert\"" (firstContent failedTarget response) `shouldBe` True
        T.isPrefixOf "<div role=\"alert\"" (firstContent healthyTarget response) `shouldBe` False
        htmlOf response `shouldContainAll` ["Failure survivor"]
      purgeRumCaches tr
      let healthy = testServant tr $ RUM.rumGetH projectId (Just "performance") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "vitals") (Just "1") Nothing
      (_, emptyPage) <- healthy
      htmlOf emptyPage `shouldContainAll` ["No Web Vitals in this time range"]
      any (\params -> lookup "tab" params == Just "performance" && lookup "since" params == Just "7D" && all (isNothing . (`lookup` params)) ["from", "to"]) (linkParams ("/p/" <> projectId.toText <> "/rum") $ htmlOf emptyPage) `shouldBe` True
      T.isInfixOf "role=\"alert\"" (htmlOf emptyPage) `shouldBe` False
      apiKey <- createTestAPIKey tr projectId "rum-panel-recovery-key"
      ingestMetric tr apiKey [] [] "browser.web_vital.fcp" 10 frozenTime
      purgeRumCaches tr
      (_, populated) <- healthy
      htmlOf populated `shouldContainAll` ["10.0 ms"]
      T.isInfixOf "role=\"alert\"" (htmlOf populated) `shouldBe` False

    it "panelLinks_carryKqlTheLogExplorerCanParse" \tr -> do
      -- windowUrl URI-encodes every parameter it is given, so a caller that encodes first
      -- ships a double-escaped query: a space arrives as %2520, the Explorer sees one
      -- meaningless token, and the link silently returns nothing.
      overview <- renderPage tr Nothing Nothing Nothing Nothing
      -- Pinned as the property rather than one exact query string: a space encoded once as
      -- %20, a quote once as %22, and no %25 anywhere on the page — %25 is the signature of a
      -- second pass, since it is what a literal % becomes.
      overview `shouldContainAll` ["/log_explorer?since=24H&amp;query=", "%20and%20", "%22documentLoad%22"]
      T.isInfixOf "%25" overview `shouldBe` False

    it "serviceFilter_scopesEveryPanelToOneBrowserService" \tr -> do
      -- Several teams report into one project. Averaging their services together hides a
      -- checkout regression behind a healthy marketing site, so the page has to be scopeable.
      -- Panels are cached for 15s across requests, and earlier examples already populated the
      -- unscoped key. Without this the assertions below read a snapshot taken before the span
      -- this example ingests.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-service-key"
      -- Reuses the session that already has a recording, so the scoped page can be checked
      -- for the replay badge as well as the rows.
      browserSpan apiKey "30000000000000000000000000000003" "3000000000000001" [("url.path", "/admin/users")] "Pageview · /admin/users" (UUID.toText mergedReplayUuid) Nothing "admin-console" tr

      unscoped <- renderPage tr Nothing Nothing Nothing Nothing
      unscoped `shouldContainAll` ["/admin/users", "/checkout"]

      scoped <- renderScoped tr Nothing Nothing Nothing Nothing (Just "admin-console")
      scoped `shouldContainAll` ["admin-console", "/admin/users"]
      -- The other team's pages are the whole point: if they survive the filter it does nothing.
      T.isInfixOf "/checkout" scoped `shouldBe` False
      -- Explorer receives the same global scope predicate as the SQL-backed panels.
      -- The compact KQL spelling is intentional: it is parsed and URL-encoded only once.
      scoped `shouldContainAll` ["service%3D%3D%22admin-console%22"]
      T.isInfixOf "&amp;service=admin-console" scoped `shouldBe` False
      T.isInfixOf "?service=admin-console" scoped `shouldBe` False
      -- A recording carries no service, so it can only be trusted where a span already places
      -- the session in this service — attached to that row, never added as a row of its own.
      scoped `shouldContainAll` ["Replay"]
      T.isInfixOf "No recording" scoped `shouldBe` False

      -- A stale global selection still resolves, and an empty result must read as "this filter
      -- matched nothing", never as "you never installed the SDK".
      ghost <- renderScoped tr Nothing Nothing Nothing Nothing (Just "ghost-service")
      ghost `shouldContainAll` ["No browser telemetry for ghost-service in this range", "global scope picker"]
      T.isInfixOf "Install the browser SDK" ghost `shouldBe` False

    it "sharedRumLinks_keepTheirEnvironmentInsteadOfUsingTheRecipientDefault" \tr -> do
      -- An alert recipient may have a different sticky environment selected. A RUM link is
      -- investigation evidence, so its explicit environment has to win and remain present on
      -- the deferred request and every self-link it renders.
      purgeRumCaches tr
      scopedHtml <- shellHtml tr $ RUM.rumGetScopedH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing (Just "admin-console") Nothing (Just "1") Nothing (Just "missing-environment")
      scopedHtml `shouldContainAll` ["environment=missing-environment"]
      T.isInfixOf "&amp;service=admin-console" scopedHtml `shouldBe` False
      T.isInfixOf "?service=admin-console" scopedHtml `shouldBe` False

      pulse <- rumBody tr $ RUM.rumGetScopedH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing (Just "admin-console") (Just "pulse") (Just "1") Nothing (Just "missing-environment")
      -- The project has browser telemetry from the preceding examples. It must not leak into
      -- a link scoped to another environment merely because this test session's default is
      -- unscoped.
      pulse.pulse `shouldBe` Nothing

      loaded <- rumBody tr $ RUM.rumGetScopedH testPid (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") (Just sessionId) Nothing (Just "sessions") (Just "1") Nothing (Just "missing-environment")
      loaded.sessions `shouldBe` []
      loaded.selectedSessionData `shouldBe` Nothing

    it "environmentSessionCache_ignoresLegacyUnattributedRecordings" \tr -> do
      purgeRumCaches tr
      let sid = "00000000-0000-0000-0000-000000000046"
          environment = Just "cache-environment"
          load env refreshM = rumBody tr $ RUM.rumGetScopedH testPid (Just "sessions") (Just "Legacy replay") Nothing Nothing Nothing (Just "24H") (Just sid) Nothing (Just "sessions") (Just "1") refreshM env
          legacyKey query = toXXHash $ show $ RUMData.RumCacheKey testPid query environment Nothing Nothing Nothing (Just "24H")
      withResource tr.trPool $ \conn ->
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?::uuid, ?, ?, ?, 1, 'Legacy replay')" (sid, testPid, frozenTime, addUTCTime 60 frozenTime)
      legacy <- load Nothing Nothing
      map (.id) legacy.sessions `shouldBe` [sid]
      runQueryEffect tr do
        RUMData.rumPanelCacheSet (legacyKey $ RUMData.SessionSearchQuery (Just "Legacy replay") RUMData.AllSessionRows) 300 (RUMData.SessionsResult legacy.sessions)
        RUMData.rumPanelCacheSet (legacyKey $ RUMData.SessionDetailQuery sid) 300 (RUMData.SessionDetailResult legacy.selectedSessionData)
      Cache.purge tr.trATCtx.rumCache
      scoped <- load environment Nothing
      scoped.sessions `shouldBe` []
      scoped.selectedSessionData `shouldBe` Nothing
      apiKey <- createTestAPIKey tr testPid "rum-environment-cache-key"
      ingestSpanReq tr $ mkSpanRequest "83000000000000000000000000000008" "8300000000000001" Nothing "documentLoad" [] Nothing [mkAttr "session.id" sid] (mkResource apiKey [mkAttr "service.name" "legacy-browser", mkAttr "telemetry.sdk.language" "webjs", mkAttr "deployment.environment.name" "cache-environment"]) frozenTime
      attributed <- load environment (Just "1")
      map (.events) attributed.sessions `shouldBe` [1]
      map (.hasReplay) attributed.sessions `shouldBe` [True]
      (.hasReplay) <$> attributed.selectedSessionData `shouldBe` Just True

    it "browserSdkWithoutSdkLanguage_isStillSeenAndGloballyScopeable" \tr -> do
      -- The OpenTelemetry browser SDKs leave telemetry.sdk.language unset, and RUM used to
      -- filter on that alone. Every browser application in production was therefore invisible:
      -- no page views, no sessions, and nothing for the global scope to narrow to.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-otel-browser-key"
      otelBrowserSpan apiKey "40000000000000000000000000000004" "4000000000000001" "session-otel" "checkout-web" tr

      page <- renderPage tr Nothing Nothing Nothing Nothing
      page `shouldContainAll` ["https://shop.example/cart"]

      -- And the global scope can narrow to it.
      scoped <- renderScoped tr Nothing Nothing Nothing Nothing (Just "checkout-web")
      scoped `shouldContainAll` ["checkout-web", "https://shop.example/cart"]
      T.isInfixOf "/admin/users" scoped `shouldBe` False

    it "sdkPageview_fullUrlWithQueryStillGroupsByPath" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-pageview-query-key"
      browserSpan apiKey "41000000000000000000000000000004" "4100000000000001" [("url.path", "/cart"), ("url.full", "/cart?coupon=x")] "Pageview · /cart" "session-page-query" Nothing "pageview-query-test" tr
      loaded <- rumBody (withService (Just "pageview-query-test") tr) $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pages") (Just "1") Nothing
      map (.path) loaded.pages `shouldBe` ["/cart"]

    it "sessionLastPage_isTheLatestPageView_notALexicographicResourceUrl" \tr -> do
      -- MAX(path) over every browser span used to pick the alphabetically largest URL: on
      -- real traffic that is a third-party font fetched by the page, shown as the page.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-lastpage-key"
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000001" [("url.path", "/alpha")] "Pageview · /alpha" "session-lastpage" Nothing "storefront" (addUTCTime (-120) frozenTime) tr
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000002" [("url.full", "https://zzz-fonts.example/css2")] "HTTP GET" "session-lastpage" Nothing "storefront" (addUTCTime (-60) frozenTime) tr
      browserSpanAt apiKey "50000000000000000000000000000005" "5000000000000003" [("url.path", "/beta")] "Pageview · /beta" "session-lastpage" Nothing "storefront" (addUTCTime (-30) frozenTime) tr
      row <- renderPanel tr (Just "sessions") (Just "session-lastpage") Nothing Nothing Nothing (Just "sessions")
      row `shouldContainAll` ["/beta"]
      T.isInfixOf "zzz-fonts.example" row `shouldBe` False
      T.isInfixOf "/alpha" row `shouldBe` False

    it "selectedSession_withoutRecording_keepsIdentityAndTelemetryInWorkspace" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-telemetry-workspace-key"
      let sid = "session-without-recording"
      browserSpanAt apiKey "84000000000000000000000000000008" "8400000000000001" [("url.path", "/profile"), ("user.full_name", "Telemetry visitor")] "documentLoad" sid Nothing "workspace-browser" (addUTCTime (-65) frozenTime) tr
      browserSpan apiKey "84000000000000000000000000000008" "8400000000000002" [("exception.type", "TypeError"), ("exception.message", "Form failed")] "TypeError" sid (Just "8400000000000001") "workspace-browser" tr
      html <- renderPanel tr (Just "sessions") (Just sid) Nothing (Just sid) Nothing (Just "sessions")
      let workspace = fst $ T.breakOn "</section>" $ snd $ T.breakOn "<section id=\"rum-replay-workspace\"" html
          headings = fst $ T.breakOn "</thead>" $ snd $ T.breakOn "<thead" html
      workspace `shouldContainAll` ["Telemetry visitor", sid, "Duration", "1m 5s", "Page views", "Events", "Errors", "/profile", "workspace-browser", "No recording for this session", "Inspect telemetry"]
      T.isInfixOf ">2</dd>" workspace `shouldBe` True
      headings `shouldContainAll` ["Last page"]
      T.isInfixOf "Landing page" headings `shouldBe` False

    it "selectedSession_detailPanel_doesNotRunTheFullSessionSearch" \tr -> do
      purgeRumCaches tr
      projectId <- createTestProject tr "RUM independent session detail"
      apiKey <- createTestAPIKey tr projectId "rum-independent-detail-key"
      let sid = "session-independent-detail"
          query = "unrelated-full-list-search"
          searchKey = RUMData.RumCacheKey projectId (RUMData.SessionSearchQuery (Just query) RUMData.ErrorSessionRows) Nothing (Just "detail-browser") Nothing Nothing (Just "24H")
          scoped = withService (Just "detail-browser") tr
          load refresh = shellHtml scoped $ RUM.rumGetH projectId (Just "sessions") (Just query) (Just "errors") Nothing Nothing (Just "24H") (Just sid) Nothing (Just "session_detail") (Just "1") refresh
      browserSpan apiKey "85000000000000000000000000000008" "8500000000000001" [("url.path", "/independent-detail"), ("user.full_name", "Independent visitor")] "documentLoad" sid Nothing "detail-browser" tr
      html <- load Nothing
      html `shouldContainAll` ["rum-replay-workspace", "Independent visitor", sid, "/independent-detail", "No recording for this session", "Inspect telemetry"]
      T.isInfixOf "rum-sessions-list" html `shouldBe` False
      (isNothing <$> Cache.lookup tr.trATCtx.rumCache searchKey) `shouldReturn` True
      [PG.Only publications] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT count(*)::bigint FROM rum_panel_cache"
      publications `shouldBe` (1 :: Int64)
      Cache.purge tr.trATCtx.rumCache
      withResource tr.trPool $ \conn -> void $ PG.execute_ conn "UPDATE rum_panel_cache SET expires_at=now()-interval '1 minute'"
      browserSpan apiKey "85000000000000000000000000000008" "8500000000000002" [("exception.type", "TypeError")] "TypeError" sid Nothing "detail-browser" tr
      stale <- load Nothing
      stale `shouldContainAll` ["Independent visitor", "panel=session_detail", "refresh=1", "hx-target=\"#rum-replay-workspace\"", "hx-select=\"#rum-replay-workspace\"", "hx-sync=\"#rum-replay-workspace:abort\""]
      fresh <- load $ Just "1"
      T.isInfixOf ">2</dd>" fresh `shouldBe` True
      T.isInfixOf "refresh=1" fresh `shouldBe` False

    it "replayOnlySessions_sayWhatTheyAre_insteadOfUnknownPageAndZeroCounts" \tr -> do
      -- A recording whose session id never appears on a span is a real session; stacking
      -- "Unknown page" over "0 views · 0 events" reads as broken data, not as what it is.
      purgeRumCaches tr
      let replayOnlyUuid = [uuid|00000000-0000-0000-0000-000000000045|]
      withResource tr.trPool \conn ->
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?, ?, ?, ?, 1, ?) ON CONFLICT (session_id) DO UPDATE SET created_at = EXCLUDED.created_at, last_event_at = EXCLUDED.last_event_at" (replayOnlyUuid, testPid, frozenTime, addUTCTime 45 frozenTime, "Replay only user" :: Text)
      rows <- renderPanel tr (Just "sessions") (Just "Replay only user") Nothing Nothing Nothing (Just "sessions")
      rows `shouldContainAll` ["Replay only user", "Recording only", "No telemetry"]
      T.isInfixOf "Unknown page" rows `shouldBe` False
      T.isInfixOf "0 views" rows `shouldBe` False

    it "browserErrors_groupBySignature_withOccurrenceAndSessionCounts" \tr -> do
      -- Twenty copies of the loudest error used to fill the whole panel; issues with masked
      -- identifiers keep every distinct failure visible with its blast radius.
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-errors-key"
      browserSpan apiKey "60000000000000000000000000000006" "6000000000000001" [("exception.type", "TypeError"), ("exception.message", "Cannot read cart item 123")] "TypeError" "session-err-a" Nothing "storefront" tr
      browserSpan apiKey "60000000000000000000000000000006" "6000000000000002" [("exception.type", "TypeError"), ("exception.message", "Cannot read cart item 456")] "TypeError" "session-err-b" Nothing "storefront" tr
      panel <- renderPanel tr Nothing Nothing Nothing Nothing Nothing (Just "errors")
      panel `shouldContainAll` ["TypeError", "×2", "2 sessions", "View session", "Telemetry"]
      panel `shouldSatisfy` T.isInfixOf "tab=sessions"
      -- Grouped, not listed: the message renders once for the pair.
      T.count "Cannot read cart item" panel `shouldBe` 1

    it "panelCache_writeFailure_preservesFreshTelemetry" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-cache-write-key"
      otelBrowserSpan apiKey "81000000000000000000000000000008" "8100000000000001" "session-cache-write" "cache-write-browser" tr
      E.bracket_
        (execSql tr "ALTER TABLE rum_panel_cache ADD CONSTRAINT reject_panel_cache_write CHECK (false)")
        (execSql tr "ALTER TABLE rum_panel_cache DROP CONSTRAINT reject_panel_cache_write")
        do
          loaded <- rumBody tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "pages") (Just "1") Nothing
          loaded.degradedPanels `shouldBe` []
          map (.path) loaded.pages `shouldSatisfy` elem "https://shop.example/cart"

    it "panelCache_concurrentColdRequests_shareOneLookupAndPublication" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-flight-key"
      otelBrowserSpan apiKey "82000000000000000000000000000008" "8200000000000001" "session-flight" "flight-browser" tr
      let fetch = renderPanel tr Nothing Nothing Nothing Nothing (Just "flight-browser") (Just "pages")
          waitForReaders expected = do
            [PG.Only readers] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT count(*)::bigint FROM pg_stat_activity WHERE datname = current_database() AND wait_event_type = 'Lock' AND query LIKE '%rum_panel_cache%'"
            unless (readers >= (expected :: Int64)) $ threadDelay 10000 >> waitForReaders expected
      withCacheWriteCounter tr "rum_flight" \writes ->
        withResource tr.trPool \conn ->
          E.bracket_
            (void $ PG.execute_ conn "BEGIN; LOCK TABLE rum_panel_cache IN ACCESS EXCLUSIVE MODE")
            (void $ PG.execute_ conn "ROLLBACK")
            $ withAsync fetch \leader -> withAsync fetch \follower -> do
              observed <-
                E.finally
                  ((,) <$> timeout 5000000 (waitForReaders 1) <*> timeout 1000000 (waitForReaders 2))
                  (void $ PG.execute_ conn "ROLLBACK")
              responses <- traverse wait [leader, follower]
              observed `shouldBe` (Just (), Nothing)
              map (T.isInfixOf "/cart") responses `shouldBe` [True, True]
              writes `shouldReturn` 1

    it "emptySessionSearch_isBrieflyShared_andRefreshOrExpiryShowsNewTelemetry" \tr -> do
      purgeRumCaches tr
      let load query refreshM = do
            page <- rumBody tr $ RUM.rumGetH testPid (Just "sessions") (Just query) Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "sessions") (Just "1") refreshM
            map (.id) page.sessions <$ (page.degradedPanels `shouldBe` [])
          refreshSearch = "session-negative-refresh"
          expirySearch = "session-negative-expiry"
      withCacheWriteCounter tr "rum_search" \writes -> do
          load refreshSearch Nothing `shouldReturn` []
          -- A fresh replica must reuse the empty result without scanning telemetry.
          observed <- withResource tr.trPool \conn ->
            E.bracket_
              (void $ PG.execute_ conn "BEGIN; LOCK TABLE otel_logs_and_spans IN ACCESS EXCLUSIVE MODE")
              (void $ PG.execute_ conn "ROLLBACK")
              $ withAsync (do memory <- load refreshSearch Nothing; Cache.purge tr.trATCtx.rumCache; shared <- load refreshSearch Nothing; pure (memory, shared)) \request -> do
                result <- E.finally (timeout 2000000 $ wait request) (void $ PG.execute_ conn "ROLLBACK")
                void $ wait request
                pure result
          observed `shouldBe` Just ([], [])
          writes `shouldReturn` 1
          -- The one entry sits under the versioned key, never under an older replica's spelling.
          let searchKey = RUMData.rumCacheDbKey $ RUMData.RumCacheKey testPid (RUMData.SessionSearchQuery (Just refreshSearch) RUMData.AllSessionRows) Nothing Nothing Nothing Nothing (Just "24H")
          [PG.Only versionedEntries] <- withResource tr.trPool $ \conn -> PG.query conn "SELECT count(*)::bigint FROM rum_panel_cache WHERE cache_key = ?" (PG.Only searchKey)
          versionedEntries `shouldBe` (1 :: Int64)
          [PG.Only shortExpiry] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT expires_at > now() AND expires_at <= now() + interval '30 seconds' FROM rum_panel_cache"
          shortExpiry `shouldBe` True
          purgeRumCaches tr
          load refreshSearch Nothing `shouldReturn` []
          apiKey <- createTestAPIKey tr testPid "rum-negative-search-key"
          otelBrowserSpan apiKey "85000000000000000000000000000008" "8500000000000001" refreshSearch "negative-search-browser" tr
          load refreshSearch Nothing `shouldReturn` []
          load refreshSearch (Just "1") `shouldReturn` [refreshSearch]
          [PG.Only positiveExpiry] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT expires_at > now() + interval '4 minutes' FROM rum_panel_cache"
          positiveExpiry `shouldBe` True
          load expirySearch Nothing `shouldReturn` []
          otelBrowserSpan apiKey "86000000000000000000000000000008" "8600000000000001" expirySearch "negative-search-browser" tr
          Cache.purge tr.trATCtx.rumCache
          execSql tr "UPDATE rum_panel_cache SET expires_at = now() - interval '1 minute'"
          load expirySearch Nothing `shouldReturn` [expirySearch]

    it "emptySessionList_withoutSearch_doesNotHideNewSdkTelemetry" \tr -> do
      purgeRumCaches tr
      let service = "negative-cache-onboarding"
          fetch = renderPanel tr (Just "sessions") Nothing Nothing Nothing (Just service) (Just "sessions")
      forM_ ([Nothing, Just " \t "] :: [Maybe Text]) $ \query -> void $ renderPanel tr (Just "sessions") query Nothing Nothing (Just service) (Just "sessions")
      [PG.Only entries] <- withResource tr.trPool $ \conn -> PG.query_ conn "SELECT count(*)::bigint FROM rum_panel_cache"
      entries `shouldBe` (0 :: Int64)
      apiKey <- createTestAPIKey tr testPid "rum-negative-onboarding-key"
      otelBrowserSpan apiKey "87000000000000000000000000000008" "8700000000000001" "session-first-sdk-arrival" service tr
      fetch >>= (`shouldContainAll` ["session-first-sdk-arrival", "/cart"])

    it "vitalPanelCache_blankWindowFields_reuseThePrewarmedPopulation" \tr -> do
      projectId <- createTestProject tr "RUM canonical cache window"
      apiKey <- createTestAPIKey tr projectId "rum-canonical-window-key"
      let ingest value seconds = ingestMetric tr apiKey [] [mkAttr "page.url" "/canonical-window"] "browser.web_vital.lcp" value (addUTCTime seconds frozenTime)
          load from to since panel = do
            page <- loadPerformance tr projectId from to since Nothing (Just panel)
            [measured vital.measurement | vital <- page.vitals, vital.name == "lcp"] <$ (page.degradedPanels `shouldBe` [])
      ingest 1000 (-10)
      withCacheWriteCounter tr "rum_window" \publications -> do
          warm <- load Nothing Nothing (Just "24H") "vital_trend"
          ingest 9000 (-9)
          hits <- forM [(Just "", Just "", Just "24H"), (Nothing, Nothing, Nothing)] \(from, to, since) -> do
            Cache.purge tr.trATCtx.rumCache
            load from to since "vitals"
          warm : hits `shouldBe` replicate 3 [(Just 1000, 1)]
          let fromQuery = Just $ toText $ iso8601Show $ addUTCTime (-60) frozenTime
              toQuery = Just $ toText $ iso8601Show frozenTime
          explicit <- load fromQuery toQuery Nothing "vital_trend"
          ingest 10000 (-8)
          Cache.purge tr.trATCtx.rumCache
          blankSince <- load fromQuery toQuery (Just "") "vitals"
          publications `shouldReturn` 2
          [explicit, blankSince] `shouldBe` replicate 2 [(Just 7000, 2)]
          Cache.purge tr.trATCtx.rumCache
          load (Just "") (Just "") (Just "") "vitals" `shouldReturn` [(Just 9500, 3)]
          Cache.purge tr.trATCtx.rumCache
          load Nothing Nothing Nothing "vitals" `shouldReturn` [(Just 1000, 1)]
          publications `shouldReturn` 3

    it "panelCache_isSharedAcrossReplicas_notPerProcessMemory" \tr -> do
      -- A fresh replica has an empty memory cache; the shared rum_panel_cache table must
      -- still answer, proven by deleting the underlying rows so a recompute could not.
      -- The span is documentLoad-shaped: TimeFusion's text-index prefilter currently loses
      -- LIKE-matched ("Pageview \183 ") names inside narrow windows, which is a store bug
      -- this example must not depend on.
      apiKey <- createTestAPIKey tr testPid "rum-l2-key"
      ingestSpanReq tr $ mkSpanRequest "80000000000000000000000000000008" "8000000000000001" Nothing "documentLoad" [] Nothing [mkAttr "session.id" "session-l2", mkAttr "url.full" "https://l2.example/cached"] (mkResource apiKey [mkAttr "service.name" "storefront", mkAttr "user_agent.original" "Mozilla/5.0 L2"]) frozenTime
      -- A window no earlier example used, so the shared table has no entry yet for this key.
      let renderPages since refreshM = shellHtml tr $ RUM.rumGetH testPid Nothing Nothing Nothing Nothing Nothing (Just since) Nothing Nothing (Just "pages") (Just "1") refreshM
      firstRender <- renderPages "6H" Nothing
      -- Top pages renders path-only routes ('pageRoute'), so the host is stripped.
      firstRender `shouldContainAll` ["/cached"]
      -- Memory only — the shared table entry is exactly what a fresh replica would find.
      Cache.purge tr.trATCtx.rumCache
      withResource tr.trPool \conn ->
        void $ PG.execute conn "DELETE FROM otel_logs_and_spans WHERE project_id = ? AND attributes___session___id = ?" (testPid, "session-l2" :: Text)
      secondRender <- renderPages "6H" Nothing
      secondRender `shouldContainAll` ["/cached"]
      -- A fresh entry answers and asks for nothing more; only a stale one may schedule a refetch.
      T.isInfixOf "refresh=1" secondRender `shouldBe` False

      -- Past expiry but inside the prune horizon: the panel must still paint its last-known
      -- data rather than a cold scan, and carry the hidden refresh trigger that revalidates
      -- it. Without the trigger the page would show aged data forever.
      Cache.purge tr.trATCtx.rumCache
      withResource tr.trPool \conn ->
        void $ PG.execute_ conn "UPDATE rum_panel_cache SET expires_at = now() - interval '1 minute'"
      staleRender <- renderPages "6H" Nothing
      staleRender `shouldContainAll` ["/cached", "refresh=1"]
      -- And the revalidation terminates: refresh bypasses the stale band, so its response
      -- carries no trigger of its own.
      Cache.purge tr.trATCtx.rumCache
      refreshRender <- renderPages "6H" (Just "1")
      T.isInfixOf "refresh=1" refreshRender `shouldBe` False

    it "wideColdSessionList_paintsTheNewestThreeHours_thenRevalidatesTheFullWindow" \tr -> do
      purgeRumCaches tr
      projectId <- createTestProject tr "RUM wide cold list"
      apiKey <- createTestAPIKey tr projectId "rum-wide-cold-key"
      let load since refreshM = do
            (_, response@(RUM.RumGet (PageCtx _ body))) <- testServant tr $ RUM.rumGetH projectId (Just "sessions") Nothing Nothing Nothing Nothing (Just since) Nothing Nothing (Just "sessions") (Just "1") refreshM
            (,htmlOf response) <$> deferredBody body
      browserSpanAt apiKey "8a000000000000000000000000000008" "8a00000000000001" [("url.path", "/old")] "documentLoad" "session-wide-old" Nothing "wide-ui" (addUTCTime (-10 * 3600) frozenTime) tr
      browserSpanAt apiKey "8b000000000000000000000000000008" "8b00000000000001" [("url.path", "/new")] "documentLoad" "session-wide-new" Nothing "wide-ui" (addUTCTime (-60) frozenTime) tr
      (quick, quickHtml) <- load "24H" Nothing
      map (.id) quick.sessions `shouldBe` ["session-wide-new"]
      quick.servedStale `shouldBe` True
      quickHtml `shouldContainAll` ["refresh=1", "Refreshing the full time range"]
      -- The slice is never cached: a failed revalidation degrades instead of serving it as final.
      failed <- rumBody tr{trATCtx = tr.trATCtx{env = tr.trATCtx.env{enableTimefusionReads = True}}} $ tfUnavailable $ RUM.rumGetH projectId (Just "sessions") Nothing Nothing Nothing Nothing (Just "24H") Nothing Nothing (Just "sessions") (Just "1") (Just "1")
      failed.degradedPanels `shouldBe` [RUMData.SessionSearchQuery Nothing RUMData.AllSessionRows]
      (full, fullHtml) <- load "24H" (Just "1")
      (map (.id) full.sessions, full.servedStale) `shouldBe` (["session-wide-new", "session-wide-old"], False)
      T.isInfixOf "Refreshing the full time range" fullHtml `shouldBe` False
      -- The full result is what got cached, so the next cold-free load is complete at once.
      fst <$> load "24H" Nothing >>= \cached -> (map (.id) cached.sessions, cached.servedStale) `shouldBe` (["session-wide-new", "session-wide-old"], False)
      -- Last night's list still paints first the next morning, then revalidates.
      Cache.purge tr.trATCtx.rumCache
      withResource tr.trPool $ \conn -> void $ PG.execute_ conn "UPDATE rum_panel_cache SET expires_at = now() - interval '9 hours'"
      fst <$> load "24H" Nothing >>= \overnight -> (map (.id) overnight.sessions, overnight.servedStale) `shouldBe` (["session-wide-new", "session-wide-old"], True)
      -- Narrow windows scan themselves in one read.
      purgeRumCaches tr
      fst <$> load "6H" Nothing >>= \narrow -> (map (.id) narrow.sessions, narrow.servedStale) `shouldBe` (["session-wide-new"], False)

    it "staleSessionList_liveSelection_keepsRevalidationInsideTheSwappedChild" \tr -> do
      purgeRumCaches tr
      projectId <- createTestProject tr "RUM stale live selection"
      apiKey <- createTestAPIKey tr projectId "rum-live-stale-key"
      let ingest trId spId sid path at = ingestSpanReq tr $ mkSpanRequest trId spId Nothing "documentLoad" [] Nothing [mkAttr "session.id" sid, mkAttr "url.path" path, mkAttr "error.type" "LiveError"] (mkResource apiKey [mkAttr "service.name" "live-stale-browser", mkAttr "deployment.environment.name" "preview"]) at
          scoped = withService (Just "live-stale-browser") tr
          load refreshM = do
            (_, response@(RUM.RumGet (PageCtx _ body))) <- testServant scoped $ RUM.rumGetScopedH projectId (Just "sessions") (Just "session-live") (Just "errors") Nothing Nothing (Just "6H") (Just "session-live-old") Nothing (Just "sessions") (Just "1") refreshM (Just "preview")
            (,htmlOf response) <$> deferredBody body
          -- HTMX's hx-select retains this section and discards its siblings.
          selectedList = fst . T.breakOn "</section>" . snd . T.breakOn "id=\"rum-sessions-list\""
      ingest "88000000000000000000000000000008" "8800000000000001" "session-live-old" "/old" (addUTCTime (-1) frozenTime)
      (initial, _) <- load Nothing
      map (.id) initial.sessions `shouldBe` ["session-live-old"]
      initial.servedStale `shouldBe` False
      ingest "89000000000000000000000000000008" "8900000000000001" "session-live-new" "/new" frozenTime
      Cache.purge tr.trATCtx.rumCache
      withResource tr.trPool $ \conn -> void $ PG.execute_ conn "UPDATE rum_panel_cache SET expires_at = now() - interval '1 minute'"
      (stale, staleHtml) <- load Nothing
      stale.servedStale `shouldBe` True
      map (.id) stale.sessions `shouldBe` ["session-live-old"]
      selectedList staleHtml `shouldContainAll` ["refresh=1", "load delay:600ms", "hx-target=\"#rum-sessions-list\"", "hx-select=\"#rum-sessions-list\"", "hx-swap=\"outerMorph\"", "hx-sync=\"#rum-session-search-form:abort\"", "hx-include=\"#rum-session-search-form\"", "q=session-live", "filter=errors", "session=session-live-old", "environment=preview", "since=6H"]
      T.isInfixOf "id=\"rum-replay-workspace\"" (selectedList staleHtml) `shouldBe` False
      (fresh, freshHtml) <- load (Just "1")
      fresh.servedStale `shouldBe` False
      map (.id) fresh.sessions `shouldBe` ["session-live-new", "session-live-old"]
      selectedList freshHtml `shouldContainAll` ["/new", "/old", "aria-current=\"true\""]
      T.isInfixOf "refresh=1" (selectedList freshHtml) `shouldBe` False
      (.id) <$> fresh.selectedSessionData `shouldBe` Just "session-live-old"

    it "audiencePanel_classifiesUserAgentsIntoBrowserOsAndDevice" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-audience-key"
      let chromeUa = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36"
          iphoneUa = "Mozilla/5.0 (iPhone; CPU iPhone OS 17_5 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Mobile/15E148 Safari/604.1"
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000001" [("url.path", "/a"), ("user_agent.original", chromeUa)] "Pageview · /a" "session-ua-1" Nothing "storefront" tr
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000002" [("url.path", "/a"), ("user_agent.original", chromeUa)] "Pageview · /a" "session-ua-2" Nothing "storefront" tr
      browserSpan apiKey "70000000000000000000000000000007" "7000000000000003" [("url.path", "/b"), ("user_agent.original", iphoneUa)] "Pageview · /b" "session-ua-3" Nothing "storefront" tr
      panel <- renderPanel tr Nothing Nothing Nothing Nothing Nothing (Just "audience")
      panel `shouldContainAll` ["Audience", "Chrome", "Windows", "Safari", "iOS", "Mobile", "Desktop", "2 sessions"]

    it "sessionSearch_matchesBeforeLimits_andKeepsCrossStoreContext" \tr -> do
      purgeRumCaches tr
      apiKey <- createTestAPIKey tr testPid "rum-search-limit-key"
      let oldId = "00000000-0000-0000-000b-000000000001"
          oldTime = addUTCTime (-3600) frozenTime
          recentId :: Int -> Text
          recentId n = "00000000-0000-0000-000a-" <> T.justifyRight 12 '0' (show n)
      browserSpanAt apiKey "e0000000000000000000000000000001" "e000000000000001" [("url.path", "/needle%_path")] "documentLoad" oldId Nothing "bulk-ui" oldTime tr
      browserSpanAt apiKey "e0000000000000000000000000000001" "e000000000000002" [("url.path", "/checkout"), ("exception.type", "TypeError")] "documentLoad" oldId Nothing "bulk-ui" (addUTCTime 1 oldTime) tr
      forM_ ([1 .. 201] :: [Int]) $ \n ->
        browserSpan apiKey ("f" <> T.justifyRight 31 '0' (show n)) (T.justifyRight 16 '0' (show n)) [("url.path", "/recent")] "documentLoad" (recentId n) Nothing "bulk-ui" tr
      withResource tr.trPool $ \conn -> do
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) VALUES (?::uuid, ?, ?, ?, 1, 'Archive analyst')" (oldId, testPid, oldTime, addUTCTime 60 oldTime)
        void $ PG.execute conn "INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name) SELECT ('00000000-0000-0000-000a-' || lpad(i::text, 12, '0'))::uuid, ?, ?, ?, 1, 'Recent shopper' FROM generate_series(1, 201) i" (testPid, frozenTime, addUTCTime 60 frozenTime)
      -- Both a historical page and recording-only identity must find the old session,
      -- retain all its events, and attach its recording, despite 201 newer rows.
      forM_ ["needle%_path", "ARCHIVE ANALYST", oldId] $ \query -> do
        rows <- renderPanel tr (Just "sessions") (Just query) Nothing Nothing (Just "bulk-ui") (Just "sessions")
        rows `shouldContainAll` ["Archive analyst", "2 views", "2 events", "Replay", "/checkout"]
        T.count "class=\"rum-session-link" rows `shouldBe` 1
      errors <- renderPanel tr (Just "sessions") Nothing (Just "errors") Nothing (Just "bulk-ui") (Just "sessions")
      errors `shouldContainAll` ["Archive analyst", "1 error"]
      -- Selecting an old deep link does not depend on it surviving the list's limit.
      detail <- renderPanel tr (Just "sessions") Nothing Nothing (Just oldId) (Just "bulk-ui") (Just "sessions")
      detail `shouldContainAll` ["Newest 200 sessions", "initialSession=\"" <> oldId <> "\""]

    it "pageLinks_findAbsoluteUrlEventsWithoutWideningTheirRouteOrScope" \tr -> do
      apiKey <- createTestAPIKey tr testPid "rum-page-links-key"
      let service = "route-links"
          environment = "route-links-env"
          ingest index attribute url svc env at =
            ingestSpanReq tr $ mkSpanRequest ("d" <> T.justifyRight 31 '0' (show index)) ("d" <> T.justifyRight 15 '0' (show index)) Nothing "documentLoad" [] Nothing [mkAttr attribute url] (mkResource apiKey [mkAttr "service.name" svc, mkAttr "user_agent.original" "Mozilla/5.0", mkAttr "deployment.environment.name" env]) at
      forM_ (zip ([1 ..] :: [Int]) ["/cart", "/cart?coupon=x", "/cart#section", "/cartoon", "/other/cart", "/search?q=/cart", "/Cart", "/cart/"]) \(index, path) ->
        ingest index "url.full" ("https://shop.example" <> path) service environment frozenTime
      ingest 9 "url.path" "/cart" service environment frozenTime
      ingest 10 "url.full" "https://shop.example/cart" "other-service" environment frozenTime
      ingest 11 "url.full" "https://shop.example/cart" service "other-environment" frozenTime
      ingest 12 "url.full" "https://shop.example/cart" service environment $ addUTCTime (-120) frozenTime
      ingestMetric tr apiKey [mkAttr "service.name" service, mkAttr "deployment.environment.name" environment] [mkAttr "page.url" "https://shop.example/cart"] "browser.web_vital.lcp" 1200 frozenTime
      forM_ (zip ([13 ..] :: [Int]) ["https://shop.example", "https://shop.example/", "/?q=x", "/cart", "https://shop.example/a.b+(x)", "https://shop.example/aZbxxx", "https://shop.example/literal%2Fsegment"]) \(index, url) ->
        ingest index "url.full" url service environment frozenTime
      forM_ ["/", "/cart/", "/a.b+(x)", "/literal%2Fsegment"] \route ->
        ingestMetric tr apiKey [mkAttr "service.name" service, mkAttr "deployment.environment.name" environment] [mkAttr "page.url" $ "https://shop.example" <> route] "browser.web_vital.lcp" 1200 frozenTime
      let from = toText $ iso8601Show $ addUTCTime (-60) frozenTime
          to = toText $ iso8601Show $ addUTCTime 60 frozenTime
          scoped = withService (Just service) tr
      forM_ ["pages", "vital_trend"] \panel -> do
        html <- shellHtml scoped $ RUM.rumGetScopedH testPid (Just "performance") Nothing Nothing (Just from) (Just to) Nothing Nothing (Just service) (Just panel) (Just "1") Nothing (Just environment)
        forM_ ([("/cart", 5), ("/", 3), ("/cart/", 1), ("/a.b+(x)", 1), ("/literal%2Fsegment", 1)] :: [(Text, Int)]) \(route, count) -> do
          let label = if panel == "pages" then ">" <> route <> "</a>" else "aria-label=\"https://shop.example" <> route <> "\""
          html `shouldContainAll` [label]
          let href = T.takeWhile (/= '"') $ snd $ T.breakOnEnd "href=\"" $ fst $ T.breakOn label html
              params = parseQueryText $ encodeUtf8 $ T.replace "&amp;" "&" $ snd $ T.breakOn "?" href
          lookup "from" params `shouldBe` Just (Just from)
          lookup "to" params `shouldBe` Just (Just to)
          query <- maybe (fail "RUM page link omitted its Explorer query") pure $ join $ lookup "query" params
          (_, events) <- testServant tr $ Log.logExplorerDataH testPid def{Log.query = Just query, Log.from = Just from, Log.to = Just to}
          events.error `shouldBe` Nothing
          events.queryResultCount `shouldBe` if panel == "pages" then count else 1
          forM_ [events.nextUrl, events.resetLogsUrl, events.recentUrl] \url ->
            join (lookup "query" $ parseQueryText $ encodeUtf8 $ snd $ T.breakOn "?" url) `shouldBe` Just query
