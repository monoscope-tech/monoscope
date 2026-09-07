module Pages.ReportsSpec (spec) where

import BackgroundJobs qualified
import Codec.Compression.GZip qualified as GZip
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64URL
import Data.Default (def)
import Data.Effectful.Notify (EmailData (..), Notification (..))
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid (renderText, toHtml)
import Models.Apis.Issues (ReportListItem (..))
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers qualified as Containers
import Models.Telemetry.Report qualified as Report
import Pages.BodyWrapper (PageCtx (..))
import Pages.Reports qualified as Reports
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.EmailTemplates qualified as Email
import Pkg.Parser (parseQueryToAST)
import Pkg.TestUtils
import Relude
import Test.Hspec
import UnliftIO.Async (concurrently)
import Utils (formatWithCommas)


spec :: Spec
spec = around withTestResources do
  describe "Report Notification Lifecycle" do
    it "delivers enabled daily and weekly reports, records a muted report, and isolates report details" \tr -> do
      (_, initialPage) <- testServant tr $ Reports.reportsGetH testPid Nothing Nothing Nothing
      case initialPage of
        Reports.ReportsGetMain (PageCtx _ (pid, _, _)) -> pid `shouldBe` testPid
        _ -> fail "the reports page did not load"
      initial <- runTestBg frozenTime tr $ Projects.projectById testPid
      ((.dailyNotif) <$> initial, (.weeklyNotif) <$> initial) `shouldBe` (Just False, Just True)

      void $ testServant tr $ Reports.reportsPostH testPid Projects.RTDaily
      void $ testServant tr $ Reports.reportsPostH testPid Projects.RTWeekly
      void $ withResource tr.trPool \conn ->
        PGS.execute
          conn
          [sql|UPDATE projects.teams
                SET discord_channels = ARRAY['daily-reports'], disabled_channels = '{}'
                WHERE project_id = ? AND handle = 'everyone'|]
          (PGS.Only testPid)

      sent <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.DailyReports testPid)
      sent `shouldSatisfy` any (\case DiscordNotification{} -> True; _ -> False)

      void $ testServant tr $ Reports.reportsPostH testPid Projects.RTDaily
      advanceDays tr 1
      muted <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.DailyReports testPid)
      muted `shouldBe` []

      void $ testServant tr $ Reports.reportsPostH testPid Projects.RTWeekly
      weekly <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.WeeklyReports testPid)
      weekly `shouldSatisfy` any (\case DiscordNotification{} -> True; _ -> False)
      stored <- maybe (fail "weekly snapshot was not saved") pure =<< runTestBg frozenTime tr (Issues.getLatestReportByType testPid Projects.RTWeekly)
      snapshot <- either fail pure $ AET.parseEither (AE.withObject "report" (AE..: "systemSnapshot")) stored.reportJson
      let (events, errors, _, _) = Reports.snapshotTotals snapshot
          headline = if events == 0 then "No telemetry events" else formatWithCommas (fromIntegral errors) <> " error events across " <> formatWithCommas (fromIntegral events) <> " telemetry events"
          emails = [email | EmailNotification email <- weekly]
      emails `shouldSatisfy` (not . null)
      forM_ emails $ \email -> do
        email.subject `shouldSatisfy` T.isPrefixOf "Weekly system report"
        email.htmlBody `shouldContainAll` ["Services", "Infrastructure", "Monitors", headline]

      (_, reportsPage) <- testServant tr $ Reports.reportsGetH testPid Nothing Nothing Nothing
      reportId <- case reportsPage of
        Reports.ReportsGetMain (PageCtx _ (_, reports, _)) -> do
          V.length reports `shouldBe` 3
          let pageHtml = toText $ renderText $ toHtml reportsPage
              latestUrl = "/p/" <> testPid.toText <> "/reports/" <> maybe "" ((.toText) . (.id)) (reports V.!? 0)
          pageHtml `shouldContainAll` ["Generate live report", "hx-get=\"" <> latestUrl <> "\""]
          find (\tag -> T.isPrefixOf "a " tag && T.isInfixOf "/reports/live" tag) (T.splitOn "<" pageHtml)
            `shouldSatisfy` maybe False (T.isInfixOf "hx-preload=\"false\"")
          snd (T.breakOn "id=\"detailSidebar\"" pageHtml) `shouldSatisfy` (not . T.isInfixOf "/reports/live")
          V.any ((== Projects.RTWeekly) . (.reportType)) reports `shouldBe` True
          maybe (fail "the daily report was not listed") (pure . (.id)) $ V.find ((== Projects.RTDaily) . (.reportType)) reports
        _ -> fail "the reports page did not load after generation"
      (_, reportPage) <- testServant tr $ Reports.singleReportGetH testPid reportId Nothing
      case reportPage of
        Reports.ReportsGetSingle (PageCtx _ (reportType, dateLabel, emailHtml)) -> do
          reportType `shouldBe` "daily"
          dateLabel `shouldNotBe` ""
          emailHtml `shouldContainAll` ["Daily system report", "Services", "Infrastructure"]
        _ -> fail "the report detail did not load"

      -- Weekly emails used to be rendered by the job in the SERVER's timezone, so a
      -- customer in UTC+13 saw the wrong week. renderWeeklyEmail is the one renderer now,
      -- and it dates the report in the project's IANA timezone.
      project <- maybe (fail "test project missing") pure =<< runTestBg frozenTime tr (Projects.projectById testPid)
      let elevenAmUTC = UTCTime (fromGregorian 2025 1 1) (11 * 3600)
      (tzDateLabel, tzSubject, _) <-
        runTestBg frozenTime tr
          $ Reports.renderWeeklyEmail Projects.RTWeekly "p/x/reports" project{Projects.timeZone = "Pacific/Auckland"} "Ada" elevenAmUTC elevenAmUTC False (Email.HistoricalEvidence Email.HistoricalReportEvidence{totalEvents = 0, totalErrors = 0, anomalies = V.empty, performance = V.empty, slowQueries = V.empty})
      tzDateLabel `shouldBe` "2025-01-02"
      tzSubject `shouldNotBe` ""

      let otherPid = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
      (_, otherProjectPage) <- testServant tr $ Reports.singleReportGetH otherPid reportId Nothing
      case otherProjectPage of
        Reports.ReportsGetSingle (PageCtx _ (reportType, dateLabel, emailHtml)) ->
          (reportType, dateLabel, emailHtml) `shouldBe` ("unknown", "Report not found", "")
        _ -> fail "the other project report page did not load"

      let legacyJson =
            [aesonQQ|{
            "endpoints": [], "errors": {"total": 7, "change": 2}, "events": {"total": 1234, "change": 1},
            "spanTypeStats": [], "slowDbQueries": [{"query": "SELECT legacy", "averageDuration": 1250000000, "totalEvents": 34}],
            "errorDataset": {"source": []}, "eventsDataset": {"source": []}, "issues": []
          }|]
      void $ withResource tr.trPool $ \conn -> PGS.execute conn [sql|UPDATE apis.reports SET report_json = ? WHERE id = ?|] (legacyJson, reportId)
      (_, legacyPage) <- testServant tr $ Reports.singleReportGetH testPid reportId Nothing
      case legacyPage of
        Reports.ReportsGetSingle (PageCtx _ (_, _, html)) -> html `shouldContainAll` ["Historical report", "Daily system report", "1,234", "SELECT legacy", "1250.0 ms", "34"]
        _ -> fail "the legacy report detail did not load"

    it "counts the complete service period, separates request metrics, and retains missing baselines" \tr -> do
      let start = addUTCTime (-(7 * 86400)) frozenTime
          addRow at service env kind status duration = withResource tr.trPool $ \conn ->
            void
              $ PGS.execute
                conn
                [sql| INSERT INTO otel_logs_and_spans
                (id, project_id, timestamp, start_time, resource___service___name,
                 resource___deployment___environment___name, kind, status_code, duration, context, summary)
                VALUES (gen_random_uuid(), ?, ?, ?, ?, ?, ?, ?, ?, '{}'::jsonb, '{}') |]
                (testPid, at, at, service :: Maybe Text, env :: Maybe Text, kind :: Text, status :: Text, duration :: Int64)
      addRow (addUTCTime (-1) start) (Just "api") (Just "prod") "server" "OK" 100000000
      addRow start (Just "api") (Just "prod") "server" "ERROR" 250000000
      addRow (addUTCTime 1 start) (Just "api") (Just "prod") "client" "OK" 900000000
      addRow (addUTCTime 2 start) Nothing Nothing "log" "ERROR" 0
      addRow frozenTime (Just "api") (Just "prod") "server" "ERROR" 990000000
      current <- runTestBg frozenTime tr $ Report.serviceStats False testPid start frozenTime
      previous <- runTestBg frozenTime tr $ Report.serviceStats False testPid (addUTCTime (-(7 * 86400)) start) start
      let api = find ((== Just "api") . (.service)) current
          unnamed = find (isNothing . (.service)) current
      fmap (\r -> (r.events, r.errorEvents, r.serverRequests, r.serverErrors, r.serverLatencyMs)) api `shouldBe` Just (2, 1, 1, 1, Just 250)
      fmap (\r -> (r.events, r.errorEvents, r.logs)) unnamed `shouldBe` Just (1, 1, 1)
      sum (map (.events) previous) `shouldBe` 1
      let comparisons = Report.compareServices current previous
      (find (isNothing . (.service)) comparisons >>= (.previous)) `shouldBe` Nothing
      (find ((== Just "api") . (.service)) comparisons >>= (.previous) <&> (.serverLatencyMs)) `shouldBe` Just (Just 100)
      -- A non-aligned slice boundary shares an hourly bucket. Its disjoint
      -- counts must be combined without duplicating the boundary event.
      let trendStart = addUTCTime 1 start
          boundary = addUTCTime 86400 trendStart
      addRow (addUTCTime (-1) boundary) (Just "api") Nothing "log" "OK" 0
      addRow boundary (Just "api") Nothing "log" "OK" 0
      void $ withResource tr.trPool $ \conn ->
        PGS.execute
          conn
          [sql|UPDATE otel_logs_and_spans
          SET events = '[{"event_name":"exception","event_attributes":{"exception":{"type":"EmbeddedError"}}}]'::jsonb
          WHERE project_id = ? AND timestamp = ?|]
          (testPid, boundary)
      trends <- runTestBg frozenTime tr $ Report.trendStats False testPid trendStart frozenTime
      (sum $ map (.events) trends, sum $ map (.errors) trends) `shouldBe` (4, 2)
      length (map (.epochSeconds) trends) `shouldBe` length (ordNub $ map (.epochSeconds) trends)

    it "queries HTTP, database, and workload evidence without mixing their duration units" \tr -> do
      let start = addUTCTime (-(7 * 86400)) frozenTime
      void $ withResource tr.trPool $ \conn ->
        PGS.execute
          conn
          [sql| INSERT INTO otel_logs_and_spans
          (id, project_id, timestamp, start_time, kind, resource___service___name,
           resource___deployment___environment___name, attributes___http___request___method,
           attributes___url___path, attributes___server___address, attributes___db___query___text,
           duration, context, summary)
          SELECT gen_random_uuid(), ?, ?, ?, kind, 'api', 'production', method,
            '/orders', 'api.example.com', statement, duration, '{}'::jsonb, '{}'
          FROM (VALUES ('server', 'POST', NULL, 250000000::bigint),
                       ('server', 'POST', NULL, 750000000::bigint),
                       ('client', NULL, 'SELECT secret FROM users', 1250000000::bigint),
                       ('log', NULL, NULL, 0::bigint)) AS fixtures(kind, method, statement, duration) |]
          (testPid, start, start)
      endpoints <- runTestBg frozenTime tr $ Report.endpointStats False testPid start frozenTime
      fmap (\e -> (e.service, e.environment, e.method, e.path, e.requests, e.averageMs)) endpoints
        `shouldBe` [(Just "api", Just "production", "POST", "/orders", 2, Just 500)]
      databases <- runTestBg frozenTime tr $ Report.databaseStats False testPid start frozenTime
      fmap (\q -> (q.statement, q.operations, q.averageMs)) databases
        `shouldBe` [("SELECT secret FROM users", 1, 1250)]
      workloads <- runTestBg frozenTime tr $ Report.workloadStats False testPid start frozenTime
      sum (map (.events) workloads) `shouldBe` 4
      (find ((== "log") . (.kind)) workloads >>= (.averageMs)) `shouldBe` Nothing
      Report.endpointStats False testPid frozenTime (addUTCTime 1 frozenTime)
        & runTestBg frozenTime tr
        >>= (`shouldBe` [])

    it "persists the snapshot and bounds email details while retaining full report evidence" \tr -> do
      project <- maybe (fail "test project missing") pure =<< runTestBg frozenTime tr (Projects.projectById testPid)
      emptySnapshot <- runTestBg frozenTime tr $ Reports.collectSystemReport testPid (addUTCTime (-(7 * 86400)) frozenTime) frozenTime
      let stats n = Report.ServiceStats (Just $ "service-" <> show n) (Just "production") 10 1 0 10 1 10 (Just 125)
          snapshot =
            emptySnapshot
              { Report.services = Report.compareServices (map stats [1 .. 30 :: Int]) []
              , Report.databases = Report.Available [Report.DatabaseStats (Just "api") (T.replicate 2000 "<script>private SQL &</script>") 1250 7]
              , Report.infrastructure =
                  Report.Available
                    $ Report.InfrastructureStats
                      3
                      100
                      2
                      (replicate 10 $ Report.InfrastructureResource (T.replicate 100 "resource-") "Container" (Just "host") (Just "production") (Just "namespace") (Just 0.9) (Just 0.8) (Just 0.7) (Just False) (Just 7))
                      10
                      (addUTCTime (-900) frozenTime)
                      frozenTime
              , Report.monitors =
                  Report.Available
                    $ Report.MonitorStats
                      0
                      0
                      12
                      0
                      0
                      (replicate 12 $ Report.MonitorObservation "monitor-id" (T.replicate 50 "Monitor title ") "Alerting" (Just 850) (Just frozenTime))
              , Report.issues =
                  Report.Available
                    $ Report.IssueStats
                      10
                      20
                      10
                      5
                      5
                      (replicate 10 $ Report.IssueObservation "issue-id" (T.replicate 50 "Issue title ") (Just "api") "critical" "runtime_exception" 9999)
              , Report.performance =
                  Report.Available
                    $ replicate 30
                    $ Report.EndpointComparison
                      (Report.EndpointStats (Just "api") (Just "production") "api.example.com" "GET" ("/" <> T.replicate 300 "a") (Just 500) 1000)
                      Nothing
              , Report.workloads = Report.Available [Report.WorkloadStats kind 10000 (Just 100) | kind <- ["server", "client", "consumer", "producer", "internal", "unspecified", "log"]]
              , Report.topPatterns = Report.Available $ replicate 5 (T.replicate 100 "pattern ", 100000, "body")
              , Report.trends = Just $ Report.Available [Report.TrendPoint 1735689600 100 7]
              }
          json = Reports.buildReportJson' snapshot
      AET.parseEither (AE.withObject "report" (AE..: "systemSnapshot")) json `shouldBe` Right snapshot
      let withoutTrends = case AE.toJSON snapshot of
            AE.Object fields -> AE.Object $ KM.delete "trends" fields
            value -> value
      AE.fromJSON withoutTrends `shouldBe` AE.Success snapshot{Report.trends = Nothing}
      -- Both replicas may request the same preview; exactly one owns generation.
      (firstClaim, secondClaim) <-
        concurrently
          (runTestBg frozenTime tr $ Report.claimPreview testPid)
          (runTestBg frozenTime tr $ Report.claimPreview testPid)
      token <- case catMaybes [firstClaim, secondClaim] of
        [owner] -> pure owner
        _ -> fail "preview generation was not claimed exactly once"
      (_, pendingPage) <- testServant tr $ Reports.reportsLiveGetH testPid (Just "true")
      toText (renderText $ toHtml pendingPage) `shouldContainAll` ["Preparing your system report", "every 2s", "live-report-preview"]
      runTestBg frozenTime tr $ Report.finishPreview testPid token (Report.PreviewReady snapshot)
      runTestBg frozenTime tr (Report.getPreview testPid) >>= (`shouldBe` Just (Report.PreviewReady snapshot))
      runTestBg frozenTime tr (Report.getPreview $ UUIDId $ UUID.fromWords 9 8 7 6) >>= (`shouldBe` Nothing)
      (_, readyPage) <- testServant tr $ Reports.reportsLiveGetH testPid (Just "true")
      let readyHtml = toText $ renderText $ toHtml readyPage
      readyHtml `shouldContainAll` ["live-report-preview", "srcdoc", "Infrastructure", "Monitors"]
      readyHtml `shouldSatisfy` (not . T.isInfixOf "every 2s")
      void $ withResource tr.trPool $ \conn -> PGS.execute conn [sql|UPDATE apis.report_previews SET expires_at = now() - interval '1 second' WHERE project_id = ?|] (PGS.Only testPid)
      retryToken <- maybe (fail "expired preview could not be retried") pure =<< runTestBg frozenTime tr (Report.claimPreview testPid)
      -- An old worker finishing after a retry must not overwrite the new lease.
      runTestBg frozenTime tr $ Report.finishPreview testPid token (Report.PreviewReady snapshot)
      runTestBg frozenTime tr (Report.getPreview testPid) >>= (`shouldBe` Just Report.PreviewBuilding)
      runTestBg frozenTime tr $ Report.finishPreview testPid retryToken Report.PreviewFailed
      (_, failedPage) <- testServant tr $ Reports.reportsLiveGetH testPid (Just "true")
      toText (renderText $ toHtml failedPage) `shouldContainAll` ["The report could not be prepared", "every 60s", "View saved reports"]
      (_, _, email) <- runTestBg frozenTime tr $ Reports.renderSystemEmail Projects.RTWeekly "/reports/test" project "Ada" False snapshot
      (_, _, full) <- runTestBg frozenTime tr $ Reports.renderSystemEmail Projects.RTWeekly "/reports/test" project "Ada" True snapshot
      charts <- forM (drop 1 $ T.splitOn "widgetZ=" email) $ \suffix -> do
        compressed <- either (fail . toString) pure $ B64URL.decodeBase64Untyped $ encodeUtf8 $ T.takeWhile (/= '&') suffix
        either fail pure $ AE.eitherDecode @Widget.Widget $ GZip.decompress $ fromStrict compressed
      map (.query) charts `shouldBe` replicate 4 Nothing
      map (fmap (.source) . (.dataset)) charts
        `shouldBe` concatMap (replicate 2 . Just) [[aesonQQ|[["Time","Events"],[1735689600000,100]]|], [aesonQQ|[["Time","Errors"],[1735689600000,7]]|]]
      email `shouldContainAll` ["appearance=light", "appearance=dark", "logo-white-ink.png", "email-image-dark", "View 25 more service comparisons", "&lt;script&gt;", "View 8 more monitors", "View 25 more endpoints"]
      email `shouldSatisfy` (not . T.isInfixOf "<script>")
      BS.length (encodeUtf8 email) `shouldSatisfy` (< 80000)
      full `shouldSatisfy` (not . T.isInfixOf "View 25 more service comparisons")
      T.count "Avg request" full `shouldBe` 30
      T.count "Avg request" email `shouldBe` 5
      (_, _, partial) <- runTestBg frozenTime tr $ Reports.renderSystemEmail Projects.RTWeekly "/reports/test" project "Ada" False snapshot{Report.infrastructure = Report.Unavailable, Report.trends = Just Report.Unavailable}
      partial `shouldContainAll` ["Infrastructure metrics could not be loaded", "Activity trends unavailable", "Services"]
      let quiet =
            emptySnapshot
              { Report.services = []
              , Report.infrastructure = Report.Available $ Report.InfrastructureStats 0 0 0 [] 0 (addUTCTime (-900) frozenTime) frozenTime
              , Report.monitors = Report.Available $ Report.MonitorStats 0 0 0 0 0 []
              , Report.issues = Report.Available $ Report.IssueStats 0 0 0 0 0 []
              , Report.performance = Report.Available []
              , Report.databases = Report.Available []
              , Report.workloads = Report.Available []
              , Report.topPatterns = Report.Available []
              , Report.ingestionCapped = Just False
              }
      (_, _, quietEmail) <- runTestBg frozenTime tr $ Reports.renderSystemEmail Projects.RTWeekly "/reports/test" project "Ada" False quiet
      let historical = Email.HistoricalEvidence Email.HistoricalReportEvidence{totalEvents = 0, totalErrors = 0, anomalies = V.empty, performance = V.empty, slowQueries = V.replicate 15 ("SELECT historical_slow_query", 900000000, 10)}
      (_, _, historicalEmail) <- runTestBg frozenTime tr $ Reports.renderWeeklyEmail Projects.RTWeekly "/reports/test" project "Ada" frozenTime frozenTime False historical
      (_, _, historicalFull) <- runTestBg frozenTime tr $ Reports.renderWeeklyEmail Projects.RTWeekly "/reports/test" project "Ada" frozenTime frozenTime True historical
      T.count "SELECT historical_slow_query" historicalEmail `shouldBe` 4
      T.count "SELECT historical_slow_query" historicalFull `shouldBe` 15
      historicalEmail `shouldContainAll` ["View all slow queries", "View all endpoints"]
      quietEmail `shouldContainAll` ["No telemetry events", "No HTTP server spans", "No infrastructure metrics observed", "No monitors configured"]

    it "counts issue lifecycle states before selecting the highest-priority evidence" \tr -> do
      let start = addUTCTime (-(7 * 86400)) frozenTime
      void $ withResource tr.trPool $ \conn ->
        PGS.execute
          conn
          [sql|
        INSERT INTO apis.issues (project_id, issue_type, target_hash, title, service, created_at, critical, severity, affected_requests, acknowledged_at, archived_at)
        SELECT ?, 'runtime_exception', gen_random_uuid()::text, title, 'api', ?, critical, severity, impact, ack, archived
        FROM (VALUES ('critical from severity', false, 'critical', 1, NULL::timestamptz, NULL::timestamptz),
                     ('critical from flag', true, 'info', 2, NULL, NULL),
                     ('high-volume warning', false, 'warning', 9999, NULL, NULL),
                     ('acknowledged', false, 'info', 1, ?::timestamptz, NULL),
                     ('archived', false, 'info', 1, NULL, ?::timestamptz)) AS fixtures(title, critical, severity, impact, ack, archived)
        |]
          (testPid, start, start, start)
      issues <- runTestBg frozenTime tr $ Report.issueStats testPid start frozenTime
      (issues.newIssues, issues.openIssues, issues.criticalOpen, issues.acknowledged, issues.archivedInPeriod) `shouldBe` (5, 3, 2, 1, 1)
      map (.title) issues.priorities `shouldBe` ["critical from flag", "critical from severity", "high-volume warning"]
      map (.severity) (take 2 issues.priorities) `shouldBe` ["critical", "critical"]

    it "counts all infrastructure resources and distinguishes unevaluated and paused monitors" \_ -> do
      let row =
            Containers.ContainerRow
              { containerName = "container"
              , scope = Containers.ScopeContainer
              , podName = Nothing
              , namespace = Nothing
              , nodeName = Nothing
              , cluster = Nothing
              , provider = Nothing
              , region = Nothing
              , osType = Nothing
              , architecture = Nothing
              , image = Nothing
              , imageTag = Nothing
              , workload = Nothing
              , cpuCores = Just 1
              , cpuLimit = Nothing
              , cpuRequest = Nothing
              , memBytes = Just 1024
              , memLimit = Just 2048
              , memRequest = Nothing
              , load1 = Nothing
              , storagePct = Just 0.75
              , uptime = Nothing
              , restarts = Just 3
              , ready = Just 0
              }
          infrastructure =
            Report.infrastructureStats (addUTCTime (-900) frozenTime) frozenTime
              $ V.fromList
              $ replicate 501 row
              <> [row{Containers.scope = Containers.ScopeHost, Containers.ready = Nothing}, row{Containers.scope = Containers.ScopePod}]
          evaluated = def{Monitors.lastEvaluated = Just frozenTime, Monitors.currentStatus = Monitors.MSAlerting, Monitors.currentValue = 42}
          monitors =
            Report.monitorStats
              [ evaluated
              , evaluated{Monitors.currentStatus = Monitors.MSWarning}
              , evaluated{Monitors.currentStatus = Monitors.MSNormal}
              , evaluated{Monitors.deactivatedAt = Just frozenTime}
              , evaluated{Monitors.lastEvaluated = Nothing}
              ]
      (infrastructure.hosts, infrastructure.containers, infrastructure.pods, infrastructure.unready) `shouldBe` (1, 501, 1, 502)
      length infrastructure.resources `shouldBe` 10
      map (\r -> (r.cpuRatio, r.memoryRatio, r.storageRatio, r.restartCounter)) infrastructure.resources
        `shouldBe` replicate 10 (Nothing, Just 0.5, Just 0.75, Just 3)
      (monitors.alerting, monitors.warning, monitors.normal, monitors.paused, monitors.unevaluated) `shouldBe` (1, 1, 1, 1, 1)
      (find ((== "Not evaluated") . (.status)) monitors.observations >>= (.value)) `shouldBe` Nothing

    it "renders system evidence and correct duration units without image-only metrics" \_ -> do
      let (subject, body) = Email.sampleWeeklyReport "" ""
          html = Email.renderEmail subject body
      html `shouldContainAll` ["Services", "Infrastructure", "Issues to review", "Monitors", "checkout-api", "Not ready", "1250.0 ms", "3,400", "CPU / capacity", "No previous data"]
      traverse parseQueryToAST Reports.errorsWidget.query `shouldSatisfy` isRight
      html `shouldSatisfy` (not . T.isInfixOf "data:image/svg")
      html `shouldSatisfy` (not . T.isInfixOf "td:nth-child(n+2)")
