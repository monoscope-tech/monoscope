-- | Scheduled work end to end: daily/weekly system reports (evidence, links, emails, the live
-- preview) → error alerts (sweep, toggles, acknowledgement, subscriptions, endpoint gate) →
-- the hourly notification budget and its digest → retention expiry → dashboard auto-provision
-- → the daily scheduler that seeds every per-project job, ReportUsage included.
module BackgroundJobsSpec (spec) where

import BackgroundJobs qualified
import Codec.Compression.GZip qualified as GZip
import Control.Exception (bracket_)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64URL
import Data.Effectful.Notify (EmailData (..), Notification (..), SlackData (..))
import Data.List (lookup)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Notification qualified as PGN
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues (ReportListItem (..))
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers qualified as Containers
import Models.Telemetry.Report qualified as Report
import Network.HTTP.Types.URI (parseQueryText, urlDecode)
import OddJobs.Types (pgEventName)
import Pages.BodyWrapper (PageCtx (..))
import Pages.LogExplorer.Log qualified as Log
import Pages.Projects qualified as ProjectPages
import Pages.Reports qualified as Reports
import Pkg.Components.Widget qualified as Widget
import Pkg.EmailTemplates qualified as Email
import Pkg.Parser (QueryComponents (..), SqlQueryCfg (..), defSqlQueryCfg, parseQueryToAST, parseQueryToComponents)
import Pkg.TestUtils
import Servant qualified
import System.Config qualified as Config
import System.Timeout qualified as Timeout
import UnliftIO.Async (concurrently)
import Utils (formatWithCommas)


spec :: Spec
spec = around withTestResources do
  it "reports: enabled daily and weekly reports deliver, a muted one stays silent, details are isolated" \tr -> do
    let isDiscord = \case DiscordNotification{} -> True; _ -> False
        runReport job = fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ job testPid)
        listed = call tr (Reports.reportsGetH testPid Nothing Nothing Nothing)
    Reports.ReportsGetMain (PageCtx _ (initialPid, _, _)) <- listed
    initialPid `shouldBe` testPid
    ((.dailyNotif) &&& (.weeklyNotif)) <$> projectOf tr testPid `shouldReturn` (False, True)
    void $ testServant tr $ Reports.reportsPostH testPid Projects.RTDaily
    void $ testServant tr $ Reports.reportsPostH testPid Projects.RTWeekly
    dbExec tr [sql|UPDATE projects.teams SET discord_channels = ARRAY['daily-reports'], disabled_channels = '{}' WHERE project_id = ? AND handle = 'everyone'|] (Only testPid)
    length . filter isDiscord <$> runReport BackgroundJobs.DailyReports `shouldReturn` 1
    void $ testServant tr $ Reports.reportsPostH testPid Projects.RTDaily
    advanceDays tr 1
    runReport BackgroundJobs.DailyReports `shouldReturn` []

    void $ testServant tr $ Reports.reportsPostH testPid Projects.RTWeekly
    weekly <- runReport BackgroundJobs.WeeklyReports
    length (filter isDiscord weekly) `shouldBe` 1
    stored <- maybe (fail "weekly snapshot was not saved") pure =<< runTestBg frozenTime tr (Issues.getLatestReportByType testPid Projects.RTWeekly)
    snapshot <- either fail pure $ AET.parseEither (AE.withObject "report" (AE..: "systemSnapshot")) stored.reportJson
    let (events, errors, _, _) = Reports.snapshotTotals snapshot
        headline = if events == 0 then "No telemetry events" else formatWithCommas (fromIntegral errors) <> " error events across " <> formatWithCommas (fromIntegral events) <> " telemetry events"
        emails = [email | EmailNotification email <- weekly]
    length emails `shouldBe` 2
    for_ emails \email -> do
      email.subject `shouldSatisfy` T.isPrefixOf "Weekly system report"
      email.htmlBody `shouldContainAll` ["Services", "Infrastructure", "Monitors", headline]

    reportsPage <- listed
    reportId <- case reportsPage of
      Reports.ReportsGetMain (PageCtx _ (_, reports, _)) -> do
        (V.length reports, V.any ((== Projects.RTWeekly) . (.reportType)) reports) `shouldBe` (3, True)
        -- The newest report opens by default; the live link never preloads, and isn't in the detail sidebar.
        let pageHtml = renderT reportsPage
        goldenParts "reports-list-links" $ tagsWith ["/reports/live", "hx-get=\"/p/" <> testPid.toText <> "/reports/" <> maybe "" ((.toText) . (.id)) (reports V.!? 0) <> "\""] pageHtml
        T.concat (elementsWith ["id=\"detailSidebar\""] pageHtml) `shouldNotSatisfy` T.isInfixOf "/reports/live"
        maybe (fail "the daily report was not listed") (pure . (.id)) $ V.find ((== Projects.RTDaily) . (.reportType)) reports
      _ -> fail "the reports page did not load after generation"
    Reports.ReportsGetSingle (PageCtx _ (reportType, dateLabel, emailHtml)) <- call tr $ Reports.singleReportGetH testPid reportId Nothing
    (reportType, dateLabel /= "") `shouldBe` ("daily", True)
    emailHtml `shouldContainAll` ["Daily system report", "Services", "Infrastructure"]
    -- Weekly emails were dated in the SERVER's timezone, so a UTC+13 customer saw the wrong week.
    project <- projectOf tr testPid
    let elevenAmUTC = UTCTime (fromGregorian 2025 1 1) (11 * 3600)
    (tzDateLabel, tzSubject, _) <- runTestBg frozenTime tr $ Reports.renderWeeklyEmail Projects.RTWeekly "p/x/reports" project{Projects.timeZone = "Pacific/Auckland"} "Ada" elevenAmUTC elevenAmUTC False (Email.HistoricalEvidence Email.HistoricalReportEvidence{totalEvents = 0, totalErrors = 0, anomalies = V.empty, performance = V.empty, slowQueries = V.empty})
    (tzDateLabel, tzSubject /= "") `shouldBe` ("2025-01-02", True)
    Reports.ReportsGetSingle (PageCtx _ other) <- call tr $ Reports.singleReportGetH (UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0) reportId Nothing
    other `shouldBe` ("unknown", "Report not found", "")
    let legacyJson = [aesonQQ|{"endpoints": [], "errors": {"total": 7, "change": 2}, "events": {"total": 1234, "change": 1}, "spanTypeStats": [], "slowDbQueries": [{"query": "SELECT legacy", "averageDuration": 1250000000, "totalEvents": 34}], "errorDataset": {"source": []}, "eventsDataset": {"source": []}, "issues": []}|]
    dbExec tr [sql|UPDATE apis.reports SET report_json = ? WHERE id = ?|] (legacyJson, reportId)
    Reports.ReportsGetSingle (PageCtx _ (_, _, legacyHtml)) <- call tr $ Reports.singleReportGetH testPid reportId Nothing
    legacyHtml `shouldContainAll` ["Historical report", "Daily system report", "1,234", "SELECT legacy", "1250.0 ms", "34"]

  it "report evidence: service periods and baselines, HTTP/DB/workload units, issue states, infrastructure and monitors" \tr -> do
    let start = addUTCTime (-(7 * 86400)) frozenTime
        addRow pid at service env kind status duration =
          dbExec tr [sql|INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, resource___service___name, resource___deployment___environment___name, kind, status_code, duration, context, summary) VALUES (gen_random_uuid(), ?, ?, ?, ?, ?, ?, ?, ?, '{}'::jsonb, '{}')|] (pid, at, at, service :: Maybe Text, env :: Maybe Text, kind :: Text, status :: Text, duration :: Int64)
    svc <- createTestProject tr "report-services"
    addRow svc (addUTCTime (-1) start) (Just "api") (Just "prod") "server" "OK" 100000000
    addRow svc start (Just "api") (Just "prod") "server" "ERROR" 250000000
    addRow svc (addUTCTime 1 start) (Just "api") (Just "prod") "client" "OK" 900000000
    addRow svc (addUTCTime 2 start) Nothing Nothing "log" "ERROR" 0
    addRow svc frozenTime (Just "api") (Just "prod") "server" "ERROR" 990000000
    current <- runTestBg frozenTime tr $ Report.serviceStats False svc start frozenTime
    previous <- runTestBg frozenTime tr $ Report.serviceStats False svc (addUTCTime (-(7 * 86400)) start) start
    fmap (\r -> (r.events, r.errorEvents, r.serverRequests, r.serverErrors, r.serverLatencyMs)) (find ((== Just "api") . (.service)) current) `shouldBe` Just (2, 1, 1, 1, Just 250)
    fmap (\r -> (r.events, r.errorEvents, r.logs)) (find (isNothing . (.service)) current) `shouldBe` Just (1, 1, 1)
    sum (map (.events) previous) `shouldBe` 1
    let comparisons = Report.compareServices current previous
    (find (isNothing . (.service)) comparisons >>= (.previous)) `shouldBe` Nothing
    (find ((== Just "api") . (.service)) comparisons >>= (.previous) <&> (.serverLatencyMs)) `shouldBe` Just (Just 100)
    -- A non-aligned slice boundary shares an hourly bucket: combine without duplicating its event.
    let trendStart = addUTCTime 1 start
        boundary = addUTCTime 86400 trendStart
    addRow svc (addUTCTime (-1) boundary) (Just "api") Nothing "log" "OK" 0
    addRow svc boundary (Just "api") Nothing "log" "OK" 0
    dbExec tr [sql|UPDATE otel_logs_and_spans SET events = '[{"event_name":"exception","event_attributes":{"exception":{"type":"EmbeddedError"}}}]'::jsonb WHERE project_id = ? AND timestamp = ?|] (svc, boundary)
    trends <- runTestBg frozenTime tr $ Report.trendStats False svc trendStart frozenTime
    (sum $ map (.events) trends, sum $ map (.errors) trends) `shouldBe` (4, 2)
    length trends `shouldBe` length (ordNub $ map (.epochSeconds) trends)

    -- HTTP, database and workload evidence never mix their duration units.
    ev <- createTestProject tr "report-evidence"
    dbExec
      tr
      [sql|INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, kind, resource___service___name, resource___deployment___environment___name, attributes___http___request___method, attributes___url___path, attributes___server___address, attributes___db___query___text, duration, context, summary)
        SELECT gen_random_uuid(), ?, ?, ?, kind, 'api', 'production', method, '/orders', 'api.example.com', statement, duration, '{}'::jsonb, '{}'
        FROM (VALUES ('server', 'POST', NULL, 250000000::bigint), ('server', 'POST', NULL, 750000000::bigint), ('client', NULL, 'SELECT secret FROM users', 1250000000::bigint), ('log', NULL, NULL, 0::bigint)) AS fixtures(kind, method, statement, duration)|]
      (ev, start, start)
    endpoints <- runTestBg frozenTime tr $ Report.endpointStats False ev start frozenTime
    fmap (\e -> (e.service, e.environment, e.method, e.path, e.requests, e.averageMs)) endpoints `shouldBe` [(Just "api", Just "production", "POST", "/orders", 2, Just 500)]
    databases <- runTestBg frozenTime tr $ Report.databaseStats False ev start frozenTime
    fmap (\q -> (q.statement, q.operations, q.averageMs)) databases `shouldBe` [("SELECT secret FROM users", 1, 1250)]
    workloads <- runTestBg frozenTime tr $ Report.workloadStats False ev start frozenTime
    (sum (map (.events) workloads), find ((== "log") . (.kind)) workloads >>= (.averageMs)) `shouldBe` (4, Nothing)
    runTestBg frozenTime tr (Report.endpointStats False ev frozenTime (addUTCTime 1 frozenTime)) `shouldReturn` []

    -- Issue lifecycle states are counted before the highest-priority evidence is picked.
    iss <- createTestProject tr "report-issues"
    dbExec
      tr
      [sql|INSERT INTO apis.issues (project_id, issue_type, target_hash, title, service, created_at, critical, severity, affected_requests, acknowledged_at, archived_at)
        SELECT ?, 'runtime_exception', gen_random_uuid()::text, title, 'api', ?, critical, severity, impact, ack, archived
        FROM (VALUES ('critical from severity', false, 'critical', 1, NULL::timestamptz, NULL::timestamptz), ('critical from flag', true, 'info', 2, NULL, NULL), ('high-volume warning', false, 'warning', 9999, NULL, NULL), ('acknowledged', false, 'info', 1, ?::timestamptz, NULL), ('archived', false, 'info', 1, NULL, ?::timestamptz)) AS fixtures(title, critical, severity, impact, ack, archived)|]
      (iss, start, start, start)
    issues <- runTestBg frozenTime tr $ Report.issueStats iss start frozenTime
    (issues.newIssues, issues.openIssues, issues.criticalOpen, issues.acknowledged, issues.archivedInPeriod) `shouldBe` (5, 3, 2, 1, 1)
    map (\i -> (i.title, i.severity)) issues.priorities `shouldBe` [("critical from flag", "critical"), ("critical from severity", "critical"), ("high-volume warning", "warning")]

    -- Every infrastructure resource counts; unevaluated and paused monitors are told apart.
    let row = Containers.ContainerRow{containerName = "container", scope = Containers.ScopeContainer, podName = Nothing, namespace = Nothing, nodeName = Nothing, cluster = Nothing, provider = Nothing, region = Nothing, osType = Nothing, architecture = Nothing, image = Nothing, imageTag = Nothing, workload = Nothing, cpuCores = Just 1, cpuLimit = Nothing, cpuRequest = Nothing, memBytes = Just 1024, memLimit = Just 2048, memRequest = Nothing, load1 = Nothing, storagePct = Just 0.75, uptime = Nothing, restarts = Just 3, ready = Just 0, nodeStats = False}
        infrastructure = Report.infrastructureStats (addUTCTime (-900) frozenTime) frozenTime $ V.fromList $ replicate 501 row <> [row{Containers.scope = Containers.ScopeHost, Containers.ready = Nothing}, row{Containers.scope = Containers.ScopePod}]
        evaluated = def{Monitors.lastEvaluated = Just frozenTime, Monitors.currentStatus = Monitors.MSAlerting, Monitors.currentValue = 42}
        monitors = Report.monitorStats [evaluated, evaluated{Monitors.currentStatus = Monitors.MSWarning}, evaluated{Monitors.currentStatus = Monitors.MSNormal}, evaluated{Monitors.deactivatedAt = Just frozenTime}, evaluated{Monitors.lastEvaluated = Nothing}]
    (infrastructure.hosts, infrastructure.containers, infrastructure.pods, infrastructure.unready) `shouldBe` (1, 501, 1, 502)
    map (\r -> (r.cpuRatio, r.memoryRatio, r.storageRatio, r.restartCounter)) infrastructure.resources `shouldBe` replicate 10 (Nothing, Just 0.5, Just 0.75, Just 3)
    (monitors.alerting, monitors.warning, monitors.normal, monitors.paused, monitors.unevaluated) `shouldBe` (1, 1, 1, 1, 1)
    (find ((== "Not evaluated") . (.status)) monitors.observations >>= (.value)) `shouldBe` Nothing

  it "report links open the Log Explorer on exactly the service, endpoint and database rows they summarise" \tr ->
    for_ @[] [(Just "api\" & team", Just "prod\" & team", "api\" & team"), (Nothing, Nothing, ""), (Nothing, Just "production", "api.example.com"), (Just "api", Nothing, "")] \(service, environment, host) -> do
      pid <- createTestProject tr "report-links"
      project <- projectOf tr pid
      let start = addUTCTime (-60) frozenTime
          cfg = (defSqlQueryCfg pid frozenTime Nothing Nothing){targetsTimefusion = True}
      dbExec
        tr
        [sql|INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, kind, resource___service___name, resource___deployment___environment___name, attributes___http___request___method, attributes___url___path, attributes___server___address, attributes___db___query___text, duration, context, summary)
          SELECT gen_random_uuid(), ?, ?, ?, kind, ?, ?, 'GET', '/cart??item="a"&b=2', ?, statement, 1250000000::bigint, '{}'::jsonb, '{}'
          FROM (VALUES ('server', NULL), ('client', 'SELECT 1')) AS fixtures(kind, statement)|]
        (pid, start, start, service :: Maybe Text, environment :: Maybe Text, if host == fromMaybe "" service then Nothing else Just (host :: Text))
      base <- runTestBg frozenTime tr $ Reports.collectSystemReport pid start frozenTime
      let snapshot =
            base
              { Report.services = Report.compareServices [Report.ServiceStats service environment 2 0 0 1 0 2 (Just 1250)] []
              , Report.performance = Report.Available [Report.EndpointComparison (Report.EndpointStats service environment host "GET" "/cart?item=\"a\"&b=2" (Just 1250) 1) Nothing]
              , Report.databases = Report.Available [Report.DatabaseStats service "SELECT 1" 1250 1]
              , Report.topPatterns = Report.Available []
              , Report.workloads = Report.Available []
              }
      (_, _, html) <- runTestBg frozenTime tr $ Reports.renderSystemEmail Projects.RTWeekly "/reports/test" project "Ada" True snapshot
      let links = [url | suffix <- drop 1 $ T.splitOn "href=\"" html, let url = T.takeWhile (/= '"') suffix, all (`T.isInfixOf` url) ["/log_explorer?", "query="]]
      length links `shouldBe` 3
      for_ (zip [2, 1, 1] links) \(expectedCount, url) -> do
        let params = parseQueryText $ encodeUtf8 $ T.replace "&amp;" "&" $ snd $ T.breakOn "?" url
            param k = maybe (fail $ "report link omitted " <> toString k) pure $ join $ lookup k params
            explore q = do
              (from, to) <- (,) <$> param "from" <*> param "to"
              res <- call tr $ Log.logExplorerDataH pid def{Log.query = Just q, Log.from = Just from, Log.to = Just to}
              (_, components) <- either (fail . toString) pure $ parseQueryToComponents cfg q
              pure (res, components)
        query <- param "query"
        (events, components) <- explore query
        (events.error, events.queryResultCount) `shouldBe` (Nothing, expectedCount)
        components.whereClause `shouldSatisfy` maybe False (\w -> "resource___service___name" `T.isInfixOf` w && not ("service->" `T.isInfixOf` w))
        -- The retained `service.name` alias compiles to the same SQL and rows.
        (retained, retainedComponents) <- explore $ T.replace "resource.service.name" "service.name" query
        retained.error `shouldBe` Nothing
        V.toList retained.logsData `shouldMatchList` V.toList events.logsData
        retainedComponents.finalSqlQuery `shouldBe` components.finalSqlQuery

  it "report snapshots persist whole, the live preview is claimed once, and emails bound their details" \tr -> do
    project <- projectOf tr testPid
    emptySnapshot <- runTestBg frozenTime tr $ Reports.collectSystemReport testPid (addUTCTime (-(7 * 86400)) frozenTime) frozenTime
    let stats n = Report.ServiceStats (Just $ "service-" <> show n) (Just "production") 10 1 0 10 1 10 (Just 125)
        snapshot =
          emptySnapshot
            { Report.services = Report.compareServices (map stats [1 .. 30 :: Int]) []
            , Report.databases = Report.Available [Report.DatabaseStats (Just "api") (T.replicate 2000 "<script>private SQL &</script>") 1250 7]
            , Report.infrastructure = Report.Available $ Report.InfrastructureStats 3 100 2 (replicate 10 $ Report.InfrastructureResource (T.replicate 100 "resource-") Containers.ScopeContainer (Just "host") (Just "production") (Just "namespace") (Just 0.9) (Just 0.8) (Just 0.7) (Just False) (Just 7)) 10 (addUTCTime (-900) frozenTime) frozenTime
            , Report.monitors = Report.Available $ Report.MonitorStats 0 0 12 0 0 (replicate 12 $ Report.MonitorObservation "monitor-id" (T.replicate 50 "Monitor title ") "Alerting" (Just 850) (Just frozenTime))
            , Report.issues = Report.Available $ Report.IssueStats 10 20 10 5 5 (replicate 10 $ Report.IssueObservation "issue-id" (T.replicate 50 "Issue title ") (Just "api") "critical" "runtime_exception" 9999)
            , Report.performance = Report.Available $ replicate 30 $ Report.EndpointComparison (Report.EndpointStats (Just "api") (Just "production") "api.example.com" "GET" ("/" <> T.replicate 300 "a") (Just 500) 1000) Nothing
            , Report.workloads = Report.Available [Report.WorkloadStats kind 10000 (Just 100) | kind <- ["server", "client", "consumer", "producer", "internal", "unspecified", "log"]]
            , Report.topPatterns = Report.Available $ replicate 5 (T.replicate 100 "pattern ", 100000, "body")
            , Report.trends = Just $ Report.Available [Report.TrendPoint 1735689600 100 7]
            }
        preview = runTestBg frozenTime tr . Report.getPreview
        livePage = renderT <$> call tr (Reports.reportsLiveGetH testPid (Just "true"))
    AET.parseEither (AE.withObject "report" (AE..: "systemSnapshot")) (Reports.buildReportJson' snapshot) `shouldBe` Right snapshot
    let withoutTrends = case AE.toJSON snapshot of AE.Object fields -> AE.Object $ KM.delete "trends" fields; value -> value
    AE.fromJSON withoutTrends `shouldBe` AE.Success snapshot{Report.trends = Nothing}
    -- Both replicas may request the same preview; exactly one owns generation.
    (firstClaim, secondClaim) <- concurrently (runTestBg frozenTime tr $ Report.claimPreview testPid) (runTestBg frozenTime tr $ Report.claimPreview testPid)
    token <- case catMaybes [firstClaim, secondClaim] of
      [owner] -> pure owner
      _ -> fail "preview generation was not claimed exactly once"
    livePage >>= (`shouldContainAll` ["Preparing your system report", "every 2s", "live-report-preview"])
    runTestBg frozenTime tr $ Report.finishPreview testPid token (Report.PreviewReady snapshot)
    preview testPid `shouldReturn` Just (Report.PreviewReady snapshot)
    preview (UUIDId $ UUID.fromWords 9 8 7 6) `shouldReturn` Nothing
    readyHtml <- livePage
    readyHtml `shouldContainAll` ["live-report-preview", "srcdoc", "Infrastructure", "Monitors"]
    readyHtml `shouldNotSatisfy` T.isInfixOf "every 2s"
    dbExec tr [sql|UPDATE apis.report_previews SET expires_at = now() - interval '1 second' WHERE project_id = ?|] (Only testPid)
    retryToken <- maybe (fail "expired preview could not be retried") pure =<< runTestBg frozenTime tr (Report.claimPreview testPid)
    -- An old worker finishing after a retry must not overwrite the new lease.
    runTestBg frozenTime tr $ Report.finishPreview testPid token (Report.PreviewReady snapshot)
    preview testPid `shouldReturn` Just Report.PreviewBuilding
    runTestBg frozenTime tr $ Report.finishPreview testPid retryToken Report.PreviewFailed
    livePage >>= (`shouldContainAll` ["The report could not be prepared", "every 60s", "View saved reports"])

    let render full snap = (\(_, _, h) -> h) <$> runTestBg frozenTime tr (Reports.renderSystemEmail Projects.RTWeekly "/reports/test" project "Ada" full snap)
    email <- render False snapshot
    full <- render True snapshot
    charts <- forM (drop 1 $ T.splitOn "widgetZ=" email) \suffix -> do
      compressed <- either (fail . toString) pure $ B64URL.decodeBase64Untyped $ urlDecode True $ encodeUtf8 $ T.takeWhile (/= '&') suffix
      either fail pure $ AE.eitherDecode @Widget.Widget $ GZip.decompress $ fromStrict compressed
    map (.query) charts `shouldBe` replicate 4 Nothing
    map (fmap (.source) . (.dataset)) charts `shouldBe` concatMap (replicate 2 . Just) [[aesonQQ|[["Time","Events"],[1735689600000,100]]|], [aesonQQ|[["Time","Errors"],[1735689600000,7]]|]]
    email `shouldContainAll` ["appearance=light", "appearance=dark", "render=2", "logo-white-ink.png", "email-image-dark", "View 25 more service comparisons", "&lt;script&gt;", "View 8 more monitors", "View 25 more endpoints"]
    email `shouldNotSatisfy` T.isInfixOf "<script>"
    BS.length (encodeUtf8 email) `shouldSatisfy` (< 80000)
    full `shouldNotSatisfy` T.isInfixOf "View 25 more service comparisons"
    (T.count "Avg request" full, T.count "Avg request" email) `shouldBe` (30, 5)
    render False snapshot{Report.infrastructure = Report.Unavailable, Report.trends = Just Report.Unavailable}
      >>= (`shouldContainAll` ["Infrastructure metrics could not be loaded", "Activity trends unavailable", "Services"])
    let quiet = emptySnapshot{Report.services = [], Report.infrastructure = Report.Available $ Report.InfrastructureStats 0 0 0 [] 0 (addUTCTime (-900) frozenTime) frozenTime, Report.monitors = Report.Available $ Report.MonitorStats 0 0 0 0 0 [], Report.issues = Report.Available $ Report.IssueStats 0 0 0 0 0 [], Report.performance = Report.Available [], Report.databases = Report.Available [], Report.workloads = Report.Available [], Report.topPatterns = Report.Available [], Report.ingestionCapped = Just False}
    render False quiet >>= (`shouldContainAll` ["No telemetry events", "No HTTP server spans", "No infrastructure metrics observed", "No monitors configured"])
    let historical = Email.HistoricalEvidence Email.HistoricalReportEvidence{totalEvents = 0, totalErrors = 0, anomalies = V.empty, performance = V.empty, slowQueries = V.replicate 15 ("SELECT historical_slow_query", 900000000, 10)}
        renderHistorical f = (\(_, _, h) -> h) <$> runTestBg frozenTime tr (Reports.renderWeeklyEmail Projects.RTWeekly "/reports/test" project "Ada" frozenTime frozenTime f historical)
    historicalEmail <- renderHistorical False
    historicalFull <- renderHistorical True
    (T.count "SELECT historical_slow_query" historicalEmail, T.count "SELECT historical_slow_query" historicalFull) `shouldBe` (4, 15)
    historicalEmail `shouldContainAll` ["View all slow queries", "View all endpoints"]
    -- System evidence and duration units render as text, never image-only metrics.
    let sample = uncurry Email.renderEmail $ Email.sampleWeeklyReport Nothing Nothing
    sample `shouldContainAll` ["Services", "Infrastructure", "Issues to review", "Monitors", "checkout-api", "Not ready", "1250.0 ms", "3,400", "CPU / capacity", "No previous data"]
    traverse parseQueryToAST Reports.errorsWidget.query `shouldSatisfy` isRight
    for_ @[] ["data:image/svg", "td:nth-child(n+2)"] \absent -> sample `shouldNotSatisfy` T.isInfixOf absent

  it "error alerts: orphan sweep, settings toggle, acknowledgement window, aged subscription, one fingerprint, endpoint gate" \tr -> do
    seedSlackChannel tr
    saveAlertToggle tr (\form v -> form{ProjectPages.errorAlerts = v}) True
    apiKey <- createTestAPIKey tr testPid "notif-key"
    -- 3h back, so processOneMinuteErrors's 15-minute inline notify window misses it.
    ingestTraceWithException tr apiKey "GET /a" "TypeError" "x is undefined" "TypeError: x is undefined\n    at f (/a.js:1:1)" (addUTCTime (-3 * 3600) frozenTime)
    drainExtractionWorker tr
    void $ runAllBackgroundJobs frozenTime tr.trATCtx
    let clearNotified = dbExec tr [sql|UPDATE apis.error_patterns SET last_notified_at = NULL WHERE project_id = ?|] (Only testPid)
        sweep = clearNotified >> fst <$> captureNotifs tr (BackgroundJobs.sweepErrorSubscriptions testPid)
        delivered = sort . map (\case EmailNotification e -> e.receiver; SlackNotification{} -> "slack"; n -> show n) <$> sweep
        -- The incident is already open, so Slack gets a thread reply and its root edit, in one run.
        alerted = delivered `shouldReturn` ["hello@monoscope.tech", "slack", "slack", "test@monoscope.tech"]
    -- The orphan state seen in prod (last_notified_at NULL) is swept, and stamped so the next tick doesn't double-fire.
    alerted
    dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM apis.error_patterns WHERE project_id = ? AND last_notified_at IS NOT NULL|] (Only testPid) `shouldReturn` 1
    saveAlertToggle tr (\form v -> form{ProjectPages.errorAlerts = v}) False
    sweep `shouldReturn` []
    saveAlertToggle tr (\form v -> form{ProjectPages.errorAlerts = v}) True
    -- A re-sweep at the same instant is the same incident event, so nothing new is sent.
    -- (A one-delivery-per-tick worker used to leak the first alert's root edit into this sweep.)
    delivered `shouldReturn` []

    -- An acknowledgement silences the issue for exactly its window, then hands it back with a trail.
    issueId <- dbOne @_ @Issues.IssueId tr [sql|SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'runtime_exception'|] (Only testPid)
    let uid = (Servant.getResponse tr.trSessAndHeader).user.id
    runTestBg frozenTime tr $ void $ Issues.setAckState testPid [issueId] $ Just Issues.AckSet{at = frozenTime, by = Just uid, window = Issues.AckFor 60}
    sweep `shouldReturn` []
    advanceMinutes tr 61
    alerted
    runTestBgNoReset tr BackgroundJobs.expireLapsedAcks
    dbQuery tr [sql|SELECT i.acknowledged_at IS NULL, EXISTS (SELECT 1 FROM apis.issue_activity_log a WHERE a.issue_id = i.id AND a.event = 'ack_expired') FROM apis.issues i WHERE i.id = ?|] (Only issueId)
      `shouldReturn` [(True, True)]

    -- "Notify every N" died once its pattern was a day old: project selection bounded every row by created_at.
    -- created_at is compared against SQL now(), the cadence against the app clock.
    let later = addUTCTime 3660 frozenTime
    dbExec tr [sql|UPDATE apis.error_patterns SET created_at = now() - interval '3 days', subscribed = TRUE, notify_every_minutes = 30, last_notified_at = ? WHERE project_id = ?|] (addUTCTime (-3600) later, testPid)
    (subscribed, _) <- runTestBackgroundWithNotifications later tr.trLogger tr.trATCtx $ BackgroundJobs.runNotificationSweep later
    -- The reminder's thread reply and its root edit.
    [sd.threadTs | SlackNotification sd <- subscribed] `shouldSatisfy` \ts -> length ts == 2 && length (filter isJust ts) == 1

    -- Unsubscribed reminders for a recurring error decay: daily in its first week, weekly after, never past 30 days.
    hash <- dbOne @_ @Text tr [sql|SELECT target_hash FROM apis.issues WHERE id = ?|] (Only issueId)
    let remindAt t (ageDays, sinceHours) = do
          dbExec tr [sql|UPDATE apis.error_patterns SET subscribed = FALSE, state = 'ongoing', created_at = ?, last_notified_at = ? WHERE project_id = ? AND hash = ?|] (addUTCTime (-(ageDays * 86400)) t, addUTCTime (-(sinceHours * 3600)) t, testPid, hash)
          dbExec tr [sql|INSERT INTO apis.error_hourly_stats (project_id, error_id, hour_bucket, event_count, user_count) SELECT project_id, id, date_trunc('hour', ?::timestamptz), 1, 1 FROM apis.error_patterns WHERE project_id = ? AND hash = ? ON CONFLICT DO NOTHING|] (t, testPid, hash)
          void $ runTestBackgroundWithNotifications t tr.trLogger tr.trATCtx (BackgroundJobs.notifyErrorSubscriptions testPid (V.singleton hash))
          (== Just t) <$> dbOne @_ @(Maybe UTCTime) tr [sql|SELECT last_notified_at FROM apis.error_patterns WHERE project_id = ? AND hash = ?|] (testPid, hash)
    zipWithM remindAt [addUTCTime (fromIntegral h * 3600) later | h <- [2 :: Int ..]] [(3, 25), (10, 25), (10, 169), (31, 169)] `shouldReturn` [True, False, True, False]

    -- One Warp transport error from different stacks groups to one pattern.
    let warp = "Warp: Client closed connection prematurely"
    ingestTraceWithException tr apiKey "GET /route-a" "InvalidRequest" warp "at Warp.run (Server.hs:42:1)\nat handle (/app/x.hs:7:3)" (addUTCTime (-120) frozenTime)
    ingestTraceWithException tr apiKey "POST /route-b" "InvalidRequest" warp "at Warp.run (Server.hs:42:1)\nat different_caller (/app/y.hs:9:5)" (addUTCTime (-60) frozenTime)
    drainExtractionWorker tr
    void $ runAllBackgroundJobs frozenTime tr.trATCtx
    patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns testPid Nothing 50 0
    length (filter ((== "InvalidRequest") . (.errorType)) patterns) `shouldBe` 1

    -- With new-endpoint alerts off the issue was once stamped notified anyway, so re-enabling recovered nothing.
    let endpointAlerts = saveAlertToggle tr \form v -> form{ProjectPages.endpointAlerts = v}
        row = Email.EndpointAlertRow{label = "GET /gated", host = Nothing, service = Nothing, environment = Nothing}
    endpointAlerts False
    iid <- dbOne @_ @Issues.IssueId tr [sql|INSERT INTO apis.issues (project_id, issue_type, title, target_hash, endpoint_hash) VALUES (?, 'api_change', 'GET /gated', 'endpoint-gate-hash', 'endpoint-gate-hash') RETURNING id|] (Only testPid)
    let send = fst <$> captureNotifs tr (BackgroundJobs.sendNewEndpointAlerts testPid "endpoint-gate-hash" [(iid, row)])
        notified = dbOne @_ @(Maybe UTCTime) tr [sql|SELECT last_notified_at FROM apis.issues WHERE id = ?|] (Only iid)
    send `shouldReturn` []
    notified `shouldReturn` Nothing
    endpointAlerts True
    length <$> send `shouldReturn` 3
    isJust <$> notified `shouldReturn` True

  it "notification budget: 20 per hour, held for the hour, reset by the next, overflow digested on the test clock" \tr -> do
    let consume = runTestBgNoReset tr $ Time.currentTime >>= BackgroundJobs.consumeNotificationToken testPid
        digestRow title at = UUIDV4.nextRandom >>= \i -> dbExec tr [sql|INSERT INTO apis.notification_digest_queue (id, project_id, reason, title, created_at) VALUES (?, ?, 'rate_limit', ?, ?)|] (i, testPid, title :: Text, at)
    runTestBg frozenTime tr pass
    dbExec tr [sql|UPDATE projects.projects SET error_alerts = true WHERE id = ?|] (Only testPid)
    replicateM 22 consume `shouldReturn` replicate 20 True <> [False, False]
    replicateM_ 4 $ digestRow "t" frozenTime
    advanceMinutes tr 30
    consume `shouldReturn` False
    advanceMinutes tr 31
    consume `shouldReturn` True
    -- The 24h window only selects projects: once selected, every queued row flushes, stamped with
    -- the test clock rather than the wall clock.
    let now' = addUTCTime (61 * 60) frozenTime
    digestRow "too-old" (addUTCTime (-25 * 3600) now')
    runTestBgNoReset tr $ BackgroundJobs.runNotificationDigest now'
    dbQuery tr [sql|SELECT sent_at FROM apis.notification_digest_queue WHERE project_id = ?|] (Only testPid) `shouldReturn` replicate 5 (Only (Just now'))

  -- Share links keep 30d + 48h grace; replay data (no BYO bucket) keeps 30d. Both cut on the test clock.
  it "retention expires share events after 32 days and replay data after 30" \tr -> do
    share <- UUIDV4.nextRandom
    session <- UUIDV4.nextRandom
    runTestBg frozenTime tr pass
    eventId <- UUIDV4.nextRandom
    dbExec tr [sql|INSERT INTO apis.share_events (id, project_id, created_at, event_id, event_type, event_created_at) VALUES (?, ?, ?, ?, 'log', ?)|] (share, testPid, frozenTime, eventId, frozenTime)
    dbExec tr [sql|INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at) VALUES (?, ?, ?)|] (session, testPid, frozenTime)
    let expireAfter days = do
          advanceDays tr days
          for_ @[] [BackgroundJobs.ExpireShareEvents, BackgroundJobs.ExpireReplayData] $ runTestBgNoReset tr . BackgroundJobs.processBackgroundJob tr.trATCtx
          (,) <$> dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM apis.share_events WHERE id = ?|] (Only share) <*> dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM projects.replay_sessions WHERE session_id = ?|] (Only session)
    expireAfter 29 `shouldReturn` (1, 1)
    expireAfter 2 `shouldReturn` (1, 0)
    expireAfter 2 `shouldReturn` (0, 0)

  it "auto-provision creates a dashboard per detected namespace once, adopts a manual one, and respects deletion" \tr -> do
    let seedMetric name = dbExec tr [sql|INSERT INTO otel_metrics_meta (project_id, metric_name, metric_type, first_seen_at, last_seen_at, first_timestamp, last_timestamp) VALUES (?, ?, 'SUM', now(), now(), now(), now()) ON CONFLICT DO NOTHING|] (testPid, name :: Text)
        provision = void $ runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.DashboardsAutoProvision frozenTime)
        -- Only this spec's templates, so fixtures like _overview.yaml can't interfere.
        provisioned = map fromOnly <$> dbQuery @_ @(Only Text) tr [sql|SELECT base_template FROM projects.dashboards WHERE project_id = ? AND base_template IN ('postgresql.yaml', 'redis.yaml', 'mysql.yaml') ORDER BY base_template|] (Only testPid)
    seedMetric "mysql.threads"
    dbExec tr [sql|INSERT INTO projects.dashboards (project_id, created_by, base_template, title) VALUES (?, '00000000-0000-0000-0000-000000000001', 'mysql.yaml', 'My MySQL')|] (Only testPid)
    seedMetric "postgresql.backends"
    provision
    provisioned `shouldReturn` ["mysql.yaml", "postgresql.yaml"]
    seedMetric "redis.memory.used"
    provision >> provision
    provisioned `shouldReturn` ["mysql.yaml", "postgresql.yaml", "redis.yaml"]
    dbExec tr [sql|DELETE FROM projects.dashboards WHERE project_id = ? AND base_template = 'postgresql.yaml'|] (Only testPid)
    provision
    provisioned `shouldReturn` ["mysql.yaml", "redis.yaml"]

  -- DailyJob seeds every per-project job, ReportUsage included, so when it stops firing billing
  -- stops with it. Prod ran it 08-24 and 08-26, not 08-25 or 08-27.
  it "the daily job is scheduled despite yesterday's spilled hourly jobs, never twice, wakes the listener, and a disabled worker leaves the queue alone" \tr -> do
    let queueAt tag offset = dbExec tr [sql|INSERT INTO background_jobs (run_at, status, payload) VALUES (date_trunc('day', now()) + ?::interval, 'queued', jsonb_build_object('tag', ?))|] (offset :: Text, tag :: Text)
        dailyJobs = dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM background_jobs WHERE payload->>'tag' = 'DailyJob'|] ()
    -- HourlyJobs are seeded from now(), not the day, so a 17:43 run leaves rows up to 16:43
    -- tomorrow; the guard read those as proof today's DailyJob ran.
    queueAt "HourlyJob" "13 hours"
    queueAt "HourlyJob" "16 hours"
    BackgroundJobs.ensureDailyJobScheduled tr.trATCtx
    dailyJobs `shouldReturn` 1
    -- odd-jobs deletes the completed row, so without this the 30-minute backstop re-inserts every half hour.
    BackgroundJobs.ensureDailyJobScheduled tr.trATCtx
    dailyJobs `shouldReturn` 1
    withResource tr.trPool \listener -> do
      let channel = Only $ pgEventName "background_jobs"
      bracket_ (PGS.execute listener "LISTEN ?" channel) (PGS.execute listener "UNLISTEN ?" channel) do
        queueAt "DailyJob" "-1 hour"
        fmap PGN.notificationChannel <$> Timeout.timeout 5_000_000 (PGN.getNotification listener) `shouldReturn` Just "jobs_created_background_jobs"
    let disabled = tr.trATCtx{Config.config = tr.trATCtx.config{Config.enableBackgroundJobs = False, Config.enableDailyJobScheduling = False}}
    Timeout.timeout 1_000_000 (BackgroundJobs.jobsWorkerInit tr.trLogger disabled tr.trTracerProvider) `shouldReturn` Just ()
    dailyJobs `shouldReturn` 2

  it "dual-exec: the TimeFusion arm outwaits a deploy drain instead of dropping the UPDATE" \tr -> do
    -- Two drain refusals, then the write lands: what TimeFusion returns across a handoff.
    dbExec
      tr
      [sql|CREATE SEQUENCE tf_drain_calls; CREATE TABLE tf_drain_target (n int); INSERT INTO tf_drain_target VALUES (0);
           CREATE FUNCTION tf_drain_probe() RETURNS int LANGUAGE plpgsql AS $$ BEGIN
             IF nextval('tf_drain_calls') <= 2 THEN
               RAISE EXCEPTION 'TimeFusion is draining for deployment; retry on the replacement' USING ERRCODE = 'XX000';
             END IF;
             RETURN 1;
           END $$|]
      ()
    let ctx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.enableTimefusionWrites = True}}
    void $ runTestBg frozenTime tr $ BackgroundJobs.dualExecPgTfWithSql ctx [HI.sql|UPDATE tf_drain_target SET n = n|] [HI.sql|UPDATE tf_drain_target SET n = tf_drain_probe()|]
    dbOne @() @Int tr [sql|SELECT n FROM tf_drain_target|] () `shouldReturn` 1


-- Round-trips the real settings form so the toggle is written the way the UI writes it.
saveAlertToggle :: TestResources -> (ProjectPages.CreateProjectForm -> Maybe Text -> ProjectPages.CreateProjectForm) -> Bool -> IO ()
saveAlertToggle tr set enabled = do
  ProjectPages.CreateProject (PageCtx _ (_, _, _, _, _, form, _, _)) <- call tr $ ProjectPages.projectSettingsGetH testPid
  void $ testServant tr $ ProjectPages.createProjectPostH testPid $ set form $ bool Nothing (Just "on") enabled
