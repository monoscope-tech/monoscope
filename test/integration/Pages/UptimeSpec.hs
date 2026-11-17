module Pages.UptimeSpec (spec) where

import BackgroundJobs qualified as Jobs
import Data.Vector qualified as V
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Apis.Uptime qualified as UptimeM
import Models.Projects.DashboardTemplates qualified as DashboardTemplates
import Pages.Charts.Charts qualified as Charts
import Pkg.Components.Widget qualified as Widget
import Pages.Uptime qualified as Uptime
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Pkg.TestUtils


spec :: Spec
spec = around withTestResources $ describe "Uptime and status pages" do
  it "draws 90 days of each check, publishes a status page, gates probes by region, and keeps history across edits" \tr -> do
    pid <- createTestProject tr "Status"
    void $ call tr $ Uptime.uptimeCheckPostH pid (Uptime.UptimeForm "Checkout API" "https://shop.example.com/health" (Just 60) (Just 200) [])
    void $ call tr $ Uptime.cronMonitorPostH pid (Uptime.CronForm "nightly-billing" "Nightly billing" 86400 3600)
    [shop] <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId pid PromCfg.CKUptime)
    [cron] <- runQueryEffect tr $ Monitors.cronMonitorsByProject pid
    -- 15-minute probes for 90 days with a three-hour outage three days ago; daily runs, one of which failed.
    dbExec
      tr
      [sql| INSERT INTO apis.check_results (project_id, check_id, kind, region, checked_at, ok, status_code, duration_ms, message)
            SELECT ?, ?, 'uptime', 'eu-west', t, NOT down, CASE WHEN down THEN 503 ELSE 200 END, 120 + (extract(epoch FROM t)::bigint / 900 % 9) * 40,
                   CASE WHEN down THEN 'status 503, expected 200' ELSE 'ok' END
            FROM generate_series(?::timestamptz - interval '89 days', ?::timestamptz - interval '15 minutes', interval '15 minutes') t,
                 LATERAL (SELECT t >= ?::timestamptz - interval '3 days' AND t < ?::timestamptz - interval '3 days' + interval '3 hours' AS down) d |]
      (pid, shop.id, frozenTime, frozenTime, frozenTime, frozenTime)
    dbExec
      tr
      [sql| INSERT INTO apis.check_results (project_id, check_id, kind, region, checked_at, ok, message)
            SELECT ?, ?, 'cron', 'global', t, t <> ?::timestamptz - interval '5 days', CASE WHEN t = ?::timestamptz - interval '5 days' THEN 'error' ELSE 'ok' END
            FROM generate_series(?::timestamptz - interval '60 days', ?::timestamptz - interval '1 day', interval '1 day') t |]
      (pid, cron.id, frozenTime, frozenTime, frozenTime, frozenTime)
    -- A probe from this node's region records its result and the check reports up.
    runTestBg frozenTime tr $ Jobs.applyUptimeResult shop "eu-west" frozenTime 180 (Right 200)
    Just probed <- runQueryEffect tr $ PromCfg.getConfigByProject pid shop.id

    overview <- renderT <$> call tr (Uptime.uptimeChecksGetH pid)
    overview `shouldContainAll` ["All systems operational", "Checkout API", "Uptime, 30 days", "99.86%", "99.86% uptime over 90 days; failures on Dec 29 (12 failed)"]
    dashboard <- renderT <$> call tr (Uptime.uptimeCheckGetH pid probed.id (Just "7D"))
    dashboard `shouldContainAll` ["Operational", "every 1 min", "expects 200", "status 503, expected 200", "Resolved", "3h", "France · eu-west"]
    cronPage <- renderT <$> call tr (Uptime.cronMonitorGetH pid cron.id)
    cronPage `shouldContainAll` ["Nightly billing", "nightly-billing", "Check-ins", "cron.checkin", "/v1/traces", "error"]

    -- Public pages are served without login; an internal one reads as missing there.
    Right _ <- call tr $ Uptime.statusPagePostH pid (Uptime.StatusPageForm "Acme status" "acme" (Just "Live health of Acme") UptimeM.VPublic [shop.id.unUUIDId, cron.id])
    (void <$> call tr (Uptime.statusPagePostH pid (Uptime.StatusPageForm "Again" "acme" Nothing UptimeM.VPublic []))) `shouldReturn` Left (Uptime.TakenSlug "acme")
    (void <$> call tr (Uptime.statusPagePostH pid (Uptime.StatusPageForm "Ops" "Not Valid" Nothing UptimeM.VPublic []))) `shouldReturn` Left Uptime.InvalidPageSlug
    Right _ <- call tr $ Uptime.statusPagePostH pid (Uptime.StatusPageForm "Ops only" "acme-ops" Nothing UptimeM.VInternal [shop.id.unUUIDId])
    public <- renderT <$> toBaseServantResponse tr (Uptime.publicStatusPageGetH "acme")
    public `shouldContainAll` ["Acme status", "Live health of Acme", "Checkout API", "Nightly billing", "Past incidents", "HTTP 503", "Powered by"]
    -- A public reader sees the status code, never the stored probe message.
    ("expected 200" `T.isInfixOf` public) `shouldBe` False
    renderT <$> toBaseServantResponse tr (Uptime.publicStatusPageGetH "acme-ops") >>= (`shouldContainAll` ["Status page not found"])
    -- The built-in board shows every check and cron monitor, whatever the pages pick.
    renderT <$> call tr (Uptime.statusAllGetH pid) >>= (`shouldContainAll` ["All monitors", "Checkout API", "Nightly billing"])
    pages <- runQueryEffect tr $ UptimeM.statusPagesByProject pid
    map (.slug) pages `shouldBe` ["acme", "acme-ops"]
    -- Editing keeps the page's URL unique, and changes what it shows.
    let acme = find ((== "acme") . (.slug)) pages
    acmeId <- maybe (fail "no acme page") (pure . (.id)) acme
    (void <$> call tr (Uptime.statusPageEditH pid acmeId (Uptime.StatusPageForm "Acme" "acme-ops" Nothing UptimeM.VPublic []))) `shouldReturn` Left (Uptime.TakenSlug "acme-ops")
    Right _ <- call tr $ Uptime.statusPageEditH pid acmeId (Uptime.StatusPageForm "Acme API" "acme" Nothing UptimeM.VPublic [shop.id.unUUIDId])
    renderT <$> toBaseServantResponse tr (Uptime.publicStatusPageGetH "acme") >>= \h -> (h `shouldContainAll` ["Acme API", "Checkout API", "every 120s"]) >> (("Nightly billing" `T.isInfixOf` h) `shouldBe` False)
    renderT <$> call tr (Uptime.statusPagesGetH pid) >>= (`shouldContainAll` ["Acme status", "/status/acme", "Ops only", "Internal"])

    -- Edits keep the history; unknown regions are dropped, and a node only probes a check that runs from its region.
    (void <$> call tr (Uptime.uptimeCheckEditH pid shop.id (Uptime.UptimeForm "Checkout" "https://shop.example.com/health" (Just 300) (Just 204) ["us-east"]))) `shouldReturn` Left (Uptime.UnservedRegion "us-east")
    Right _ <- call tr $ Uptime.uptimeCheckEditH pid shop.id (Uptime.UptimeForm "Checkout" "https://shop.example.com/health" (Just 300) (Just 204) ["eu-west", "nowhere"])
    Just edited <- runQueryEffect tr $ PromCfg.getConfigByProject pid shop.id
    (edited.name, edited.scrapeIntervalSeconds, edited.expectedStatus, edited.regions) `shouldBe` ("Checkout", 300, Just 204, ["eu-west"])
    -- A node leases only checks its region serves; one left with only another region's worker is not its to probe.
    dbExec tr [sql| UPDATE apis.prometheus_scrape_configs SET regions = '{us-east}', last_scraped_at = NULL WHERE id = ? |] (Only shop.id)
    let claimed region = map (.id) <$> runQueryEffect tr (PromCfg.claimDueConfigs region 50)
    claimed "eu-west" `shouldReturn` []
    claimed "us-east" `shouldReturn` [shop.id]
    renderT <$> call tr (Uptime.uptimeCheckGetH pid shop.id (Just "7D")) >>= (`shouldContainAll` ["Virginia · us-east · no worker"])

  it "probes are uptime.probe spans: the embedded dashboard reads daily bars, percentiles, regions and the map from them" \tr -> do
    pid <- createTestProject tr "Probes"
    void $ call tr $ Uptime.uptimeCheckPostH pid (Uptime.UptimeForm "Checkout API" "https://shop.example.com/health" (Just 60) (Just 200) [])
    [shop] <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId pid PromCfg.CKUptime)
    let at h = addUTCTime (h * 3600) frozenTime
        probe h region ms outcome = runTestBg (at h) tr $ Jobs.emitProbeSpan shop region ("0af7651916cd43dd8448eb211c80319" <> show (round h `mod` 10 :: Int), "b7ad6b716920333" <> show (round h `mod` 10 :: Int)) (at h) (addUTCTime (ms / 1000) (at h)) outcome
    probe (-50) "eu-west" 120 (Right 200)
    probe (-49) "eu-west" 140 (Right 200)
    probe (-26) "eu-west" 900 (Right 503)
    probe (-25) "us-east" 300 (Right 200)
    probe (-1) "eu-west" 90 (Right 200)
    probe (-2) "us-east" 5000 (Left "connection refused")
    -- Every view the page embeds runs over the spans, with the check filled in.
    let views = concatMap (\t -> DashboardTemplates.templateTab "uptime.yaml" t [("check", shop.url), ("region", "")]) ["Overview", "Response time", "Regions", "Errors"]
        flat w = maybe [w] (concatMap flat) w.children
        charts = [w | w <- concatMap flat views, w.wType `notElem` [Widget.WTLogs, Widget.WTTable, Widget.WTGroup]]
        window = (Just $ toText $ iso8601Show $ at (-72), Just $ toText $ iso8601Show $ at 1)
        run w = let (q, dt) = Widget.chartQuery w in runQueryEffect tr $ Charts.queryMetrics Nothing (Just dt) (Just pid) q Nothing Nothing (fst window) (snd window) Nothing Nothing []
    length charts `shouldSatisfy` (> 10)
    results <- forM charts \w -> (w,) <$> run w
    [(w.title, md.error) | (w, md) <- results, isJust md.error] `shouldBe` []
    let titled t = maybe (fail $ toString t <> " missing") (pure . snd) $ find ((== Just t) . (.title) . fst) results
    bars <- titled "Daily uptime"
    -- One series per status a day: the 503 and the refused connection are the two ERROR probes.
    let column h = maybe 0 (\i -> sum [fromMaybe 0 (join $ r V.!? (i + 1)) | r <- V.toList bars.dataset]) $ V.elemIndex h (V.drop 1 bars.headers)
    (column "OK", column "ERROR") `shouldBe` (4, 2)
    located <- titled "Probes by location"
    sort (mapMaybe (V.!? 0) $ V.toList located.dataText) `shouldBe` ["FR", "US"]
    -- The cumulative table's SQL runs over the same spans, one row per checked URL.
    tableSql <- maybe (fail "no Checks table") pure $ listToMaybe [q | w <- concatMap flat (DashboardTemplates.templateTab "uptime.yaml" "All checks" [("region", "")]), w.title == Just "Checks", Just q <- [w.sql]]
    let filled = T.replace "{{query_ast_filters}}" ("project_id = '" <> pid.toText <> "' AND name = 'uptime.probe'") tableSql
    dbQuery @_ @(Text, Int, Double, Maybe Double) tr (fromString $ toString filled) () >>= (`shouldBe` [(shop.url, 6, 66.667, True)]) . map (\(u, n, up, p) -> (u, n, fromIntegral (round (up * 1000) :: Int) / 1000 :: Double, isJust p))
    page <- renderT <$> call tr (Uptime.uptimeCheckGetH pid shop.id (Just "7D"))
    page `shouldContainAll` ["Open as dashboard", "Daily uptime", "Probes by location", "Failure reasons", "/uptime/dashboard?var-check=https"]
    ("{{var-check}}" `T.isInfixOf` page) `shouldBe` False
