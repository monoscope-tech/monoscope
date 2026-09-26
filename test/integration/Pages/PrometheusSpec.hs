module Pages.PrometheusSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid (renderText)
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as MonitorsM
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Pages.Monitors qualified as Monitors
import Pages.Settings (PrometheusForm (..), PrometheusMut (..), prometheusPostH)
import Pkg.TestUtils
import Relude
import Test.Hspec


-- A representative /metrics body: a counter family (two series), a gauge, and a
-- non-finite sample that must be dropped.
promBody :: LByteString
promBody =
  "# HELP http_requests_total Total requests\n\
  \# TYPE http_requests_total counter\n\
  \http_requests_total{method=\"get\",code=\"200\"} 42\n\
  \http_requests_total{method=\"post\",code=\"500\"} 7\n\
  \# TYPE temperature_celsius gauge\n\
  \temperature_celsius 21.5\n\
  \broken_metric +Inf\n"


spec :: Spec
spec = around withTestResources do
  describe "Prometheus scrape configs" do
    it "ingests a scraped exposition body as metrics, dropping non-finite samples" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "api-gw" "http://svc:9090/metrics" 60 Nothing (AE.object ["env" AE..= ("prod" :: Text)])
      (cfg : _) <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKPrometheus)
      n <- runTestBg frozenTime tr $ PromCfg.ingestScrapedBody cfg frozenTime promBody
      n `shouldBe` 3 -- 2 counter series + 1 gauge; +Inf dropped
      rows <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT metric_name, metric_type, aggregation_temporality FROM otel_metrics WHERE project_id = ? ORDER BY metric_name |]
            (Only testPid.toText)
          :: IO (V.Vector (Text, Text, Maybe Text))
      V.length rows `shouldBe` 3
      sort (V.toList rows) `shouldBe` [("http_requests_total", "SUM", Just "CUMULATIVE"), ("http_requests_total", "SUM", Just "CUMULATIVE"), ("temperature_celsius", "GAUGE", Nothing)]

    it "uptime checks: a public URL is added, two failures open a downtime issue, a success resolves it" \tr -> do
      (_, rejected) <- testServant tr $ Monitors.uptimeCheckPostH testPid (Monitors.UptimeForm "internal" "http://10.0.0.1/health" (Just 60) (Just 200))
      renderText rejected `shouldSatisfy` (not . ("10.0.0.1" `TL.isInfixOf`))
      (_, added) <- testServant tr $ Monitors.uptimeCheckPostH testPid (Monitors.UptimeForm "Shop" "https://shop.example.com/health" (Just 60) (Just 200))
      renderText added `shouldSatisfy` ("https://shop.example.com/health" `TL.isInfixOf`)
      (c : _) <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKUptime)
      let probe res = runTestBg frozenTime tr $ BackgroundJobs.applyUptimeResult c frozenTime 120 res
          issues = runQueryEffect tr (Issues.selectIssueByHash testPid (Issues.uptimeTargetHash c.id.toText) Issues.AnyIssue)
      probe (Right 503)
      (isNothing <$> issues) `shouldReturn` True -- one failure is a blip
      probe (Left "connection refused")
      opened <- issues >>= maybe (fail "no downtime issue after two failures") pure
      (opened.title, isNothing opened.archivedAt) `shouldBe` ("Downtime detected for https://shop.example.com/health", True)
      probe (Right 200)
      resolved <- issues >>= maybe (fail "issue vanished") pure
      isJust resolved.archivedAt `shouldBe` True
      runTestBg frozenTime tr (Issues.selectLatestStateEvent resolved.id) `shouldReturn` Just Issues.IEResolved

    it "an uptime probe through the worker records a refused connection as a failure" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertUptimeCheck testPid "refused" "http://127.0.0.1:1/health" 60 200
      (c : _) <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKUptime)
      runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.PrometheusScrapeOne c.id)
      Just probed <- runQueryEffect tr (PromCfg.getConfig c.id)
      (probed.consecutiveFailures, fmap ("request failed" `T.isPrefixOf`) probed.lastStatus) `shouldBe` (1, Just True)

    it "cron monitors: a check-in keeps it healthy, a missed window and an error run open issues, a later check-in resolves" \tr -> do
      _ <- testServant tr $ Monitors.cronMonitorPostH testPid (Monitors.CronForm "nightly-billing" "Nightly billing" 3600 300)
      [m] <- runQueryEffect tr (MonitorsM.cronMonitorsByProject testPid)
      let at dt = addUTCTime dt frozenTime
          checkin status dt =
            withResource tr.trPool \conn ->
              void
                $ PGS.execute
                  conn
                  [sql| INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, kind, name, summary, attributes)
                        VALUES (gen_random_uuid(), ?, ?, ?, 'internal', 'cron.checkin', ARRAY['cron.checkin'], jsonb_build_object('monitor', jsonb_build_object('slug', 'nightly-billing', 'status', ?::text))) |]
                  (testPid, at dt, at dt, status :: Text)
          evaluate dt = do
            fresh <- runQueryEffect tr (MonitorsM.cronMonitorsByProject testPid) >>= maybe (fail "monitor gone") pure . listToMaybe
            runTestBg (at dt) tr $ BackgroundJobs.evaluateCronMonitor (at dt) fresh
          issue = runQueryEffect tr (Issues.selectIssueByHash testPid (Issues.cronTargetHash (UUID.toText m.id)) Issues.AnyIssue)
      checkin "ok" 0
      evaluate 60
      (isNothing <$> issue) `shouldReturn` True
      evaluate (3600 + 300 + 120) -- past interval + grace with no newer check-in
      missed <- issue >>= maybe (fail "no missed issue") pure
      (missed.title, isNothing missed.archivedAt) `shouldBe` ("Cron missed: Nightly billing", True)
      checkin "ok" 4200
      evaluate 4260
      (fmap (isJust . (.archivedAt)) <$> issue) `shouldReturn` Just True
      checkin "error" 8000
      evaluate 8060
      failed <- runQueryEffect tr (Issues.selectIssueByHash testPid (Issues.cronTargetHash (UUID.toText m.id)) (Issues.OpenOfType Issues.Cron)) >>= maybe (fail "no failed issue") pure
      failed.title `shouldBe` "Cron failed: Nightly billing"

    it "claims each due target once and leases it (multi-node safe), skipping disabled" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "svc-a" "http://a/metrics" 3600 Nothing (AE.object [])
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "svc-b" "http://b/metrics" 3600 Nothing (AE.object [])
      claimed1 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed1 `shouldBe` 2 -- both due, claimed
      claimed2 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed2 `shouldBe` 0 -- leased within interval ⇒ a second node's claim gets nothing
      -- a disabled target is never claimed even though its interval has elapsed
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "svc-c" "http://c/metrics" 1 Nothing (AE.object [])
      (c : _) <- V.toList . V.filter ((== "svc-c") . (.name)) <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKPrometheus)
      void $ runQueryEffect tr $ PromCfg.setEnabled testPid c.id False
      claimed3 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed3 `shouldBe` 0

    -- A duplicate-name save must be caught by the DB UNIQUE(project_id, name) constraint
    -- (migration 0103) and turned into a friendly error — not a 500 (which is what a missed
    -- Hasql.isUniqueViolation classification would produce) and not a second row. Drive it
    -- through prometheusPostH so the handler's try+isUniqueViolation+toast wiring is pinned
    -- end-to-end: reaching the assertion proves the second save did not throw.
    it "prometheusPostH catches a duplicate-name save (no 500) and never creates a second row" \tr -> do
      let form = PrometheusForm{name = "dup", url = "http://example.com/metrics", scrapeInterval = Nothing, authHeader = Nothing, extraLabels = Nothing, clearAuth = Nothing}
      _ <- testServant tr $ prometheusPostH testPid form
      (_, PrometheusMut (_, cfgs)) <- testServant tr $ prometheusPostH testPid form
      V.length (V.filter ((== "dup") . (.name)) cfgs) `shouldBe` 1
