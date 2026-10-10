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
import Effectful.Error.Static (catchError)
import Lucid (renderText, toHtml)
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as MonitorsM
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Projects.Projects qualified as Projects
import Pages.Bots.BotTestHelpers (withHTTPResponses)
import Pages.Monitors qualified as Monitors
import Pages.Settings (PrometheusForm (..), prometheusPostH)
import Pages.Settings qualified as Settings
import Pkg.TestUtils
import Relude
import Servant (ServerError (..), getResponse)
import Servant.API (ResponseHeader (..), lookupResponseHeader)
import System.Types (RespHeaders)
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
    it "prometheusViewer_cannotChangeTargetsOrProbeAnEndpoint" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid PromCfg.CKPrometheus "Saved target" "https://metrics.example.com/metrics" 60 (Just "Bearer saved-secret") (AE.object []) Nothing
      [saved] <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKPrometheus)
      let uid = (getResponse tr.trSessAndHeader).user.id
          form = PrometheusForm "Replacement target" "https://metrics.example.com/replacement" Nothing (Just "Bearer replacement-secret") Nothing Nothing
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql| UPDATE projects.project_members SET permission = 'view' WHERE project_id = ? AND user_id = ? |] (testPid, uid)
      requests <- newIORef (0 :: Int)
      statuses <- forM
        [ void $ Settings.prometheusPostH testPid form
        , void $ Settings.prometheusUpdateH testPid saved.id form
        , void $ Settings.prometheusToggleH testPid saved.id
        , void $ Settings.prometheusDeleteH testPid saved.id
        , void $ Settings.prometheusTestH testPid form
        ]
        \mutation -> runAuthHandler tr $ withHTTPResponses (\_ _ -> modifyIORef' requests (+ 1) $> Just promBody) $ catchError @ServerError (mutation $> 200) (\_ err -> pure err.errHTTPCode)
      statuses `shouldBe` replicate 5 403
      readIORef requests `shouldReturn` 0
      [retained] <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKPrometheus)
      (retained.name, retained.url, retained.enabled, retained.authHeader) `shouldBe` (saved.name, saved.url, saved.enabled, saved.authHeader)
      (_, page) <- testServant tr $ Settings.prometheusGetH testPid
      let html = renderText $ toHtml page
      html `shouldSatisfy` TL.isInfixOf "Saved target"
      for_ ["Add target", "Edit Prometheus target", "hx-patch", "hx-delete", "saved-secret"] \private -> html `shouldNotSatisfy` TL.isInfixOf private

    it "ingests a scraped exposition body as metrics, dropping non-finite samples" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid PromCfg.CKPrometheus "api-gw" "http://svc:9090/metrics" 60 Nothing (AE.object ["env" AE..= ("prod" :: Text)]) Nothing
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
      -- A rejected submission swaps nothing, so the form keeps what was typed; an
      -- accepted one re-renders the form blank above the updated list.
      (rejectedH, rejected) <- testServant tr $ Monitors.uptimeCheckPostH testPid (Monitors.UptimeForm "internal" "http://10.0.0.1/health" (Just 60) (Just 200))
      reswapOf rejectedH `shouldBe` Just "none"
      renderText rejected `shouldSatisfy` (not . ("10.0.0.1" `TL.isInfixOf`))
      (addedH, added) <- testServant tr $ Monitors.uptimeCheckPostH testPid (Monitors.UptimeForm "Shop" "https://shop.example.com/health" (Just 60) (Just 200))
      reswapOf addedH `shouldBe` Nothing
      renderText added `shouldSatisfy` \h -> "https://shop.example.com/health" `TL.isInfixOf` h && "value name=\"url\"" `TL.isInfixOf` h
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
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid PromCfg.CKUptime "refused" "http://127.0.0.1:1/health" 60 Nothing (AE.object []) (Just 200)
      (c : _) <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKUptime)
      runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.PrometheusScrapeOne c.id)
      Just probed <- runQueryEffect tr (PromCfg.getConfig c.id)
      (probed.consecutiveFailures, fmap ("request failed" `T.isPrefixOf`) probed.lastStatus) `shouldBe` (1, Just True)

    it "cron monitors: a check-in keeps it healthy, a missed window and an error run open issues, a later check-in resolves" \tr -> do
      (_, added) <- testServant tr $ Monitors.cronMonitorPostH testPid (Monitors.CronForm "nightly-billing" "Nightly billing" 3600 300)
      renderText added `shouldSatisfy` TL.isInfixOf "value name=\"slug\""
      (dupH, _) <- testServant tr $ Monitors.cronMonitorPostH testPid (Monitors.CronForm "nightly-billing" "Again" 3600 300)
      reswapOf dupH `shouldBe` Just "none"
      [m] <- runQueryEffect tr (MonitorsM.cronMonitorsByProject testPid)
      apiKey <- createTestAPIKey tr testPid "cron-key"
      -- Check-ins go through ingestion (which also writes TimeFusion, read on CI) and every
      -- evaluation is retried until the check-in it depends on is visible.
      let at dt = addUTCTime dt frozenTime
          checkin status dt = ingestSessionEvent tr apiKey "cron.checkin" [("monitor.slug", "nightly-billing"), ("monitor.status", status)] False (at dt)
          evaluate dt = do
            fresh <- runQueryEffect tr (MonitorsM.cronMonitorsByProject testPid) >>= maybe (fail "monitor gone") pure . listToMaybe
            runTestBg (at dt) tr $ BackgroundJobs.evaluateCronMonitor (at dt) fresh
          issue sc = runQueryEffect tr (Issues.selectIssueByHash testPid (Issues.cronTargetHash (UUID.toText m.id)) sc)
          evaluateUntil dt sc ok = eventually (evaluate dt >> issue sc) ok
      checkin "ok" 0
      void $ eventually (evaluate 60 >> (.lastCheckinAt) <<$>> runQueryEffect tr (MonitorsM.cronMonitorsByProject testPid)) (== [Just (at 0)])
      (isNothing <$> issue Issues.AnyIssue) `shouldReturn` True
      -- Past interval + grace with no newer check-in.
      missed <- evaluateUntil (3600 + 300 + 120) Issues.AnyIssue isJust >>= maybe (fail "no missed issue") pure
      (missed.title, isNothing missed.archivedAt) `shouldBe` ("Cron missed: Nightly billing", True)
      checkin "ok" 4200
      (fmap (isJust . (.archivedAt)) <$> evaluateUntil 4260 Issues.AnyIssue (any (isJust . (.archivedAt)))) `shouldReturn` Just True
      checkin "error" 8000
      failed <- evaluateUntil 8060 (Issues.OpenOfType Issues.Cron) isJust >>= maybe (fail "no failed issue") pure
      failed.title `shouldBe` "Cron failed: Nightly billing"

    it "claims each due target once and leases it (multi-node safe), skipping disabled" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid PromCfg.CKPrometheus "svc-a" "http://a/metrics" 3600 Nothing (AE.object []) Nothing
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid PromCfg.CKPrometheus "svc-b" "http://b/metrics" 3600 Nothing (AE.object []) Nothing
      claimed1 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed1 `shouldBe` 2 -- both due, claimed
      claimed2 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed2 `shouldBe` 0 -- leased within interval ⇒ a second node's claim gets nothing
      -- a disabled target is never claimed even though its interval has elapsed
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid PromCfg.CKPrometheus "svc-c" "http://c/metrics" 1 Nothing (AE.object []) Nothing
      (c : _) <- V.toList . V.filter ((== "svc-c") . (.name)) <$> runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKPrometheus)
      void $ runQueryEffect tr $ PromCfg.setEnabled testPid c.id False
      claimed3 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed3 `shouldBe` 0

    -- A duplicate-name save must be caught by the DB UNIQUE(project_id, name) constraint
    -- (migration 0103) and turned into a friendly error — not a 500 (which is what a missed
    -- Hasql.isUniqueViolation classification would produce) and not a second row. Drive it
    -- through prometheusPostH so the handler's try+isUniqueViolation+inline error wiring is pinned
    -- end-to-end: reaching the assertion proves the second save did not throw.
    it "prometheusRejectedSave_preservesDraftWithoutRedirectingOrDuplicatingTargets" \tr -> do
      let form = PrometheusForm{name = "dup", url = "http://example.com/metrics", scrapeInterval = Nothing, authHeader = Nothing, extraLabels = Nothing, clearAuth = Nothing}
      _ <- testServant tr $ prometheusPostH testPid form
      (rejected, rejection) <- testServant tr $ prometheusPostH testPid form
      lookupResponseHeader @"HX-Redirect" @Text rejected `shouldBe` MissingHeader
      reswapOf rejected `shouldBe` Just "none"
      let errorHtml = renderText (toHtml rejection)
      for_ ["role=\"alert\"", "already exists", "hx-swap-oob=\"innerHTML target:", "dialog[open] .prom-save-result"] $ \expected -> errorHtml `shouldSatisfy` TL.isInfixOf expected
      (invalid, _) <- testServant tr $ prometheusPostH testPid form{url = "http://127.0.0.1/metrics"}
      reswapOf invalid `shouldBe` Just "none"
      configs <- runQueryEffect tr (PromCfg.configsByProjectId testPid PromCfg.CKPrometheus)
      V.length (V.filter ((== "dup") . (.name)) configs) `shouldBe` 1


reswapOf :: RespHeaders a -> Maybe Text
reswapOf h = case lookupResponseHeader @"HX-Reswap" h of
  Header v -> Just v
  _ -> Nothing
