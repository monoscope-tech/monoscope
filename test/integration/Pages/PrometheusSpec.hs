module Pages.PrometheusSpec (spec) where

import Data.Aeson qualified as AE
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
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
      (cfg : _) <- V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId testPid)
      n <- runQueryEffect tr $ PromCfg.ingestScrapedBody cfg frozenTime promBody
      n `shouldBe` 3 -- 2 counter series + 1 gauge; +Inf dropped
      rows <-
        withPool tr.trPool
          $ DBT.query
            [sql| SELECT metric_name, metric_type FROM telemetry.metrics WHERE project_id = ? ORDER BY metric_name |]
            (Only testPid)
          :: IO (V.Vector (Text, Text))
      V.length rows `shouldBe` 3
      sort (V.toList rows) `shouldBe` [("http_requests_total", "SUM"), ("http_requests_total", "SUM"), ("temperature_celsius", "GAUGE")]

    it "claims each due target once and leases it (multi-node safe), skipping disabled" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "svc-a" "http://a/metrics" 3600 Nothing (AE.object [])
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "svc-b" "http://b/metrics" 3600 Nothing (AE.object [])
      claimed1 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed1 `shouldBe` 2 -- both due, claimed
      claimed2 <- runQueryEffect tr (PromCfg.claimDueConfigs 50)
      length claimed2 `shouldBe` 0 -- leased within interval ⇒ a second node's claim gets nothing
      -- a disabled target is never claimed even though its interval has elapsed
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "svc-c" "http://c/metrics" 1 Nothing (AE.object [])
      (c : _) <- V.toList . V.filter ((== "svc-c") . (.name)) <$> runQueryEffect tr (PromCfg.configsByProjectId testPid)
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
