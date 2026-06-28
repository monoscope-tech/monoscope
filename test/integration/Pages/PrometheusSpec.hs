module Pages.PrometheusSpec (spec) where

import Data.Aeson qualified as AE
import Data.Time.Clock (getCurrentTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
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
      (cfg : _) <- runQueryEffect tr (PromCfg.configsByProjectId testPid)
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

    it "treats targets as due until scraped within their interval" \tr -> do
      void $ runQueryEffect tr $ PromCfg.insertConfig testPid "gauge-svc" "http://y/metrics" 3600 Nothing (AE.object [])
      (cfg : _) <- runQueryEffect tr (PromCfg.configsByProjectId testPid)
      due1 <- runQueryEffect tr PromCfg.dueConfigs
      length due1 `shouldBe` 1 -- never scraped ⇒ due
      now <- getCurrentTime
      void $ runQueryEffect tr $ PromCfg.markScraped cfg.id now "ok"
      due2 <- runQueryEffect tr PromCfg.dueConfigs
      length due2 `shouldBe` 0 -- scraped just now, 3600s interval not elapsed
      void $ runQueryEffect tr $ PromCfg.setEnabled testPid cfg.id False
      void $ runQueryEffect tr $ PromCfg.markScraped cfg.id frozenTime "ok" -- far in the past
      due3 <- runQueryEffect tr PromCfg.dueConfigs
      length due3 `shouldBe` 0 -- disabled targets are never due
