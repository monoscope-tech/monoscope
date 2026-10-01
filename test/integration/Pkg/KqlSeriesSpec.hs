module Pkg.KqlSeriesSpec (spec) where

import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Charts qualified as Charts
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn)
import Text.Read (read)


baseTime :: UTCTime
baseTime = read "2025-01-01 00:00:00 UTC"


-- | Points every 30s from base-10m to base+10m. Two cumulative series of one counter at
-- 100/s ("a" restarts at +120s), a DELTA counter at 100/s, and two gauges.
seed :: TestResources -> Projects.ProjectId -> IO ()
seed tr pid = withResource tr.trPool \conn ->
  void
    $ execute
      conn
      [sql| INSERT INTO otel_metrics (project_id, timestamp, start_timestamp, id, series_id, metric_name, metric_type, aggregation_temporality, attributes, value)
            SELECT ?, ?::timestamptz + make_interval(secs => s), CASE WHEN m.temporality = 'DELTA' THEN ?::timestamptz + make_interval(secs => s - 30) END,
                   gen_random_uuid(), m.series, m.name, m.mtype, m.temporality, jsonb_build_object('t', m.t),
                   CASE m.series WHEN 'ca' THEN CASE WHEN s < 120 THEN 100 * (s + 600) ELSE 100 * (s - 90) END
                                 WHEN 'cb' THEN 100 * (s + 600) WHEN 'd' THEN 3000 WHEN 'g1' THEN 1000 + s ELSE 2000 + s END
            FROM generate_series(-600, 600, 30) AS s,
                 (VALUES ('ca', 'c.total', 'SUM', 'CUMULATIVE', 'a'), ('cb', 'c.total', 'SUM', 'CUMULATIVE', 'b'), ('d', 'd.total', 'SUM', 'DELTA', 'a'),
                         ('g1', 'g.bytes', 'GAUGE', NULL, 'a'), ('g2', 'g.bytes', 'GAUGE', NULL, 'b')) AS m(series, name, mtype, temporality, t) |]
      (pid.toText, baseTime, baseTime)


spec :: Spec
spec = around withTestResources $ describe "rate / increase / last" do
  it "are counter-aware per series, reset-safe, honour every by column, and cover DELTA and gauges" \tr -> do
    pid <- UUIDId <$> UUIDV4.nextRandom
    seed tr pid
    let at s = toText $ iso8601Show $ addUTCTime s baseTime
        chart dt q = runQueryEffect tr $ Charts.queryMetrics Nothing dt (Just pid) (Just q) Nothing Nothing (Just $ at 0) (Just $ at 290) (Just "metrics") Nothing []
        series q = do
          md <- chart (Just Charts.DTMetric) q
          md.error `shouldBe` Nothing
          pure (V.toList (V.drop 1 md.headers), map (V.toList . V.drop 1) (V.toList md.dataset))
        rows = fmap snd . series
        scalar q = (.dataFloat) <$> chart (Just Charts.DTFloat) q
        counter = "metrics | where metric_name == \"c.total\" | summarize "
    -- One point per 10s bin: each bin's first point still reads its predecessor (the
    -- first one from before the range), and the restart reads as 100/s, not a spike.
    series (counter <> "rate(value) by bin(timestamp, 10s), attributes.t") `shouldReturn` (["a", "b"], replicate 10 [Just 100, Just 100])
    -- Without a by, per-series rates add up.
    rows (counter <> "rate(value) by bin(timestamp, 10s)") `shouldReturn` replicate 10 [Just 200]
    scalar (counter <> "rate(value)") `shouldReturn` Just 200
    -- Several by columns, one series per combination.
    (fst <$> series "metrics | where metric_name in (\"c.total\", \"d.total\") | summarize rate(value) by bin(timestamp, 10s), metric_name, attributes.t") `shouldReturn` ["c.total / a", "c.total / b", "d.total / a"]
    -- increase over the window = value at the end - value before it, through the reset.
    scalar (counter <> "increase(value)") `shouldReturn` Just 60000
    (sum . concatMap catMaybes <$> rows (counter <> "increase(value) by bin(timestamp, 1m)")) `shouldReturn` 60000
    -- DELTA points are their own increment, over their start..end interval.
    rows "metrics | where metric_name == \"d.total\" | summarize rate(value) by bin(timestamp, 10s)" `shouldReturn` replicate 10 [Just 100]
    -- last: newest point of each gauge series in the bin, summed across series.
    (take 1 <$> rows "metrics | where metric_name == \"g.bytes\" | summarize last(value) by bin(timestamp, 1m)") `shouldReturn` [[Just 3060]]
