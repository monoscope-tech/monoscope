-- | Unit tests for the CLI's pure renderers. Everything under test takes data
-- and returns @[Text]@, so these assert on exactly what a user would see —
-- no terminal, no server, no golden files.
module Main (main) where

import CLI.Chart
import CLI.Commands (OpenTarget (..), parseOpenTarget, uiPath)
import CLI.Dashboard qualified as Dash
import CLI.LogView
import Data.Aeson qualified as AE
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text qualified as T
import Relude
import Test.Hspec


main :: IO ()
main = hspec do
  describe "CLI.Chart" do
    it "sizes the plot to the requested rows and columns" do
      let out = renderTimeseries opts{width = 40, height = 6} [Series "a" [(fromIntegral i, Just (fromIntegral (i * i))) | i <- [0 :: Int .. 9]]]
      -- 6 plot rows + axis + x labels; every row fits the width budget.
      length out `shouldBe` 8
      all ((<= 40) . visibleWidth) out `shouldBe` True

    it "says so rather than drawing an empty grid when there is no data" do
      renderTimeseries opts [Series "a" []] `shouldSatisfy` any ("no data in range" `T.isInfixOf`)

    it "survives a flat series (zero-height y range)" do
      let out = renderTimeseries opts [Series "a" [(fromIntegral i, Just 5) | i <- [0 :: Int .. 5]]]
      out `shouldSatisfy` any (T.any (> '\x2800'))

    it "scales bars against the largest value, not the sum" do
      let out = renderBars opts{width = 40} [("a", 10), ("b", 5)]
      -- The top bar fills its track; the half-size one is visibly shorter.
      let blocks i = T.length (T.filter (== '\x2588') (fromMaybe "" (out !!? i)))
      blocks 0 `shouldSatisfy` (> blocks 1)

    it "keeps every bar row inside the width budget" do
      all ((<= 40) . visibleWidth) (renderBars opts{width = 40} [("a-very-long-service-name-here", 10), ("b", 5)]) `shouldBe` True

    it "formats values with SI and duration units" do
      map (formatValue "") [0, 12, 1234, 2_500_000] `shouldBe` ["0", "12", "1.2k", "2.5M"]
      formatValue "ms" 1500 `shouldBe` "1.5s"
      formatValue "%" 99.42 `shouldBe` "99.4%"

    it "reads a leading timestamp column as the x axis" do
      let md =
            decodeOrDie
              [aesonQQ|{ "headers": ["timestamp","hits"]
                                    , "dataset": [[1000, 5], [2000, 7]]
                                    , "data_float": null, "data_json": [], "data_text": []
                                    , "rows_count": 2, "rows_per_min": null
                                    , "from": null, "to": null, "stats": null, "error": null }|]
      map (.points) (seriesFromMetrics md) `shouldBe` [[(1000, Just 5), (2000, Just 7)]]

    it "pads and truncates to an exact visible width despite ANSI escapes" do
      visibleWidth (padTo 10 (colorize True (seriesColor 0) "abc")) `shouldBe` 10
      visibleWidth (padTo 4 "abcdefgh") `shouldBe` 4

  describe "CLI.LogView" do
    it "renders one line per event, within the width budget" do
      let out = map (renderEventLine False 100) (eventRows sampleEvents)
      length out `shouldBe` 2
      all ((<= 100) . visibleWidth) out `shouldBe` True

    it "puts the clock time, service and message on the line" do
      let l = fromMaybe "" (viaNonEmpty head (map (renderEventLine False 120) (eventRows sampleEvents)))
      l `shouldSatisfy` T.isInfixOf "01:02:03.456"
      l `shouldSatisfy` T.isInfixOf "checkout"
      l `shouldSatisfy` T.isInfixOf "GET /cart"

    it "flags an error row" do
      let ls = map (renderEventLine False 120) (eventRows sampleEvents)
      ls !!? 1 `shouldSatisfy` maybe False (T.isInfixOf "ERR")

    it "emits greppable key=value pairs in logfmt" do
      let l = fromMaybe "" (viaNonEmpty head (map renderLogfmt (eventRows sampleEvents)))
      l `shouldSatisfy` T.isInfixOf "service=checkout"
      l `shouldSatisfy` T.isInfixOf "trace=t1"

    it "rejects an unknown --format instead of guessing" do
      parseLogFormat "logfmt" `shouldBe` Right FmtLogfmt
      parseLogFormat "nope" `shouldSatisfy` isLeft

    it "draws a waterfall with a row per span, children indented" do
      let out = renderWaterfall False 100 (eventRows sampleEvents)
      length out `shouldBe` 3 -- header + 2 spans
      out !!? 2 `shouldSatisfy` maybe False (" " `T.isPrefixOf`)

    it "still renders when a span's parent is missing from the result" do
      renderWaterfall False 80 (eventRows orphanEvent) `shouldSatisfy` ((== 2) . length)

  describe "CLI.Commands (open)" do
    it "builds deep links for each target" do
      uiPath "PID" OpenDashboard (Just "d1") Nothing `shouldBe` "/p/PID/dashboards/d1"
      uiPath "PID" OpenIssue (Just "i1") Nothing `shouldBe` "/p/PID/issues/i1"
      uiPath "PID" OpenProject Nothing Nothing `shouldBe` "/p/PID"
      uiPath "PID" OpenMonitors Nothing (Just "24h") `shouldBe` "/p/PID/monitors?since=24h"

    it "percent-encodes a KQL query into the log explorer link" do
      uiPath "PID" OpenLogs (Just "severity.text==\"ERROR\"") (Just "6h")
        `shouldBe` "/p/PID/log_explorer?query=severity.text%3D%3D%22ERROR%22&since=6h"

    it "rejects an unknown target" do
      parseOpenTarget "trace" `shouldBe` Just OpenTrace
      parseOpenTarget "nope" `shouldBe` Nothing

  describe "CLI.Dashboard" do
    it "puts side-by-side widgets on the same terminal row" do
      let out = Dash.renderDashboard False 80 (decodeOrDie sampleDashboard)
      -- Both titles land on one line because both widgets span half the grid.
      out `shouldSatisfy` any (\l -> "Left" `T.isInfixOf` l && "Right" `T.isInfixOf` l)

    it "never exceeds the terminal width" do
      all ((<= 80) . visibleWidth) (Dash.renderDashboard False 80 (decodeOrDie sampleDashboard)) `shouldBe` True

    it "shows a widget's query error instead of an empty box" do
      Dash.renderDashboard False 80 (decodeOrDie sampleDashboard) `shouldSatisfy` any (T.isInfixOf "query failed")


opts :: ChartOpts
opts = defaultChartOpts{colorful = False}


decodeOrDie :: AE.FromJSON a => AE.Value -> a
decodeOrDie v = case AE.fromJSON v of
  AE.Success a -> a
  AE.Error e -> error (toText e)


-- | Two rows in the server's column-indexed envelope: a parent span and its
-- failing child.
sampleEvents :: AE.Value
sampleEvents =
  [aesonQQ|{ "colIdxMap": { "id": 0, "timestamp": 1, "service": 2, "span_name": 3
                          , "duration": 4, "trace_id": 5, "parent_id": 6
                          , "start_time_ns": 7, "errors": 8, "kind": 9 }
           , "logsData":
               [ ["s1", "2026-08-06T01:02:03.456789Z", "checkout", "GET /cart", "120000000", "t1", "", "1000000000", "false", "server"]
               , ["s2", "2026-08-06T01:02:03.500000Z", "cart-db",  "SELECT",    "40000000",  "t1", "s1", "1040000000", "true",  "client"]
               ]
           }|]


orphanEvent :: AE.Value
orphanEvent =
  [aesonQQ|{ "colIdxMap": { "id": 0, "timestamp": 1, "span_name": 2, "duration": 3, "parent_id": 4, "start_time_ns": 5 }
           , "logsData": [["s9", "2026-08-06T01:02:03Z", "orphan", "5000000", "missing-parent", "1000000000"]]
           }|]


sampleDashboard :: AE.Value
sampleDashboard =
  [aesonQQ|{ "title": "Ops", "tab": null, "tabs": [], "since": "1h"
           , "widgets":
               [ { "id": "l", "title": "Left", "subtitle": null, "w_type": "timeseries", "unit": null
                 , "layout": {"x": 0, "y": 0, "w": 6, "h": 3}, "value": null
                 , "headers": ["timestamp", "hits"], "rows": [[1000, 3], [2000, 9]]
                 , "text_rows": [], "error": null, "columns": [], "children": [] }
               , { "id": "r", "title": "Right", "subtitle": null, "w_type": "timeseries", "unit": null
                 , "layout": {"x": 6, "y": 0, "w": 6, "h": 3}, "value": null
                 , "headers": [], "rows": [], "text_rows": [], "error": "boom", "columns": [], "children": [] }
               ]
           }|]
