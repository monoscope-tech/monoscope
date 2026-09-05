module Pkg.QueryCacheSpec (spec) where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Pages.Charts.Charts (MetricsData (..))
import Pkg.Parser (RangeEnd (..), defPid, defSqlQueryCfg, fixedUTCTime)
import Pkg.Parser.Expr (Subject (..))
import Pkg.Parser.Stats (BinFunction (..), ByClauseItem (..), Section (..), SummarizeByClause (..))
import Pkg.QueryCache (CacheKey (..), bucketStart, chartChunks, generateCacheKey, hasSummarizeWithBin, mergeTimeseriesData, trimOldData, trimToRange)
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)


mkTime :: Int -> UTCTime
mkTime = posixSecondsToUTCTime . fromIntegral


mkMetrics :: [(Double, Double)] -> MetricsData
mkMetrics rows =
  MetricsData
    { dataset = V.fromList $ map (\(ts, val) -> V.fromList [Just ts, Just val]) rows
    , dataFloat = Nothing
    , dataJSON = V.empty
    , dataText = V.empty
    , headers = V.fromList ["timestamp", "value"]
    , rowsCount = fromIntegral $ length rows
    , rowsPerMin = Nothing
    , from = floor . fst <$> viaNonEmpty head rows
    , to = floor . fst <$> viaNonEmpty last rows
    , stats = Nothing
    , error = Nothing
    }


emptyMetrics :: MetricsData
emptyMetrics = MetricsData V.empty Nothing V.empty V.empty V.empty 0 Nothing Nothing Nothing Nothing Nothing


timestampSubject :: Subject
timestampSubject = Subject "timestamp" "timestamp" []


spec :: Spec
spec = do
  describe "chart chunk boundaries" do
    it "covers a range once, newest first, including the inclusive final endpoint" do
      chartChunks "1 hour" (mkTime 17, mkTime 20000)
        `shouldBe` [(mkTime 14400, mkTime 20000, InclusiveEnd), (mkTime 17, mkTime 14400, ExclusiveEnd)]
      chartChunks "1 hour" (mkTime 0, mkTime 0) `shouldBe` []
      bucketStart "1 hour" (mkTime (-1)) `shouldBe` mkTime (-3600)
      bucketStart "1w" (mkTime 700000) `shouldBe` mkTime 604800
      bucketStart "700ms" (mkTime 1) `shouldBe` posixSecondsToUTCTime 0.7

    it "keeps internal boundaries on whole buckets for widths that do not divide a day" do
      forM_ ["5h", "7h"] \interval -> do
        let chunks = chartChunks interval (mkTime 17, mkTime 864000)
        forM_ chunks \(lower, upper, _) -> do
          when (lower /= mkTime 17) $ bucketStart interval lower `shouldBe` lower
          when (upper /= mkTime 864000) $ bucketStart interval upper `shouldBe` upper
        map (\(lower, _, _) -> lower) (take (length chunks - 1) chunks)
          `shouldBe` map (\(_, upper, _) -> upper) (drop 1 chunks)

  describe "hasSummarizeWithBin" do
    it "detects bin(timestamp, interval)" do
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [ByBinFunc $ Bin timestampSubject "5m"])]
      hasSummarizeWithBin sections `shouldBe` True

    it "detects bin_auto(timestamp)" do
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [ByBinFunc $ BinAuto timestampSubject])]
      hasSummarizeWithBin sections `shouldBe` True

    it "returns False for summarize without bin" do
      let sections = [SummarizeCommand [] Nothing]
      hasSummarizeWithBin sections `shouldBe` False

    it "returns False for empty sections" do
      let sections = [] :: [Section]
      hasSummarizeWithBin sections `shouldBe` False

  describe "generateCacheKey" do
    it "produces consistent hash for same query" do
      let cfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [ByBinFunc $ Bin timestampSubject "5m"])]
      let key1 = generateCacheKey defPid Nothing sections cfg
      let key2 = generateCacheKey defPid Nothing sections cfg
      key1.queryHash `shouldBe` key2.queryHash

    it "extracts bin interval" do
      let cfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [ByBinFunc $ Bin timestampSubject "10m"])]
      let key = generateCacheKey defPid Nothing sections cfg
      key.binInterval `shouldBe` "10m"

  describe "mergeTimeseriesData" do
    it "merges empty cached with new data" do
      let new = mkMetrics [(1000, 10), (2000, 20)]
      let result = mergeTimeseriesData emptyMetrics new
      V.length result.dataset `shouldBe` 2

    it "merges cached with empty new data" do
      let cached = mkMetrics [(1000, 10), (2000, 20)]
      let result = mergeTimeseriesData cached emptyMetrics
      V.length result.dataset `shouldBe` 2

    it "merges and sorts by timestamp" do
      let cached = mkMetrics [(1000, 10), (3000, 30)]
      let new = mkMetrics [(2000, 20), (4000, 40)]
      let result = mergeTimeseriesData cached new
      V.length result.dataset `shouldBe` 4
      (join $ V.headM $ V.head result.dataset) `shouldBe` Just 1000
      (join $ V.headM $ V.last result.dataset) `shouldBe` Just 4000

    it "deduplicates by timestamp keeping latest" do
      let cached = mkMetrics [(1000, 10), (2000, 20)]
      let new = mkMetrics [(2000, 25), (3000, 30)]
      let result = mergeTimeseriesData cached new
      V.length result.dataset `shouldBe` 3
      let vals = V.mapMaybe (\r -> join $ V.headM (V.drop 1 r)) result.dataset
      V.toList vals `shouldBe` [10, 25, 30]

  describe "trimToRange" do
    it "filters rows within range" do
      let metrics = mkMetrics [(1000, 10), (2000, 20), (3000, 30), (4000, 40)]
      let result = trimToRange metrics (mkTime 1500) (mkTime 3500)
      V.length result.dataset `shouldBe` 2
      result.from `shouldBe` Just 1500
      result.to `shouldBe` Just 3500

    it "returns empty for range with no data" do
      let metrics = mkMetrics [(1000, 10), (2000, 20)]
      let result = trimToRange metrics (mkTime 5000) (mkTime 6000)
      V.length result.dataset `shouldBe` 0

  describe "trimOldData" do
    it "removes data before window start" do
      let metrics = mkMetrics [(1000, 10), (2000, 20), (3000, 30)]
      let result = trimOldData (mkTime 2000) metrics
      V.length result.dataset `shouldBe` 2
      (join $ V.headM $ V.head result.dataset) `shouldBe` Just 2000
