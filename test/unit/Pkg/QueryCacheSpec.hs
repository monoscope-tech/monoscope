module Pkg.QueryCacheSpec (spec) where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Pages.Charts.Types (MetricsData (..))
import Pkg.Parser (defPid, defSqlQueryCfg, fixedUTCTime)
import Pkg.Parser.Expr (Subject (..))
import Pkg.Parser.Stats (BinFunction (..), Section (..), SummarizeByClause (..))
import Pkg.QueryCache (CacheKey (..), generateCacheKey, hasSummarizeWithBin, mergeTimeseriesData, trimOldData, trimToRange)
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)


mkTime :: Int -> UTCTime
mkTime = posixSecondsToUTCTime . fromIntegral


mkMetrics :: [(Double, Double)] -> MetricsData
mkMetrics rows = MetricsData
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
  }


emptyMetrics :: MetricsData
emptyMetrics = MetricsData V.empty Nothing V.empty V.empty V.empty 0 Nothing Nothing Nothing Nothing


timestampSubject :: Subject
timestampSubject = Subject "timestamp" "timestamp" []


spec :: Spec
spec = do
  describe "hasSummarizeWithBin" do
    it "detects bin(timestamp, interval)" do
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [Right $ Bin timestampSubject "5m"])]
      hasSummarizeWithBin sections `shouldBe` True

    it "detects bin_auto(timestamp)" do
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [Right $ BinAuto timestampSubject])]
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
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [Right $ Bin timestampSubject "5m"])]
      let key1 = generateCacheKey defPid Nothing sections cfg
      let key2 = generateCacheKey defPid Nothing sections cfg
      key1.queryHash `shouldBe` key2.queryHash

    it "extracts bin interval" do
      let cfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [Right $ Bin timestampSubject "10m"])]
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
