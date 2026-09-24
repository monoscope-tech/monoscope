module Pkg.QueryCacheSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, replicateConcurrently)
import Data.Effectful.Hasql (SqlSource (..))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Pages.Charts.Charts (DataType (..), MetricsData (..), convertTimestampsToMs)
import Pkg.Parser (RangeEnd (..), SqlQueryCfg (..), defPid, defSqlQueryCfg, fixedUTCTime)
import Pkg.Parser.Expr (Subject (..))
import Pkg.Parser.Stats (BinFunction (..), ByClauseItem (..), Section (..), SummarizeByClause (..))
import Pkg.QueryCache (CacheKey (..), RawCacheKey (..), bucketStart, chartChunks, coalesceRawQuery, generateCacheKey, hasSummarizeWithBin, mergeTimeseriesData, newRawQueryFlights, trimOldData, trimToRange)
import Relude
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldReturn)


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
  describe "raw SQL request coalescing" do
    it "runs one leader for concurrent requests with the complete endpoint key" do
      flights <- newRawQueryFlights
      calls <- newIORef (0 :: Int)
      let key = rawKey "endpoint-a" (Just "production") "5 minutes"
          fetch = do
            atomicModifyIORef' calls $ \n -> (n + 1, ())
            threadDelay 100_000
            pure emptyMetrics
      results <- replicateConcurrently 12 $ coalesceRawQuery flights key fetch
      readIORef calls `shouldReturn` 1
      length (filter fst results) `shouldBe` 1
      map ((.rowsCount) . snd) results `shouldBe` replicate 12 0

    it "does not coalesce requests when an explicit key dimension changes" do
      flights <- newRawQueryFlights
      calls <- newIORef (0 :: Int)
      let key = rawKey
          keys :: [RawCacheKey]
          keys = [key "endpoint-a" Nothing "5 minutes", key "endpoint-b" Nothing "5 minutes", key "endpoint-a" (Just "production") "5 minutes", key "endpoint-a" Nothing "1 hour"]
          fetch = atomicModifyIORef' calls (\n -> (n + 1, ())) $> emptyMetrics
      void $ mapConcurrently (\k -> coalesceRawQuery flights k fetch) keys
      readIORef calls `shouldReturn` length keys

  describe "chart response event rate" do
    it "uses event totals and the selected interval for fresh, merged and trimmed data" do
      let fresh = (mkMetrics [(86400, 103680000)]){from = Just 0, to = Just 2592000, rowsCount = 103680000, rowsPerMin = Just 0.01}
          merged = mergeTimeseriesData (mkMetrics [(86400, 51840000)]) (mkMetrics [(172800, 51840000)])
          trimmed = trimToRange merged (mkTime 0) (mkTime 2592000)
      forM_ [fresh, trimmed] \metrics -> do
        let response = convertTimestampsToMs metrics
        response.rowsPerMin `shouldBe` Just 2400
        response.to `shouldBe` Just 2592000000
      forM_ [0, -1] \end ->
        (convertTimestampsToMs fresh{to = Just end}).rowsPerMin `shouldBe` Just 0
      (convertTimestampsToMs fresh{rowsCount = 0, dataset = V.empty}).rowsPerMin `shouldBe` Just 0
      (convertTimestampsToMs fresh{rowsPerMin = Nothing}).rowsPerMin `shouldBe` Nothing

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

    it "separates cache entries for distinct environment scopes" do
      let sections = [SummarizeCommand [] (Just $ SummarizeByClause [ByBinFunc $ Bin timestampSubject "10m"])]
          cfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
          prodKey = generateCacheKey defPid Nothing sections cfg{environment = Just "production"}
          stagingKey = generateCacheKey defPid Nothing sections cfg{environment = Just "staging"}
      prodKey.queryHash `shouldNotBe` stagingKey.queryHash

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


rawKey :: Text -> Maybe Text -> Text -> RawCacheKey
rawKey endpointHash environment rollupInterval =
  RawCacheKey
    { projectId = defPid
    , endpointHash
    , from = mkTime 10
    , to = mkTime 20
    , environment
    , service = Nothing
    , sqlQuery = "SELECT 1"
    , kqlQuery = ""
    , rollupInterval
    , backend = SqlTimefusion
    , decoder = DTText
    }
