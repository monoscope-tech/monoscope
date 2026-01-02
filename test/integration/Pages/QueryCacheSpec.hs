module Pages.QueryCacheSpec (spec) where

import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Charts qualified as Charts
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)
import Text.Read (read)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


baseTime :: UTCTime
baseTime = read "2025-01-01 00:00:00 UTC"


clearCache :: TestResources -> IO ()
clearCache tr = withResource tr.trPool \conn ->
  void $ execute_ conn [sql|DELETE FROM query_cache WHERE project_id = '00000000-0000-0000-0000-000000000000'|]


clearAllTestData :: TestResources -> IO ()
clearAllTestData tr = withResource tr.trPool \conn -> do
  void $ execute_ conn [sql|DELETE FROM query_cache WHERE project_id = '00000000-0000-0000-0000-000000000000'|]
  void $ execute_ conn [sql|DELETE FROM otel_logs_and_spans WHERE project_id = '00000000-0000-0000-0000-000000000000'|]


queryMetrics :: TestResources -> Text -> Text -> Text -> IO Charts.MetricsData
queryMetrics tr query timeFrom timeTo =
  runQueryEffect tr $ Charts.queryMetrics (Just Charts.DTMetric) (Just pid) (Just query) Nothing Nothing (Just timeFrom) (Just timeTo) (Just "spans") []


-- Format time offset from baseTime as ISO8601 string
timeAt :: Int -> Text
timeAt offsetSeconds = toText $ iso8601Show $ addUTCTime (fromIntegral offsetSeconds) baseTime


-- Compare two MetricsData for equivalence including stats
compareResults :: Charts.MetricsData -> Charts.MetricsData -> Bool
compareResults a b =
  a.rowsCount == b.rowsCount
    && V.length a.dataset == V.length b.dataset
    && V.toList a.headers == V.toList b.headers
    && V.toList a.dataset == V.toList b.dataset
    && compareStats a.stats b.stats
  where
    compareStats Nothing Nothing = True
    compareStats (Just sa) (Just sb) = sa.min == sb.min && sa.max == sb.max && sa.sum == sb.sum && sa.count == sb.count && sa.maxGroupSum == sb.maxGroupSum
    compareStats _ _ = False


-- Verify timestamps are sorted ascending
isSorted :: V.Vector Double -> Bool
isSorted xs = V.and $ V.zipWith (<=) xs (V.drop 1 xs)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Query Cache" do
    it "cache hit returns same results as fresh query" $ \tr -> do
      clearAllTestData tr
      key <- createTestAPIKey tr pid "cache-key-1"
      forM_ [0 .. 19] \i -> ingestLog tr key ("Log " <> show i) (addUTCTime (fromIntegral $ i * 60) baseTime)
      void $ runAllBackgroundJobs tr.trATCtx

      let (query, from, to) = ("summarize count(*) by bin_auto(timestamp)", timeAt (-1800), timeAt 1800)
      result1 <- queryMetrics tr query from to
      result2 <- queryMetrics tr query from to -- cache hit
      clearCache tr
      resultFresh <- queryMetrics tr query from to

      V.length result1.dataset `shouldSatisfy` (> 0)
      compareResults result1 result2 `shouldBe` True
      compareResults result1 resultFresh `shouldBe` True

    it "partial hit merge returns same results as fresh query with correct ordering" $ \tr -> do
      clearAllTestData tr
      key <- createTestAPIKey tr pid "cache-key-2"

      -- Phase 1: initial data, populate cache
      forM_ [0 .. 9] \i -> ingestLog tr key ("Log1 " <> show i) (addUTCTime (fromIntegral $ i * 60 - 600) baseTime)
      void $ runAllBackgroundJobs tr.trATCtx
      let query = "summarize count(*) by bin_auto(timestamp)"
      _ <- queryMetrics tr query (timeAt (-900)) (timeAt 0)

      -- Phase 2: add more data, query extended range (triggers partial hit)
      forM_ [0 .. 9] \i -> ingestLog tr key ("Log2 " <> show i) (addUTCTime (fromIntegral $ i * 60) baseTime)
      void $ runAllBackgroundJobs tr.trATCtx
      resultMerged <- queryMetrics tr query (timeAt (-900)) (timeAt 900)

      clearCache tr
      resultFresh <- queryMetrics tr query (timeAt (-900)) (timeAt 900)

      -- Verify data matches and timestamps are sorted
      compareResults resultMerged resultFresh `shouldBe` True
      let getTimestamps md = V.mapMaybe (join . V.headM) md.dataset
      isSorted (getTimestamps resultMerged) `shouldBe` True

    it "multi-column grouping calculates correct stats after partial hit merge" $ \tr -> do
      clearAllTestData tr
      key <- createTestAPIKey tr pid "cache-key-3"

      -- Phase 1: initial multi-column data (logs + traces create different 'kind' values)
      forM_ [0 .. 4] \i -> do
        let ts = addUTCTime (fromIntegral $ i * 60 - 300) baseTime
        replicateM_ 3 $ ingestLog tr key ("Log " <> show i) ts
        replicateM_ 2 $ ingestTrace tr key ("GET /api/" <> show i) ts
      void $ runAllBackgroundJobs tr.trATCtx

      let query = "summarize count(*) by bin_auto(timestamp), kind"
      _ <- queryMetrics tr query (timeAt (-600)) (timeAt 0)

      -- Phase 2: add more data, query extended range
      forM_ [0 .. 4] \i -> do
        let ts = addUTCTime (fromIntegral $ i * 60) baseTime
        replicateM_ 4 $ ingestLog tr key ("Log2 " <> show i) ts
        replicateM_ 3 $ ingestTrace tr key ("GET /api2/" <> show i) ts
      void $ runAllBackgroundJobs tr.trATCtx

      resultMerged <- queryMetrics tr query (timeAt (-600)) (timeAt 600)
      clearCache tr
      resultFresh <- queryMetrics tr query (timeAt (-600)) (timeAt 600)

      -- Verify multiple columns exist (timestamp + at least 2 kind values)
      V.length resultMerged.headers `shouldSatisfy` (>= 3)
      -- Stats must match - catches bugs where only first column was used for stats
      compareResults resultMerged resultFresh `shouldBe` True
