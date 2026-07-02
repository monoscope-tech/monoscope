module Pages.QueryCacheSpec (spec) where

import Data.Default (def)
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (execute_)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Charts qualified as Charts
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Parser (dateRange, defSqlQueryCfg, parseQueryToAST)
import Pkg.QueryCache qualified as QC
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn, shouldSatisfy)
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
  runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just query) Nothing Nothing (Just timeFrom) (Just timeTo) (Just "spans") []


-- Format time offset from baseTime as ISO8601 string
timeAt :: Int -> Text
timeAt offsetSeconds = toText $ iso8601Show $ addUTCTime (fromIntegral offsetSeconds) baseTime


-- Read the cache watermark (cached_to) for a given key.
cachedToFor :: TestResources -> QC.CacheKey -> IO (Maybe UTCTime)
cachedToFor tr key = withResource tr.trPool \conn -> do
  rows <-
    PG.query
      conn
      [sql|SELECT cached_to FROM query_cache WHERE project_id = ? AND query_hash = ? AND bin_interval = ? AND source = ?|]
      (key.projectId, key.queryHash, key.binInterval, key.source) :: IO [PG.Only UTCTime]
  pure $ PG.fromOnly <$> listToMaybe rows


-- Seed a non-empty cache entry for query @q@ covering [t0, t1], keyed exactly as
-- the request over [t0, t2] will build it (source = Nothing). Returns the key.
seedCache :: TestResources -> Text -> (UTCTime, UTCTime, UTCTime) -> IO QC.CacheKey
seedCache tr q (t0, t1, t2) = do
  sections <- either (fail . toString) pure (parseQueryToAST q)
  let cfg = (defSqlQueryCfg pid baseTime Nothing Nothing){dateRange = (Just t0, Just t2)}
      key = QC.generateCacheKey pid Nothing sections cfg
      seedTs = realToFrac (utcTimeToPOSIXSeconds t0) :: Double
      seed = def{Charts.dataset = V.singleton (V.fromList [Just seedTs, Just 5]), Charts.headers = V.fromList ["timestamp", "count"], Charts.rowsCount = 1}
  runQueryEffect tr $ QC.updateCache key (t0, t1) seed q
  pure key


-- Compare two MetricsData for equivalence including stats
compareResults :: Charts.MetricsData -> Charts.MetricsData -> Bool
compareResults a b =
  a.rowsCount
    == b.rowsCount
    && V.length a.dataset
    == V.length b.dataset
    && V.toList a.headers
    == V.toList b.headers
    && V.toList a.dataset
    == V.toList b.dataset
    && compareStats a.stats b.stats
  where
    compareStats Nothing Nothing = True
    compareStats (Just sa) (Just sb) = sa.min == sb.min && sa.max == sb.max && sa.sum == sb.sum && sa.count == sb.count && sa.maxGroupSum == sb.maxGroupSum
    compareStats _ _ = False


-- Verify timestamps are sorted ascending
isSorted :: V.Vector Double -> Bool
isSorted xs = V.and $ V.zipWith (<=) xs (V.drop 1 xs)


spec :: Spec
spec = around withTestResources do
  describe "Query Cache" do
    it "cache hit returns same results as fresh query" $ \tr -> do
      clearAllTestData tr
      key <- createTestAPIKey tr pid "cache-key-1"
      forM_ [0 .. 19] \i -> ingestLog tr key ("Log " <> show i) (addUTCTime (fromIntegral $ i * 60) baseTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

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
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      let query = "summarize count(*) by bin_auto(timestamp)"
      _ <- queryMetrics tr query (timeAt (-900)) (timeAt 0)

      -- Phase 2: add more data, query extended range (triggers partial hit)
      forM_ [0 .. 9] \i -> ingestLog tr key ("Log2 " <> show i) (addUTCTime (fromIntegral $ i * 60) baseTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
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
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      let query = "summarize count(*) by bin_auto(timestamp), kind"
      _ <- queryMetrics tr query (timeAt (-600)) (timeAt 0)

      -- Phase 2: add more data, query extended range
      forM_ [0 .. 4] \i -> do
        let ts = addUTCTime (fromIntegral $ i * 60) baseTime
        replicateM_ 4 $ ingestLog tr key ("Log2 " <> show i) ts
        replicateM_ 3 $ ingestTrace tr key ("GET /api2/" <> show i) ts
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      resultMerged <- queryMetrics tr query (timeAt (-600)) (timeAt 600)
      clearCache tr
      resultFresh <- queryMetrics tr query (timeAt (-600)) (timeAt 600)

      -- Verify multiple columns exist (timestamp + at least 2 kind values)
      V.length resultMerged.headers `shouldSatisfy` (>= 3)
      -- Stats must match - catches bugs where only first column was used for stats
      compareResults resultMerged resultFresh `shouldBe` True

  -- Regression: chart SQL failures used to be silently swallowed into empty
  -- datasets, hiding broken widgets behind the same "no data" overlay as a
  -- legitimately empty time range. The response must now carry an explicit
  -- `error` field, and the request must not crash the page.
  describe "Widget SQL failures" do
    it "non-existent column surfaces a sanitized error instead of empty data" $ \tr -> do
      clearAllTestData tr
      -- Reference a column that does not exist on otel_logs_and_spans.
      let q = "totally_made_up_column == \"x\" | summarize count(*) by bin_auto(timestamp)"
      result <- queryMetrics tr q (timeAt (-1800)) (timeAt 1800)
      result.error `shouldBe` Just "Column not found"
      V.length result.dataset `shouldBe` 0

  -- Regression: a partial-hit delta fetch that fails (timeout / TF planning
  -- error / dropped conn) used to still advance `cached_to` to the requested
  -- `to`, even though no new data was merged. That left a permanent hole
  -- between the old and new watermark — short ranges rendered near-empty while
  -- wider ranges (a separate bin_interval cache entry) looked fine. The
  -- watermark must never advance past data we actually fetched.
  describe "partial-hit watermark" do
    it "does not advance cached_to when the delta fetch errors" $ \tr -> do
      clearAllTestData tr
      -- Seed covers [t0, t1]; the bad-column query makes the delta over (t1, t2] error.
      let q = "totally_made_up_column == \"x\" | summarize count(*) by bin_auto(timestamp)"
          times@(_, t1, _) = (addUTCTime (-1800) baseTime, baseTime, addUTCTime 1800 baseTime)
      key <- seedCache tr q times
      result <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just q) Nothing Nothing (Just (timeAt (-1800))) (Just (timeAt 1800)) Nothing []
      -- A failed delta serves the (stale) cached rows rather than dropping them...
      V.length result.dataset `shouldBe` 1
      -- ...and must NOT advance the watermark, so the gap is retried next refresh.
      cachedToFor tr key `shouldReturn` Just t1

    it "does not populate cache when a cache-miss fetch errors" $ \tr -> do
      clearAllTestData tr
      let q = "totally_made_up_column == \"x\" | summarize count(*) by bin_auto(timestamp)"
      sections <- either (fail . toString) pure (parseQueryToAST q)
      let cfg = (defSqlQueryCfg pid baseTime Nothing Nothing){dateRange = (Just (addUTCTime (-1800) baseTime), Just (addUTCTime 1800 baseTime))}
          key = QC.generateCacheKey pid Nothing sections cfg
      _ <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just q) Nothing Nothing (Just (timeAt (-1800))) (Just (timeAt 1800)) Nothing []
      cachedToFor tr key `shouldReturn` Nothing

    -- Counterpart to the guard above: a delta that *succeeds* but legitimately
    -- returns no rows is not a failure, so the watermark must still advance.
    it "advances cached_to when a successful delta returns no rows" $ \tr -> do
      clearAllTestData tr
      let q = "summarize count(*) by bin_auto(timestamp)"
          times@(_, _, t2) = (addUTCTime (-1800) baseTime, baseTime, addUTCTime 1800 baseTime)
      key <- seedCache tr q times
      -- No telemetry ingested, so the delta over (t1, t2] succeeds with 0 rows.
      _ <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just q) Nothing Nothing (Just (timeAt (-1800))) (Just (timeAt 1800)) Nothing []
      cachedToFor tr key `shouldReturn` Just t2
