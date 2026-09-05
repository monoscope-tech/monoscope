module Pages.QueryCacheSpec (spec) where

import Control.Exception qualified as E
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Data.Pool (defaultPoolConfig, destroyAllResources, newPool, setNumStripes, withResource)
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
import Servant qualified
import Servant.Types.SourceT qualified as Source
import System.Config (AuthContext (..))
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
  runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just query) Nothing Nothing (Just timeFrom) (Just timeTo) (Just "spans") Nothing []


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
      (key.projectId, key.queryHash, key.binInterval, key.source)
      :: IO [PG.Only UTCTime]
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


-- The parser remains valid while the isolated test database cannot execute the
-- query. Restore the table even if an assertion or request fails.
withoutTelemetryTable :: TestResources -> IO a -> IO a
withoutTelemetryTable tr =
  E.bracket_
    (rename "ALTER TABLE otel_logs_and_spans RENAME TO chart_test_unavailable")
    (rename "ALTER TABLE chart_test_unavailable RENAME TO otel_logs_and_spans")
  where
    rename sqlQuery = withResource tr.trPool $ \conn -> void $ execute_ conn sqlQuery


-- | Rejected by the parser before any SQL is built.
unparseableQuery :: Text
unparseableQuery = "totally_made_up_column == \"x\" | summarize count(*) by bin_auto(timestamp)"


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
  describe "Streaming cache and chunk boundaries" do
    it "retries a generated chart read after its pooled connection closes" $ \tr -> do
      clearAllTestData tr
      key <- createTestAPIKey tr pid "closed-chart-connection"
      ingestLog tr key "one event" (addUTCTime 3600 baseTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      let createPool = newPool $ setNumStripes (Just 1) $ defaultPoolConfig (PG.connectPostgreSQL tr.trConnStr) PG.close 1800 1
      E.bracket createPool destroyAllResources \pool -> do
        withResource pool \conn -> do
          [PG.Only backend] <- PG.query_ conn "SELECT pg_backend_pid()" :: IO [PG.Only Int]
          withResource tr.trPool $ \control -> void (PG.query control "SELECT pg_terminate_backend(?, 5000)" (PG.Only backend) :: IO [PG.Only Bool])
        let resources = tr{trATCtx = tr.trATCtx{timefusionPgPool = pool}}
        response <- runQueryEffect resources $ Charts.queryMetricsStream (Just "timefusion") (Just Charts.DTMetric) (Just pid) (Just "summarize count(*) by bin(timestamp, 1h)") Nothing Nothing (Just $ timeAt 0) (Just $ timeAt 259200) (Just "spans") Nothing []
        frames <- runExceptT (Source.runSourceT $ Servant.getResponse response) >>= either fail pure
        case viaNonEmpty last frames of
          Just (AE.Object obj) -> do
            KM.lookup "type" obj `shouldBe` Just (AE.String "complete")
            let count = KM.lookup "data" obj >>= \case AE.Object fields -> KM.lookup "rows_count" fields; _ -> Nothing
            count `shouldBe` Just (AE.Number 1)
          _ -> fail "missing complete chart frame"

    it "keeps a slow query's response active until completion" $ \tr -> do
      let slowSql = "SELECT 1::integer AS bucket, 'wait'::text AS series, count(*)::float AS value FROM pg_sleep(6)"
      response <- runQueryEffect tr $ Charts.queryMetricsStream (Just "postgres") (Just Charts.DTMetric) (Just pid) Nothing (Just slowSql) Nothing (Just $ timeAt 0) (Just $ timeAt 3600) (Just "spans") Nothing []
      values <- runExceptT $ Source.runSourceT $ Servant.getResponse response
      frames <- either fail pure values
      let kind = \case AE.Object obj -> KM.lookup "type" obj; _ -> Nothing
      fmap kind (listToMaybe frames) `shouldBe` Just (Just $ AE.String "partial")
      fmap kind (viaNonEmpty last frames) `shouldBe` Just (Just $ AE.String "complete")

    it "matches a whole query, populates the cache, and serves a hit without reading telemetry" $ \tr -> do
      clearAllTestData tr
      key <- createTestAPIKey tr pid "stream-cache-boundaries"
      forM_ [17, 3599, 3600, 86400, 172800, 259200] $ \offset ->
        ingestLog tr key ("boundary " <> show offset) (addUTCTime (fromIntegral offset) baseTime)
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      let q = "summarize count(*) by bin(timestamp, 1h)"
          runStream = do
            response <- runQueryEffect tr $ Charts.queryMetricsStream (Just "postgres") (Just Charts.DTMetric) (Just pid) (Just q) Nothing Nothing (Just $ timeAt 17) (Just $ timeAt 259200) (Just "spans") Nothing []
            values <- runExceptT $ Source.runSourceT $ Servant.getResponse response
            either fail pure values
          final :: [AE.Value] -> IO Charts.MetricsData
          final values = case viaNonEmpty last values of
            Just (AE.Object obj) | KM.lookup "type" obj == Just (AE.String "complete") ->
              case KM.lookup "data" obj of
                Just value -> case AE.fromJSON value of AE.Success md -> pure md; AE.Error message -> fail message
                Nothing -> fail "missing final data"
            _ -> fail "missing completion frame"
      expected <- queryMetrics tr q (timeAt 17) (timeAt 259200)
      clearCache tr
      frames <- runStream
      length frames `shouldSatisfy` (> 2)
      actual <- final frames
      actual.dataset `shouldBe` expected.dataset
      actual.headers `shouldBe` expected.headers
      actual.rowsCount `shouldBe` expected.rowsCount
      withResource tr.trPool $ \conn -> void $ execute_ conn [sql|DELETE FROM otel_logs_and_spans WHERE project_id = '00000000-0000-0000-0000-000000000000'|]
      cachedFrames <- runStream
      length cachedFrames `shouldBe` 1
      cached <- final cachedFrames
      cached.dataset `shouldBe` expected.dataset
      cached.rowsCount `shouldBe` expected.rowsCount

  describe "Query Cache" do
    it "reuses successful empty history without querying telemetry again" $ \tr -> do
      clearAllTestData tr
      let q = "summarize count(*) by bin(timestamp, 1h)"
          fetch = queryMetrics tr q (timeAt 0) (timeAt 3600)
      initial <- fetch
      initial.error `shouldBe` Nothing
      initial.dataset `shouldBe` V.empty
      cached <- withoutTelemetryTable tr fetch
      cached.error `shouldBe` Nothing
      cached.dataset `shouldBe` V.empty
      withResource tr.trPool $ \conn -> void $ execute_ conn [sql|UPDATE query_cache SET cached_data = jsonb_set(cached_data, '{error}', '"old failure"') WHERE project_id = '00000000-0000-0000-0000-000000000000'|]
      recovered <- fetch
      recovered.error `shouldBe` Nothing
      recovered.dataset `shouldBe` V.empty

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
    it "an unknown field surfaces the parser's message instead of empty data" $ \tr -> do
      clearAllTestData tr
      result <- queryMetrics tr unparseableQuery (timeAt (-1800)) (timeAt 1800)
      result.error `shouldBe` Just "Unknown field \"totally_made_up_column\""
      V.length result.dataset `shouldBe` 0

    it "a backend SQL failure surfaces a sanitized error instead of empty data" $ \tr -> do
      clearAllTestData tr
      result <- withoutTelemetryTable tr $ queryMetrics tr "summarize count(*) by bin_auto(timestamp)" (timeAt (-1800)) (timeAt 1800)
      result.error `shouldSatisfy` isJust

  -- Regression: a partial-hit delta fetch that fails (timeout / TF planning
  -- error / dropped conn) used to still advance `cached_to` to the requested
  -- `to`, even though no new data was merged. That left a permanent hole
  -- between the old and new watermark — short ranges rendered near-empty while
  -- wider ranges (a separate bin_interval cache entry) looked fine. The
  -- watermark must never advance past data we actually fetched.
  describe "partial-hit watermark" do
    it "does not advance cached_to when the delta fetch errors" $ \tr -> do
      clearAllTestData tr
      let q = "summarize count(*) by bin_auto(timestamp)"
          times@(_, t1, _) = (addUTCTime (-1800) baseTime, baseTime, addUTCTime 1800 baseTime)
      key <- seedCache tr q times
      result <- withoutTelemetryTable tr $ runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just q) Nothing Nothing (Just (timeAt (-1800))) (Just (timeAt 1800)) Nothing Nothing []
      result.error `shouldSatisfy` isJust
      cachedToFor tr key `shouldReturn` Just t1

    it "does not populate cache when a cache-miss fetch errors" $ \tr -> do
      clearAllTestData tr
      result <- withoutTelemetryTable tr $ queryMetrics tr "summarize count(*) by bin_auto(timestamp)" (timeAt (-1800)) (timeAt 1800)
      result.error `shouldSatisfy` isJust
      rows <- withResource tr.trPool $ \conn -> PG.query_ conn [sql|SELECT count(*) FROM query_cache WHERE project_id = '00000000-0000-0000-0000-000000000000'|]
      rows `shouldBe` [PG.Only (0 :: Int64)]

    -- Counterpart to the guard above: a delta that *succeeds* but legitimately
    -- returns no rows is not a failure, so the watermark must still advance.
    it "advances cached_to when a successful delta returns no rows" $ \tr -> do
      clearAllTestData tr
      let q = "summarize count(*) by bin_auto(timestamp)"
          times@(_, _, t2) = (addUTCTime (-1800) baseTime, baseTime, addUTCTime 1800 baseTime)
      key <- seedCache tr q times
      -- No telemetry ingested, so the delta over (t1, t2] succeeds with 0 rows.
      _ <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just q) Nothing Nothing (Just (timeAt (-1800))) (Just (timeAt 1800)) Nothing Nothing []
      cachedToFor tr key `shouldReturn` Just t2
