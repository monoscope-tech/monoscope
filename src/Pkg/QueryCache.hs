module Pkg.QueryCache (
  CacheKey (..),
  CacheEntry (..),
  CacheResult (..),
  RawCacheKey (..),
  RawCacheResult (..),
  RawQueryFlights,
  QueryFlights,
  newQueryFlights,
  coalesceQuery,
  withSharedCache,
  sharedCacheKey,
  cachedRawQuery,
  lookupRawCache,
  updateRawCache,
  generateCacheKey,
  lookupCache,
  updateCache,
  mergeTimeseriesData,
  trimToRange,
  trimOldData,
  rewriteBinAutoToFixed,
  cleanupExpiredCache,
  slidingWindowSeconds,
  cacheWindowStart,
  deltaOverlapSeconds,
  bucketStart,
  chartChunks,
  nextBucketStart,
  chunkIntervalSupported,
  replaceTimeseriesRange,
  summarizeSeries,

  -- * Trace session identity
  TraceSessionInfo (..),
  TraceSessionKey,
  TraceSessionCache,
  newTraceSessionCache,
  lookupAndStamp,
  evictStaleEntries,
) where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as E
import Control.Exception.Annotated qualified as Ann
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)

-- Merge, not Intro: the sort MUST be stable. 'mergeTimeseriesData' dedupes keeping the last
-- occurrence, and the delta is appended after the cached rows, so stability is
-- what makes "fresh data wins on an overlapping bin" true rather than arbitrary.
import Data.Vector.Algorithms.Merge qualified as VA
import Effectful (Eff, IOE, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Clock (getMonotonicTime)
import Hasql.Interpolate qualified as HI
import Models.Ids qualified as Projects
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), atMapText, generateSummary)
import OpenTelemetry.Attributes qualified as OA
import Pages.Charts.Types (DataType, MetricsData (..), MetricsStats (..))
import Pkg.DeriveUtils (AesonText (..), DB, unAesonText, unAesonTextMaybe)
import Pkg.Metrics qualified as Metrics
import Pkg.Parser (RangeEnd (..), SqlQueryCfg (..), autoBinWidth, fixedUTCTime, intervalSeconds)
import Pkg.Parser.Expr (ToQueryText (..), display, kqlTimespanToTimeBucket, resolveWildcardTimes)
import Pkg.Parser.Stats (BinFunction (..), ByClauseItem (..), Section (..), Sources (..), SummarizeByClause (..), defaultBinSize)
import Relude
import UnliftIO (withRunInIO)
import UnliftIO.Exception (onException)
import Utils (encodeText, jsonToMap, nestedJsonFromDotNotation, toXXHash)


-- $setup
-- >>> import Data.Default (def)
-- >>> import Data.Time (UTCTime (..))
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
-- >>> import Data.Vector qualified as V
-- >>> import Pages.Charts.Types (MetricsData (..))
-- >>> import Pkg.Parser (RangeEnd (..), SqlQueryCfg (..), defPid, defSqlQueryCfg, fixedUTCTime)
-- >>> import Pkg.Parser.Expr (Subject (..))
-- >>> import Pkg.Parser.Stats (BinFunction (..), ByClauseItem (..), Section (..), SummarizeByClause (..))
-- >>> import Relude
-- >>> :set -XOverloadedStrings
-- >>> let mkTime :: Int -> UTCTime; mkTime = posixSecondsToUTCTime . fromIntegral
-- >>> let emptyMetrics :: MetricsData; emptyMetrics = def
-- >>> let mkMetrics :: [(Double, Double)] -> MetricsData; mkMetrics rows = emptyMetrics{dataset = V.fromList [V.fromList [Just ts, Just val] | (ts, val) <- rows], headers = V.fromList ["timestamp", "value"], rowsCount = fromIntegral (length rows), from = floor . fst <$> viaNonEmpty head rows, to = floor . fst <$> viaNonEmpty last rows}


data CacheKey = CacheKey
  { projectId :: Projects.ProjectId
  , source :: Text
  , queryHash :: Text
  , binInterval :: Text
  }
  deriving stock (Eq, Generic, Show)


data CacheEntry = CacheEntry
  { projectId :: Projects.ProjectId
  , source :: Text
  , queryHash :: Text
  , binInterval :: Text
  , originalQuery :: Text
  , cachedFrom :: UTCTime
  , cachedTo :: UTCTime
  , cachedData :: MetricsData
  , hitCount :: Int
  }
  deriving stock (Generic, Show)


data CacheResult = CacheHit CacheEntry | PartialHit CacheEntry | CacheMiss | CacheBypassed Text
  deriving stock (Show)


-- | Identity of one complete endpoint-widget SQL result. Unlike the incremental
-- timeseries cache above, every dimension is exact: a result is reusable only for
-- the same endpoint, requested window, scope, query templates and rollup width.
-- Backend and decoder are additional safety dimensions because identical SQL can
-- legitimately produce different wire values on PostgreSQL and TimeFusion.
data RawCacheKey = RawCacheKey
  { projectId :: Projects.ProjectId
  , endpointHash :: Text
  , from :: UTCTime
  , to :: UTCTime
  , environment :: Maybe Text
  , service :: Maybe Text
  , sqlQuery :: Text
  , kqlQuery :: Text
  , rollupInterval :: Text
  , backend :: Hasql.SqlSource
  , decoder :: DataType
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (AE.ToJSON)


data RawCacheResult = RawCacheHit MetricsData | RawCacheMiss
  deriving stock (Show)


-- | Per-process request coalescer. The durable result cache is shared through
-- PostgreSQL; this small registry prevents one web process from issuing the same
-- expensive TimeFusion query many times while the shared entry is still absent.
newtype QueryFlights key value = QueryFlights (STM.TVar (M.Map key (STM.TMVar (Either E.SomeException value))))


type RawQueryFlights = QueryFlights RawCacheKey MetricsData


newQueryFlights :: IO (QueryFlights key value)
newQueryFlights = QueryFlights <$> STM.newTVarIO M.empty


-- | The action includes cache lookup, execution and persistence. Publication and
-- removal are atomic, including when a leader is cancelled; followers cannot hang.
coalesceQuery :: Ord key => QueryFlights key value -> key -> IO value -> IO (Bool, value)
coalesceQuery (QueryFlights flights) key action = E.mask \restore -> do
  (leader, resultVar) <- STM.atomically do
    running <- STM.readTVar flights
    case M.lookup key running of
      Just existing -> pure (False, existing)
      Nothing -> do
        created <- STM.newEmptyTMVar
        STM.writeTVar flights $ M.insert key created running
        pure (True, created)
  if leader
    then do
      result <- E.try @E.SomeException $ restore action
      STM.atomically do
        STM.putTMVar resultVar result
        STM.modifyTVar' flights $ M.delete key
      either E.throwIO (pure . (True,)) result
    else do
      result <- restore $ STM.atomically $ STM.readTMVar resultVar
      either E.throwIO (pure . (False,)) result


-- | Every leader looks up the durable cache inside its flight and stores before
-- releasing it. Cache failures fall through to the backend, never to stale data.
cachedRawQuery :: (DB es, IOE :> es) => RawQueryFlights -> UTCTime -> RawCacheKey -> Eff es MetricsData -> Eff es MetricsData
cachedRawQuery flights now key fetch = do
  started <- liftIO getMonotonicTime
  let backend = [("backend", OA.toAttribute $ display key.backend)]
      recordOutcome outcome = do
        ended <- liftIO getMonotonicTime
        let attrs = ("outcome", OA.toAttribute (outcome :: Text)) : backend
        Metrics.bump Metrics.dashboardQueryCacheOutcomes attrs
        Metrics.recordMs Metrics.endpointCacheDuration ((ended - started) * 1000) attrs
        pure attrs
      finish outcome value = do
        recordOutcome outcome >>= Metrics.recordMs Metrics.endpointCacheResultBytes (fromIntegral $ LBS.length $ AE.encode value)
        pure value
      cacheFailure :: IOE :> es' => Text -> a -> Hasql.HasqlException -> Eff es' a
      cacheFailure operation fallback _ = do
        Metrics.bump Metrics.endpointCacheErrors (("operation", OA.toAttribute (operation :: Text)) : backend)
        pure fallback
      action = do
        cached <- lookupRawCache now key `Ann.catch` cacheFailure "lookup" RawCacheMiss
        case cached of
          RawCacheHit value -> do
            Metrics.bump Metrics.endpointCacheAvoidedQueries backend
            finish "raw-hit" value
          RawCacheMiss -> do
            Metrics.bump Metrics.endpointCacheBackendQueries backend
            value <- fetch
            updateRawCache now key value `Ann.catch` cacheFailure "store" ()
            finish "raw-miss" value
      run =
        withRunInIO (\unlift -> coalesceQuery flights key (unlift action)) >>= \case
          (True, value) -> pure value
          (False, value) -> Metrics.bump Metrics.endpointCacheAvoidedQueries backend >> finish "raw-coalesced" value
  run `onException` recordOutcome "raw-error"


rawCacheHash :: RawCacheKey -> Text
rawCacheHash = toXXHash . ("v2:" <>) . encodeText


lookupRawCache :: DB es => UTCTime -> RawCacheKey -> Eff es RawCacheResult
lookupRawCache now key = do
  rows :: [HI.OneColumn (AesonText MetricsData)] <-
    Hasql.interp
      [HI.sql|
      SELECT cached_data
      FROM query_cache
      WHERE project_id = #{key.projectId} AND source = 'raw-sql'
        AND query_hash = #{rawCacheHash key} AND bin_interval = #{key.rollupInterval}
        AND cached_from = #{key.from} AND cached_to = #{key.to}
        AND updated_at > #{now}::timestamptz - interval '60 seconds'
        AND updated_at <= #{now}
      LIMIT 1
    |]
  pure case rows of
    (HI.OneColumn (AesonText value) : _) | isNothing value.error -> RawCacheHit value
    _ -> RawCacheMiss


-- | Bound both staleness and entry size. The timestamp is the query snapshot,
-- not completion time, so a slow query cannot renew old data for another minute.
updateRawCache :: (DB es, IOE :> es) => UTCTime -> RawCacheKey -> MetricsData -> Eff es ()
updateRawCache now key value
  | isJust value.error = pass
  | LBS.length (AE.encode value) > 1048576 = Metrics.bump Metrics.endpointCacheOversized []
  | otherwise =
      Hasql.interpExecute_
        [HI.sql|
          INSERT INTO query_cache
            (project_id, source, query_hash, bin_interval, original_query,
             cached_from, cached_to, cached_data, hit_count, last_accessed_at, updated_at)
          VALUES
            (#{key.projectId}, 'raw-sql', #{rawCacheHash key}, #{key.rollupInterval}, #{key.sqlQuery},
             #{key.from}, #{key.to}, #{AesonText value}, 1, #{now}, #{now})
          ON CONFLICT (project_id, source, query_hash, bin_interval)
          DO UPDATE SET cached_data = EXCLUDED.cached_data,
            original_query = EXCLUDED.original_query,
            hit_count = query_cache.hit_count + 1,
            last_accessed_at = EXCLUDED.last_accessed_at,
            updated_at = EXCLUDED.updated_at
          WHERE query_cache.updated_at <= EXCLUDED.updated_at
        |]


-- | Rewrite bin_auto to fixed bin interval for delta fetches
rewriteBinAutoToFixed :: Text -> [Section] -> [Section]
rewriteBinAutoToFixed interval = map \case
  SummarizeCommand aggs (Just (SummarizeByClause items)) ->
    SummarizeCommand aggs (Just (SummarizeByClause (map rewriteItem items)))
  s -> s
  where
    rewriteItem (ByBinFunc (BinAuto subj)) = ByBinFunc (Bin subj interval)
    rewriteItem item = item


-- | Parse bin interval text to seconds (e.g., "5 minutes" -> 300, "1 hour" -> 3600)
parseBinIntervalToSeconds :: Text -> Int
parseBinIntervalToSeconds = max 1 . ceiling . intervalWidth


intervalWidth :: Text -> Rational
intervalWidth interval = fromMaybe 60 (kqlTimespanToTimeBucket interval >>= intervalSeconds)


-- Chart timestamps are whole seconds. Keep subsecond bins on the existing
-- single-query path rather than merge distinct groups with the same timestamp.
chunkIntervalSupported :: Text -> Bool
chunkIntervalSupported interval =
  let width = intervalWidth interval
   in width >= 1 && width == fromInteger (floor width)


-- | Calculate sliding window size in seconds based on bin interval
-- Keeps at least 48 data points or 24 hours, whichever is larger
slidingWindowSeconds :: Text -> Int
slidingWindowSeconds binInterval = max 86400 (parseBinIntervalToSeconds binInterval * 48)


-- | The oldest timestamp the cache may trim to for a request over @(from, to)@:
-- the bin-derived retention window, but never inside the range being viewed.
--
-- The retention window scales with bin width, so a fine bin shrinks it below the
-- requested range — and 'refetchUnlessAdequate' only refetches on an *empty*
-- result, so the caller would be handed the truncated series instead. At 1h bins
-- the window is 48h, well short of a 7-day chart:
--
-- >>> let day n = UTCTime (fromGregorian 2020 1 n) 0
-- >>> cacheWindowStart "1 hour" (day 1, day 8) == day 1
-- True
--
-- A narrow request keeps the full retention window, so refreshes stay incremental:
--
-- >>> cacheWindowStart "1 hour" (day 7, day 8) == day 6
-- True
cacheWindowStart :: Text -> (UTCTime, UTCTime) -> UTCTime
cacheWindowStart binInterval (from, to) =
  min from $ addUTCTime (negate $ fromIntegral $ slidingWindowSeconds binInterval) to


-- | How far back BEFORE the cache watermark a delta fetch re-reads.
--
-- A delta starting exactly at @cachedTo@ reads every bin once and never revisits
-- it, so a single under-returned read is frozen into the chart permanently — that
-- is what turned TimeFusion's bounded-dedup under-count (2026-08-07) into
-- multi-minute holes users kept seeing long after the rows were readable again.
-- Re-reading a trailing span lets 'mergeTimeseriesData' (last occurrence wins)
-- correct those bins on the next refresh.
--
-- >>> deltaOverlapSeconds "30 seconds"
-- 900
-- >>> deltaOverlapSeconds "5 minutes"
-- 1500
-- >>> deltaOverlapSeconds "1 hour"
-- 3600
deltaOverlapSeconds :: Text -> Int
deltaOverlapSeconds binInterval = min 3600 $ max 900 (parseBinIntervalToSeconds binInterval * 5)


-- | Epoch-aligned start of a fixed-width bucket, including dates before 1970.
--
-- >>> bucketStart "1 hour" (mkTime (-1)) == mkTime (-3600)
-- True
-- >>> bucketStart "1w" (mkTime 700000) == mkTime 604800
-- True
-- >>> bucketStart "700ms" (mkTime 1) == posixSecondsToUTCTime 0.7
-- True
bucketStart :: Text -> UTCTime -> UTCTime
bucketStart interval timestamp =
  let width = intervalWidth interval
      bucket = floor $ toRational (utcTimeToPOSIXSeconds timestamp) / width
   in posixSecondsToUTCTime $ fromRational $ fromInteger bucket * width


nextBucketStart :: Text -> UTCTime -> UTCTime
nextBucketStart interval = addUTCTime (fromRational $ intervalWidth interval) . bucketStart interval


-- | A completed refresh replaces even buckets that are now empty (for example
-- after deletions). Merely merging nonempty rows preserves stale values.
replaceTimeseriesRange :: MetricsData -> MetricsData -> UTCTime -> UTCTime -> MetricsData
replaceTimeseriesRange cached fresh start end =
  mergeTimeseriesData (filterByTimestamp (\ts -> ts < toPosix start || ts >= toPosix end) cached) fresh


-- | Recent buckets first. Grow to six hours of work, or one wider bucket, per
-- query. Adjacent chunks have exclusive upper bounds, so boundary events occur
-- exactly once. The original request keeps its inclusive final endpoint.
--
-- >>> chartChunks "1 hour" (mkTime 17, mkTime 20000) == [(mkTime 14400, mkTime 20000, InclusiveEnd), (mkTime 17, mkTime 14400, ExclusiveEnd)]
-- True
-- >>> chartChunks "1 hour" (mkTime 0, mkTime 0)
-- []
-- >>> let ok iv = let cs = chartChunks iv (mkTime 17, mkTime 864000) in all (\(lo, hi, _) -> (lo == mkTime 17 || bucketStart iv lo == lo) && (hi == mkTime 864000 || bucketStart iv hi == hi)) cs && map (\(lo, _, _) -> lo) (take (length cs - 1) cs) == map (\(_, hi, _) -> hi) (drop 1 cs)
-- >>> all ok (["5h", "7h"] :: [Text])
-- True
chartChunks :: Text -> (UTCTime, UTCTime) -> [(UTCTime, UTCTime, RangeEnd)]
chartChunks interval (start, end) = go 1 end InclusiveEnd
  where
    width = max 1 $ parseBinIntervalToSeconds interval
    go buckets upper endMode
      | upper <= start = []
      | otherwise =
          let lower = max start $ addUTCTime (negate $ fromIntegral $ width * buckets) $ bucketStart interval upper
              next = min (max 1 $ 21600 `div` width) (buckets * 4)
           in (lower, upper, endMode) : go next lower ExclusiveEnd


-- | Generate a cache key from query components.
--
-- >>> let cfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
-- >>> let secs :: Text -> [Section]; secs iv = [SummarizeCommand [] (Just $ SummarizeByClause [ByBinFunc $ Bin (Subject "timestamp" "timestamp" []) iv])]
-- >>> (generateCacheKey defPid Nothing (secs "5m") cfg).queryHash == (generateCacheKey defPid Nothing (secs "5m") cfg).queryHash
-- True
-- >>> (generateCacheKey defPid Nothing (secs "10m") cfg).binInterval
-- "10m"
-- >>> (generateCacheKey defPid Nothing (secs "10m") cfg{environment = Just "production"}).queryHash /= (generateCacheKey defPid Nothing (secs "10m") cfg{environment = Just "staging"}).queryHash
-- True
generateCacheKey :: Projects.ProjectId -> Maybe Sources -> [Section] -> SqlQueryCfg -> CacheKey
generateCacheKey pid sourceM sections sqlCfg =
  -- Scope changes the generated WHERE clause while leaving the user's KQL untouched.
  -- It must therefore participate in cache identity; otherwise a production or
  -- checkout response can be served after switching scope.
  CacheKey
    { projectId = pid
    , source = maybe "spans" toQText sourceM
    , queryHash = toXXHash $ toQText sections <> "\NULenvironment=" <> fromMaybe "" sqlCfg.environment <> "\NULservice=" <> fromMaybe "" sqlCfg.service <> foldMap compiledPredicate sections
    , binInterval = fromMaybe defaultBinSize $ listToMaybe [b | SummarizeCommand _ (Just (SummarizeByClause items)) <- sections, Just b <- map binOf items]
    }
  where
    binOf = \case
      ByBinFunc (Bin _ interval) -> Just interval
      ByBinFunc (BinAuto _) -> Just $ autoBinWidth sqlCfg
      _ -> Nothing
    -- Compiler changes must not reuse old historical results. A fixed clock keeps
    -- now()/ago() fingerprints stable while preserving their JSON-path rendering.
    fingerprint = ("\NULpredicate=" <>) . display . resolveWildcardTimes fixedUTCTime
    compiledPredicate = \case
      Search expr -> fingerprint expr
      WhereClause expr -> fingerprint expr
      HavingClause expr -> fingerprint expr
      _ -> ""


-- | Convert UTCTime to POSIX epoch seconds
toPosix :: UTCTime -> Int
toPosix = floor . utcTimeToPOSIXSeconds


-- | Extract timestamp from first column of a row
rowTimestamp :: V.Vector (Maybe Double) -> Maybe Double
rowTimestamp = join . V.headM


-- | Look up cache entry and determine cache result
lookupCache :: DB es => CacheKey -> (UTCTime, UTCTime) -> Eff es CacheResult
lookupCache key (reqFrom, reqTo) =
  Hasql.interp
    [HI.sql|
      SELECT project_id, source, query_hash, bin_interval, original_query,
             cached_from, cached_to, cached_data, hit_count::bigint
      FROM query_cache
      WHERE project_id = #{key.projectId} AND source = #{key.source} AND query_hash = #{key.queryHash} AND bin_interval = #{key.binInterval}
      LIMIT 1
    |]
    <&> \case
      [] -> CacheMiss
      ((pid, src, qh, bi, oq, cf, ct, AesonText cd, hc) : _) ->
        let entry = CacheEntry pid src qh bi oq cf ct cd hc
         in if
              | isJust cd.error -> CacheBypassed "Cached query failed"
              | reqFrom >= cf && reqTo <= ct -> CacheHit entry
              | reqFrom >= cf && reqTo > ct -> PartialHit entry
              | reqFrom < cf -> CacheBypassed "Request extends before cached range"
              | otherwise -> CacheMiss


-- | Update or insert cache entry (replaces time range and data on conflict)
updateCache :: (DB es, Time :> es) => CacheKey -> (UTCTime, UTCTime) -> MetricsData -> Text -> Eff es ()
updateCache key (fromTime, toTime) metricsData originalQuery = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql|
    INSERT INTO query_cache (project_id, source, query_hash, bin_interval, original_query,
                             cached_from, cached_to, cached_data, hit_count, last_accessed_at)
    VALUES (#{key.projectId}, #{key.source}, #{key.queryHash}, #{key.binInterval}, #{originalQuery}, #{fromTime}, #{toTime}, #{AesonText metricsData}, 1, #{now})
    ON CONFLICT (project_id, source, query_hash, bin_interval)
    DO UPDATE SET
      cached_from = EXCLUDED.cached_from,
      cached_to = EXCLUDED.cached_to,
      cached_data = EXCLUDED.cached_data,
      original_query = EXCLUDED.original_query,
      hit_count = query_cache.hit_count + 1,
      last_accessed_at = EXCLUDED.last_accessed_at,
      updated_at = EXCLUDED.last_accessed_at
  |]


-- | Merge two MetricsData by timestamp, handling different column structures.
-- Empty data is a two-sided identity, merging is idempotent, rows are sorted by
-- timestamp, and overlapping timestamps are right-biased (fresh data wins).
--
-- >>> let a = mkMetrics [(1000, 10), (2000, 20)]
-- >>> (mergeTimeseriesData emptyMetrics a).dataset == a.dataset && (mergeTimeseriesData a emptyMetrics).dataset == a.dataset
-- True
-- >>> (mergeTimeseriesData a a).dataset == a.dataset
-- True
-- >>> let sorted = mergeTimeseriesData (mkMetrics [(1000, 10), (3000, 30)]) (mkMetrics [(2000, 20), (4000, 40)])
-- >>> V.toList $ V.mapMaybe (join . V.headM) sorted.dataset
-- [1000.0,2000.0,3000.0,4000.0]
-- >>> let deduped = mergeTimeseriesData a (mkMetrics [(2000, 25), (3000, 30)])
-- >>> V.toList $ V.mapMaybe (join . V.headM . V.drop 1) deduped.dataset
-- [10.0,25.0,30.0]
mergeTimeseriesData :: MetricsData -> MetricsData -> MetricsData
mergeTimeseriesData cached new
  | V.null cached.dataset = new
  | V.null new.dataset = cached
  | otherwise =
      let
        -- Union headers: keep first (timestamp), then unique non-timestamp headers
        cachedHdrs = V.toList cached.headers
        newHdrs = V.toList new.headers
        mergedHdrs = V.fromList $ take 1 cachedHdrs <> ordNub (drop 1 cachedHdrs <> drop 1 newHdrs)
        -- Build column index maps: header -> position in source headers
        cachedIdx = M.fromList $ zip cachedHdrs [0 ..]
        newIdx = M.fromList $ zip newHdrs [0 ..]
        -- Normalize row to merged header structure
        normalizeRow srcIdx row = V.map (\hdr -> M.lookup hdr srcIdx >>= join . (row V.!?)) mergedHdrs
        sorted = V.modify (VA.sortBy $ comparing rowTimestamp) $ V.map (normalizeRow cachedIdx) cached.dataset <> V.map (normalizeRow newIdx) new.dataset
        -- Rows are sorted, so keeping the last of each timestamp run dedupes (fresh data wins)
        deduped = V.ifilter (\i row -> i == V.length sorted - 1 || rowTimestamp row /= rowTimestamp (sorted V.! (i + 1))) sorted
       in
        (withRows cached deduped)
          { headers = mergedHdrs
          , from = liftA2 min cached.from new.from <|> cached.from <|> new.from
          , to = liftA2 max cached.to new.to <|> cached.to <|> new.to
          }


-- | Replace the dataset and recompute every figure derived from it.
withRows :: MetricsData -> V.Vector (V.Vector (Maybe Double)) -> MetricsData
withRows metrics rows =
  let (rowsCount, rpm, stats) = summarizeSeries rows
   in metrics{dataset = rows, rowsCount, rowsPerMin = Just rpm, stats = Just stats}


-- | Compute @(rowsCount, rowsPerMin, MetricsStats)@ over pivoted timeseries rows
-- (@[timestamp, series1, ...]@).
--
-- Empty datasets and gap-only rows yield zero totals and 'def' stats ('last' is 'Nothing'):
--
-- >>> summarizeSeries V.empty
-- (0.0,0.0,MetricsStats {min = 0.0, max = 0.0, sum = 0.0, count = 0, mean = 0.0, mode = 0.0, maxGroupSum = 0.0, last = Nothing})
-- >>> summarizeSeries (V.fromList [V.fromList [Just 60, Nothing]])
-- (0.0,0.0,MetricsStats {min = 0.0, max = 0.0, sum = 0.0, count = 0, mean = 0.0, mode = 0.0, maxGroupSum = 0.0, last = Nothing})
--
-- A trailing gap row leaves 'last' as 'Nothing' rather than fabricating @Just 0@:
--
-- >>> let (_, _, st) = summarizeSeries (V.fromList [V.fromList [Just 0, Just 5], V.fromList [Just 60, Nothing]])
-- >>> (st.maxGroupSum, st.last)
-- (5.0,Nothing)
--
-- >>> let (tot, rpm, st) = summarizeSeries (V.fromList [V.fromList [Just 0, Just 2, Just 3], V.fromList [Just 60, Just 4, Nothing]])
-- >>> (tot, rpm, st.min, st.max, st.count, st.maxGroupSum, st.last)
-- (9.0,3.0,2.0,4.0,3,5.0,Just 4.0)
summarizeSeries :: V.Vector (V.Vector (Maybe Double)) -> (Double, Double, MetricsStats)
summarizeSeries rows
  | V.null allValues = (0, 0, def)
  | otherwise = (sumV, rate, MetricsStats minV maxV sumV cnt (sumV / fromIntegral cnt) mode maxGroupSum lastRowSum)
  where
    allValues = V.concatMap (V.catMaybes . V.drop 1) rows
    rowSums = V.map (\r -> let vs = V.catMaybes (V.drop 1 r) in V.sum vs <$ guard (not $ V.null vs)) rows
    nonEmptySums = V.catMaybes rowSums
    h = V.head allValues
    maxFreqEntries = 1000 -- cap frequency map to prevent memory issues with high-cardinality data
    (!minV, !maxV, !sumV, !cnt, !freq) =
      V.foldl'
        ( \(!mn, !mx, !s, !c, !f) x ->
            let f' = if M.size f < maxFreqEntries || M.member x f then M.insertWith (+) x 1 f else f
             in (min mn x, max mx x, s + x, c + 1, f')
        )
        (h, h, h, 1, one (h, 1))
        (V.tail allValues)
    maxGroupSum = if V.null nonEmptySums then 0 else V.maximum nonEmptySums
    lastRowSum = snd =<< V.unsnoc rowSums
    -- first (smallest) value wins ties, hence the strict `>`
    mode = fst $ M.foldlWithKey' (\acc@(_, cnt') k c -> if c > cnt' then (k, c) else acc) (h, 0) freq
    timestamps = V.mapMaybe rowTimestamp rows
    rate =
      if V.null timestamps || V.maximum timestamps <= V.minimum timestamps
        then 0
        else fromIntegral cnt * 60 / (V.maximum timestamps - V.minimum timestamps)


-- | Filter dataset rows by timestamp predicate and recalculate stats
filterByTimestamp :: (Int -> Bool) -> MetricsData -> MetricsData
filterByTimestamp p metrics = withRows metrics $ V.filter (maybe False (p . floor) . rowTimestamp) metrics.dataset


-- | Trim data to a specific time range.
--
-- >>> let r = trimToRange (mkMetrics [(1000, 10), (2000, 20), (3000, 30), (4000, 40)]) (mkTime 1500) (mkTime 3500)
-- >>> (V.length r.dataset, r.from, r.to)
-- (2,Just 1500,Just 3500)
-- >>> V.length (trimToRange (mkMetrics [(1000, 10), (2000, 20)]) (mkTime 5000) (mkTime 6000)).dataset
-- 0
trimToRange :: MetricsData -> UTCTime -> UTCTime -> MetricsData
trimToRange metrics fromTime toTime =
  let (fromE, toE) = (toPosix fromTime, toPosix toTime)
   in (filterByTimestamp (\ts -> ts >= fromE && ts <= toE) metrics){from = Just fromE, to = Just toE}


-- | Trim old data outside the sliding window.
--
-- >>> let r = trimOldData (mkTime 2000) (mkMetrics [(1000, 10), (2000, 20), (3000, 30)])
-- >>> (V.length r.dataset, join $ V.headM $ V.head r.dataset)
-- (2,Just 2000.0)
trimOldData :: UTCTime -> MetricsData -> MetricsData
trimOldData windowStart = filterByTimestamp (>= toPosix windowStart)


-- | Cleanup expired cache entries (LRU eviction)
cleanupExpiredCache :: (DB es, Time :> es) => Eff es Int
cleanupExpiredCache = do
  now <- Time.currentTime
  fromMaybe 0
    . viaNonEmpty head
    <$> Hasql.interp
      [HI.sql|
      WITH deleted AS (
        DELETE FROM query_cache
        WHERE last_accessed_at < #{now}::timestamptz - interval '4 hours'
           OR (source = 'raw-sql' AND updated_at < #{now}::timestamptz - interval '10 minutes')
        RETURNING id
      )
      SELECT COUNT(*)::bigint FROM deleted
    |]


-- | Fleet-wide L2 behind a per-replica memory cache (its table keeps the RUM page's historical name): read the shared table, else compute and
-- publish. @rawKey@ is hashed, so callers pass a readable @show@n key. Expired rows are pruned
-- on write, which is rare enough (one per cold key per TTL) to need no cleanup job.
withSharedCache :: (AE.FromJSON a, AE.ToJSON a, DB es) => Text -> Int64 -> Eff es a -> Eff es a
withSharedCache rawKey ttl compute =
  Hasql.interpOne [HI.sql|SELECT payload FROM rum_panel_cache WHERE cache_key = #{key} AND expires_at > now()|] >>= maybe publish (pure . unAesonText . HI.getOneColumn)
  where
    key = sharedCacheKey rawKey
    publish = do
      fresh <- compute
      Hasql.interpExecute_
        [HI.sql|INSERT INTO rum_panel_cache (cache_key, payload, expires_at)
          VALUES (#{key}, #{AesonText fresh}, now() + make_interval(secs => #{ttl}::double precision))
          ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, expires_at = EXCLUDED.expires_at|]
      Hasql.interpExecute_ [HI.sql|DELETE FROM rum_panel_cache WHERE expires_at < now() - interval '12 hours'|]
      pure fresh


-- | The shared cache table is fleet-wide with no project column, so the key is a full SHA-256:
-- a short hash would let a member grind @rawKey@ text into another project's row.
--
-- >>> import Data.Text qualified as T
-- >>> T.length (sharedCacheKey "endpointStats:(a,q)")
-- 64
-- >>> sharedCacheKey "endpointStats:(a,q)" == sharedCacheKey "endpointStats:(b,q)"
-- False
sharedCacheKey :: Text -> Text
sharedCacheKey rawKey = show (hash (encodeUtf8 rawKey :: ByteString) :: Digest SHA256)


-- In-memory cache for propagating session/user identity across all spans in a trace.

data TraceSessionInfo = TraceSessionInfo
  { sessionId :: !(Maybe Text)
  , userId :: !(Maybe Text)
  , userEmail :: !(Maybe Text)
  , userName :: !(Maybe Text)
  , userFullName :: !(Maybe Text)
  , lastAccessedAt :: !UTCTime
  }


-- | Merge: keep first non-Nothing per field, latest access time.
--
-- >>> import Data.Time.Clock (UTCTime(..))
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> let t = UTCTime (fromGregorian 2025 1 1) 0
-- >>> let a = TraceSessionInfo (Just "s1") Nothing Nothing Nothing Nothing t
-- >>> let b = TraceSessionInfo Nothing (Just "u1") Nothing Nothing Nothing t
-- >>> (a <> b).sessionId
-- Just "s1"
-- >>> (a <> b).userId
-- Just "u1"
instance Semigroup TraceSessionInfo where
  a <> b =
    TraceSessionInfo
      { sessionId = a.sessionId <|> b.sessionId
      , userId = a.userId <|> b.userId
      , userEmail = a.userEmail <|> b.userEmail
      , userName = a.userName <|> b.userName
      , userFullName = a.userFullName <|> b.userFullName
      , lastAccessedAt = max a.lastAccessedAt b.lastAccessedAt
      }


type TraceSessionKey = (Text, Text) -- (project_id, trace_id)
type TraceSessionCache = IORef (HM.HashMap TraceSessionKey TraceSessionInfo)


newTraceSessionCache :: IO TraceSessionCache
newTraceSessionCache = newIORef HM.empty


extractSessionInfo :: UTCTime -> OtelLogsAndSpans -> Maybe TraceSessionInfo
extractSessionInfo now span_ =
  let am = unAesonTextMaybe span_.attributes
      sId = atMapText "session.id" am
      uId = atMapText "user.id" am
      uEmail = atMapText "user.email" am
      uName = atMapText "user.name" am
      uFull = atMapText "user.full_name" am
      info = TraceSessionInfo{sessionId = sId, userId = uId, userEmail = uEmail, userName = uName, userFullName = uFull, lastAccessedAt = now}
   in info <$ (sId <|> uId <|> uEmail <|> uName <|> uFull)


traceKey :: OtelLogsAndSpans -> Maybe TraceSessionKey
traceKey span_ = (span_.project_id,) <$> (span_.context >>= (.trace_id))


-- | Insert session/user attributes into the span's attribute Map without overwriting existing keys.
-- Uses nestedJsonFromDotNotation so dotted keys (e.g. "session.id") become nested JSON
-- matching the structure produced by OTLP ingestion, which atMapText expects for lookup.
stampSpan :: TraceSessionInfo -> OtelLogsAndSpans -> OtelLogsAndSpans
stampSpan info span_ =
  let am = fromMaybe M.empty (unAesonTextMaybe span_.attributes)
      pairs =
        catMaybes
          [ ("session.id",) . AE.String <$> info.sessionId
          , ("user.id",) . AE.String <$> info.userId
          , ("user.email",) . AE.String <$> info.userEmail
          , ("user.name",) . AE.String <$> info.userName
          , ("user.full_name",) . AE.String <$> info.userFullName
          ]
      inserts = fromMaybe M.empty $ jsonToMap $ nestedJsonFromDotNotation pairs
      stamped = span_{attributes = Just (AesonText (am `M.union` inserts))}
   in -- Conversion builds the display summary before this trace-wide stamping
      -- pass. Rebuild it from the inherited attributes so collapsed parents
      -- expose the same session/user identity as the child that supplied it.
      stamped{summary = generateSummary stamped}


-- | Two-pass: (1) collect session info from spans, upsert cache; (2) stamp spans missing info.
lookupAndStamp :: (IOE :> es, Time.Time :> es) => TraceSessionCache -> V.Vector OtelLogsAndSpans -> Eff es (V.Vector OtelLogsAndSpans)
lookupAndStamp cache spans = do
  now <- Time.currentTime
  -- Pass 1: collect and upsert
  let infos = V.mapMaybe (\s -> (,) <$> traceKey s <*> extractSessionInfo now s) spans
  unless (V.null infos)
    $ atomicModifyIORef' cache \m ->
      (V.foldl' (\acc (k, v) -> HM.insertWith (<>) k v acc) m infos, ())
  -- Pass 2: stamp from cache
  cached <- readIORef cache
  let stamp s = case traceKey s >>= (`HM.lookup` cached) of
        Nothing -> s
        Just info -> stampSpan info s
  pure $ V.map stamp spans


-- | Evict entries idle > maxIdleSecs, then LRU down to maxEntries cap. Returns eviction count.
evictStaleEntries :: TraceSessionCache -> Int -> Int -> UTCTime -> IO Int
evictStaleEntries cache maxIdleSecs maxEntries now =
  atomicModifyIORef' cache \m ->
    let threshold = fromIntegral maxIdleSecs :: NominalDiffTime
        fresh = HM.filter (\info -> diffUTCTime now info.lastAccessedAt <= threshold) m
        before = HM.size m
     in if HM.size fresh <= maxEntries
          then (fresh, before - HM.size fresh)
          else
            let sorted = sortOn ((.lastAccessedAt) . snd) (HM.toList fresh)
                kept = HM.fromList $ drop (HM.size fresh - maxEntries) sorted
             in (kept, before - HM.size kept)
