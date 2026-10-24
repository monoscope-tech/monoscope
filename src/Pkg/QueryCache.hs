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
  cachedRawQuery,
  lookupRawCache,
  updateRawCache,
  generateCacheKey,
  lookupCache,
  updateCache,
  mergeTimeseriesData,
  trimToRange,
  trimOldData,
  hasSummarizeWithBin,
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
) where

import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as E
import Control.Exception.Annotated qualified as Ann
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Map.Strict qualified as M
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as V

-- Merge, not Intro: the sort MUST be stable. 'mergeTimeseriesData' dedupes keeping the last
-- occurrence, and the delta is appended after the cached rows, so stability is
-- what makes "fresh data wins on an overlapping bin" true rather than arbitrary.
import Data.Vector.Algorithms.Merge qualified as VA
import Effectful (Eff, IOE, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Clock (getMonotonicTime)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import OpenTelemetry.Attributes qualified as OA
import Pages.Charts.Types (DataType, MetricsData (..), MetricsStats (..))
import Pkg.DeriveUtils (AesonText (..), DB, unAesonText)
import Pkg.Metrics qualified as Metrics
import Pkg.Parser (RangeEnd (..), SqlQueryCfg (..), autoBinWidth, fixedUTCTime, intervalSeconds)
import Pkg.Parser.Expr (ToQueryText (..), display, kqlTimespanToTimeBucket, resolveWildcardTimes)
import Pkg.Parser.Stats (BinFunction (..), ByClauseItem (..), Section (..), Sources (..), SummarizeByClause (..), defaultBinSize)
import Relude
import UnliftIO (withRunInIO)
import UnliftIO.Exception (onException)
import Utils (encodeText, toXXHash)


-- $setup
-- >>> import Data.Time (UTCTime (..))
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> :set -XOverloadedStrings


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


-- | Check if query has a summarize command with time binning (bin or bin_auto)
hasSummarizeWithBin :: [Section] -> Bool
hasSummarizeWithBin = any \case
  SummarizeCommand _ (Just (SummarizeByClause items)) -> any (\case ByBinFunc _ -> True; _ -> False) items
  _ -> False


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


-- | Generate a cache key from query components
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


-- | Merge two MetricsData by timestamp, handling different column structures
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
withRows metrics rows = metrics{dataset = rows, rowsCount = sum $ V.concatMap (V.catMaybes . V.drop 1) rows, rowsPerMin = Just $ timeseriesRate rows, stats = Just $ recalculateStats rows}


-- | Recalculate stats from dataset - processes all value columns and calculates maxGroupSum
recalculateStats :: V.Vector (V.Vector (Maybe Double)) -> MetricsStats
recalculateStats rows =
  let
    -- Extract all non-null values from value columns (skip timestamp at index 0)
    allValues = V.concatMap (V.catMaybes . V.drop 1) rows
    rowSums = V.map (sum . V.catMaybes . V.drop 1) rows
   in
    if V.null allValues
      then def
      else
        let h = V.head allValues
            maxFreqEntries = 1000 -- cap frequency map to prevent memory issues with high-cardinality data
            -- Calculate min, max, sum, count and mode frequency in single pass (capped freq map)
            (!minV, !maxV, !sumV, !cnt, !freq) =
              V.foldl'
                ( \(!mn, !mx, !s, !c, !f) x ->
                    let f' = if M.size f < maxFreqEntries || M.member x f then M.insertWith (+) x 1 f else f
                     in (min mn x, max mx x, s + x, c + 1, f')
                )
                (h, h, h, 1, one (h, 1))
                (V.tail allValues)
            maxGroupSum = if V.null rowSums then 0 else V.maximum rowSums
            mode = fst $ M.foldlWithKey' (\acc@(_, cnt') k c -> if c > cnt' then (k, c) else acc) (h, 0) freq
         in MetricsStats minV maxV sumV cnt (sumV / fromIntegral cnt) mode maxGroupSum (snd <$> V.unsnoc rowSums)


timeseriesRate :: V.Vector (V.Vector (Maybe Double)) -> Double
timeseriesRate rows =
  let timestamps = V.mapMaybe rowTimestamp rows
      count = V.length $ V.concatMap (V.catMaybes . V.drop 1) rows
   in if V.null timestamps || V.maximum timestamps <= V.minimum timestamps
        then 0
        else fromIntegral count * 60 / (V.maximum timestamps - V.minimum timestamps)


-- | Filter dataset rows by timestamp predicate and recalculate stats
filterByTimestamp :: (Int -> Bool) -> MetricsData -> MetricsData
filterByTimestamp p metrics = withRows metrics $ V.filter (maybe False (p . floor) . rowTimestamp) metrics.dataset


-- | Trim data to a specific time range
trimToRange :: MetricsData -> UTCTime -> UTCTime -> MetricsData
trimToRange metrics fromTime toTime =
  let (fromE, toE) = (toPosix fromTime, toPosix toTime)
   in (filterByTimestamp (\ts -> ts >= fromE && ts <= toE) metrics){from = Just fromE, to = Just toE}


-- | Trim old data outside the sliding window
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
    key = toXXHash rawKey
    publish = do
      fresh <- compute
      let payload = AesonText fresh
      Hasql.interpExecute_
        [HI.sql|INSERT INTO rum_panel_cache (cache_key, payload, expires_at)
          VALUES (#{key}, #{payload}, now() + make_interval(secs => #{ttl}::double precision))
          ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, expires_at = EXCLUDED.expires_at|]
      Hasql.interpExecute_ [HI.sql|DELETE FROM rum_panel_cache WHERE expires_at < now() - interval '12 hours'|]
      pure fresh
