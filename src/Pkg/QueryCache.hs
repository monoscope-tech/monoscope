module Pkg.QueryCache (
  CacheKey (..),
  CacheEntry (..),
  CacheResult (..),
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

import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as V

-- Merge, not Intro: the sort MUST be stable. 'dedupeByTimestamp' keeps the last
-- occurrence, and the delta is appended after the cached rows, so stability is
-- what makes "fresh data wins on an overlapping bin" true rather than arbitrary.
import Data.Vector.Algorithms.Merge qualified as VA
import Effectful (Eff, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Types (MetricsData (..), MetricsStats (..))
import Pkg.DeriveUtils (AesonText (..), DB)
import Pkg.Parser (RangeEnd (..), SqlQueryCfg (..), autoBinWidth)
import Pkg.Parser.Expr (ToQueryText (..), kqlTimespanToTimeBucket)
import Pkg.Parser.Stats (BinFunction (..), ByClauseItem (..), Section (..), Sources (..), SummarizeByClause (..), defaultBinSize)
import Relude
import Utils (toXXHash)


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


-- | Check if query has a summarize command with time binning (bin or bin_auto)
hasSummarizeWithBin :: [Section] -> Bool
hasSummarizeWithBin = any \case
  SummarizeCommand _ (Just (SummarizeByClause items)) -> any isBinFunc items
  _ -> False
  where
    isBinFunc (ByBinFunc _) = True
    isBinFunc _ = False


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
intervalWidth interval = fromMaybe 60 $ do
  canonical <- kqlTimespanToTimeBucket interval
  case words canonical of
    [n, unit] -> do
      count <- readMaybe @Integer $ toString n
      multiplier <-
        lookup
          (T.dropWhileEnd (== 's') unit)
          [("second", 1), ("minute", 60), ("hour", 3600), ("day", 86400), ("week", 604800), ("millisecond", 1 / 1000), ("microsecond", 1 / 1000000), ("nanosecond", 1 / 1000000000)]
      guard (count > 0)
      pure $ fromInteger count * multiplier
    _ -> Nothing


-- Chart timestamps are whole seconds. Keep subsecond bins on the existing
-- single-query path rather than merge distinct groups with the same timestamp.
chunkIntervalSupported :: Text -> Bool
chunkIntervalSupported interval =
  let width = intervalWidth interval
   in width >= 1 && width == fromInteger (floor width)


-- | Calculate sliding window size in seconds based on bin interval
-- Keeps at least 48 data points or 24 hours, whichever is larger
slidingWindowSeconds :: Text -> Int
slidingWindowSeconds binInterval =
  let binSecs = parseBinIntervalToSeconds binInterval
      minPoints = 48
   in max 86400 (binSecs * minPoints)


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


-- | Extract bin interval from summarize clause
extractBinInterval :: SqlQueryCfg -> [Section] -> Text
extractBinInterval sqlCfg =
  fromMaybe defaultBinSize . asum . map \case
    SummarizeCommand _ (Just (SummarizeByClause items)) -> asum $ map getBinInterval items
    _ -> Nothing
  where
    getBinInterval (ByBinFunc (Bin _ interval)) = Just interval
    getBinInterval (ByBinFunc (BinAuto _)) = Just $ autoBinWidth sqlCfg
    getBinInterval _ = Nothing


-- | Generate a cache key from query components
generateCacheKey :: Projects.ProjectId -> Maybe Sources -> [Section] -> SqlQueryCfg -> CacheKey
generateCacheKey pid sourceM sections sqlCfg =
  CacheKey
    { projectId = pid
    , source = maybe "spans" toQText sourceM
    , queryHash = toXXHash $ toQText sections
    , binInterval = extractBinInterval sqlCfg sections
    }


-- | Convert UTCTime to POSIX epoch seconds
toPosix :: UTCTime -> Int
toPosix = floor . utcTimeToPOSIXSeconds


-- | Extract timestamp from first column of a row
rowTimestamp :: V.Vector (Maybe Double) -> Maybe Double
rowTimestamp = join . V.headM


-- | Look up cache entry and determine cache result
lookupCache :: DB es => CacheKey -> (UTCTime, UTCTime) -> Eff es CacheResult
lookupCache key (reqFrom, reqTo) = do
  let (kPid, kSrc, kHash, kBin) = (key.projectId, key.source, key.queryHash, key.binInterval)
  Hasql.interp
    [HI.sql|
      SELECT project_id, source, query_hash, bin_interval, original_query,
             cached_from, cached_to, cached_data, hit_count::bigint
      FROM query_cache
      WHERE project_id = #{kPid} AND source = #{kSrc} AND query_hash = #{kHash} AND bin_interval = #{kBin}
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
  let (kPid, kSrc, kHash, kBin) = (key.projectId, key.source, key.queryHash, key.binInterval)
      cd = AesonText metricsData
  Hasql.interpExecute_
    [HI.sql|
    INSERT INTO query_cache (project_id, source, query_hash, bin_interval, original_query,
                             cached_from, cached_to, cached_data, hit_count, last_accessed_at)
    VALUES (#{kPid}, #{kSrc}, #{kHash}, #{kBin}, #{originalQuery}, #{fromTime}, #{toTime}, #{cd}, 1, #{now})
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
        normalizeRow srcIdx row = V.generate (V.length mergedHdrs) \i ->
          let hdr = mergedHdrs V.! i
           in M.lookup hdr srcIdx >>= (row V.!?) & join
        normalizedCached = V.map (normalizeRow cachedIdx) cached.dataset
        normalizedNew = V.map (normalizeRow newIdx) new.dataset
        -- Sort and deduplicate by timestamp (stay in vector operations)
        sorted = V.modify (VA.sortBy $ comparing (join . V.headM)) $ normalizedCached <> normalizedNew
        dedupedRows = dedupeByTimestamp sorted
       in
        cached
          { dataset = dedupedRows
          , headers = mergedHdrs
          , rowsCount = sum $ V.concatMap (V.mapMaybe id . V.drop 1) dedupedRows
          , rowsPerMin = Just $ timeseriesRate dedupedRows
          , from = liftA2 min cached.from new.from <|> cached.from <|> new.from
          , to = liftA2 max cached.to new.to <|> cached.to <|> new.to
          , stats = Just $ recalculateStats dedupedRows
          }


-- | Deduplicate sorted rows by timestamp, keeping last occurrence (stays in vector operations)
dedupeByTimestamp :: V.Vector (V.Vector (Maybe Double)) -> V.Vector (V.Vector (Maybe Double))
dedupeByTimestamp rows
  | V.null rows = V.empty
  | otherwise =
      let n = V.length rows
          shouldKeep i = i == n - 1 || rowTimestamp (rows V.! i) /= rowTimestamp (rows V.! (i + 1))
       in V.ifilter (\i _ -> shouldKeep i) rows


-- | Recalculate stats from dataset - processes all value columns and calculates maxGroupSum
recalculateStats :: V.Vector (V.Vector (Maybe Double)) -> MetricsStats
recalculateStats rows =
  let
    -- Extract all non-null values from value columns (skip timestamp at index 0)
    allValues = V.concatMap (V.mapMaybe id . V.drop 1) rows
    -- Sum of values per row (for maxGroupSum calculation)
    rowSums = V.map (sum . V.mapMaybe id . V.drop 1) rows
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
         in MetricsStats minV maxV sumV cnt (sumV / fromIntegral cnt) mode maxGroupSum


timeseriesRate :: V.Vector (V.Vector (Maybe Double)) -> Double
timeseriesRate rows =
  let timestamps = V.mapMaybe rowTimestamp rows
      count = V.length $ V.concatMap (V.mapMaybe id . V.drop 1) rows
   in if V.null timestamps || V.maximum timestamps <= V.minimum timestamps
        then 0
        else fromIntegral count * 60 / (V.maximum timestamps - V.minimum timestamps)


-- | Filter dataset rows by timestamp predicate and recalculate stats
filterByTimestamp :: (Int -> Bool) -> MetricsData -> MetricsData
filterByTimestamp p metrics =
  let filtered = V.filter (maybe False (p . floor) . rowTimestamp) metrics.dataset
   in metrics{dataset = filtered, rowsCount = sum $ V.concatMap (V.mapMaybe id . V.drop 1) filtered, rowsPerMin = Just $ timeseriesRate filtered, stats = Just $ recalculateStats filtered}


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
        RETURNING id
      )
      SELECT COUNT(*)::bigint FROM deleted
    |]
