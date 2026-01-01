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
) where

import Data.List (groupBy)
import Data.Map.Strict qualified as M
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Only (..))
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Types (MetricsData (..), MetricsStats (..))
import Pkg.DeriveUtils (AesonText (..))
import Pkg.Parser (SqlQueryCfg (..), calculateAutoBinWidth)
import Pkg.Parser.Expr (ToQueryText (..))
import Pkg.Parser.Stats (BinFunction (..), Section (..), Sources (..), SummarizeByClause (..), defaultBinSize)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.DB (DB)
import Utils (toXXHash)


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
  SummarizeCommand _ (Just (SummarizeByClause fields)) -> any isRight fields
  _ -> False


-- | Rewrite bin_auto to fixed bin interval for delta fetches
rewriteBinAutoToFixed :: Text -> [Section] -> [Section]
rewriteBinAutoToFixed interval = map \case
  SummarizeCommand aggs (Just (SummarizeByClause fields)) ->
    SummarizeCommand aggs (Just (SummarizeByClause (map rewriteField fields)))
  s -> s
  where
    rewriteField (Right (BinAuto subj)) = Right (Bin subj interval)
    rewriteField f = f


-- | Extract bin interval from summarize clause
extractBinInterval :: SqlQueryCfg -> [Section] -> Text
extractBinInterval sqlCfg =
  fromMaybe defaultBinSize . asum . map \case
    SummarizeCommand _ (Just (SummarizeByClause fields)) -> asum $ map getBinInterval (rights fields)
    _ -> Nothing
  where
    getBinInterval (Bin _ interval) = Just interval
    getBinInterval (BinAuto _) = Just $ calculateAutoBinWidth sqlCfg.dateRange sqlCfg.currentTime


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


-- | Look up cache entry and determine cache result
lookupCache :: DB es => CacheKey -> (UTCTime, UTCTime) -> Eff es CacheResult
lookupCache key (reqFrom, reqTo) =
  PG.query
    [sql|
      SELECT project_id, source, query_hash, bin_interval, original_query,
             cached_from, cached_to, cached_data, hit_count
      FROM query_cache
      WHERE project_id = ? AND source = ? AND query_hash = ? AND bin_interval = ?
      LIMIT 1
    |]
    (key.projectId, key.source, key.queryHash, key.binInterval)
    <&> \case
      [] -> CacheMiss
      ((pid, src, qh, bi, oq, cf, ct, AesonText cd, hc) : _) ->
        let entry = CacheEntry pid src qh bi oq cf ct cd hc
         in case () of
              _
                | reqFrom >= cf && reqTo <= ct -> CacheHit entry
                | reqFrom >= cf && reqTo > ct -> PartialHit entry
                | reqFrom < cf -> CacheBypassed "Request extends before cached range"
                | otherwise -> CacheMiss


-- | Update or insert cache entry (merges time ranges on conflict)
updateCache :: DB es => CacheKey -> (UTCTime, UTCTime) -> MetricsData -> Text -> Eff es ()
updateCache key (fromTime, toTime) metricsData originalQuery =
  void
    $ PG.execute
      [sql|
    INSERT INTO query_cache (project_id, source, query_hash, bin_interval, original_query,
                             cached_from, cached_to, cached_data, hit_count, last_accessed_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1, now())
    ON CONFLICT (project_id, source, query_hash, bin_interval)
    DO UPDATE SET
      cached_from = LEAST(query_cache.cached_from, EXCLUDED.cached_from),
      cached_to = GREATEST(query_cache.cached_to, EXCLUDED.cached_to),
      cached_data = EXCLUDED.cached_data,
      original_query = EXCLUDED.original_query,
      hit_count = query_cache.hit_count + 1,
      last_accessed_at = now(),
      updated_at = now()
  |]
      (key.projectId, key.source, key.queryHash, key.binInterval, originalQuery, fromTime, toTime, AesonText metricsData)


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
        -- Sort and deduplicate by timestamp
        sorted = V.modify (VA.sortBy $ comparing (join . V.headM)) $ normalizedCached <> normalizedNew
        dedupedRows = V.fromList . map Unsafe.last . groupBy ((==) `on` (join . V.headM)) $ V.toList sorted
       in
        cached
          { dataset = dedupedRows
          , headers = mergedHdrs
          , rowsCount = fromIntegral $ V.length dedupedRows
          , from = liftA2 min cached.from new.from <|> cached.from <|> new.from
          , to = liftA2 max cached.to new.to <|> cached.to <|> new.to
          , stats = Just $ recalculateStats dedupedRows
          }


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
      then MetricsStats 0 0 0 0 0 0 0
      else
        let h = V.head allValues
            -- Calculate min, max, sum, count and mode frequency in single pass
            (!minV, !maxV, !sumV, !cnt, !freq) = V.foldl' (\(!mn, !mx, !s, !c, !f) x -> (min mn x, max mx x, s + x, c + 1, M.insertWith (+) x 1 f)) (h, h, h, 1, M.singleton h 1) (V.tail allValues)
            maxGroupSum = if V.null rowSums then 0 else V.maximum rowSums
            mode = fst $ M.foldlWithKey' (\acc@(_, cnt') k c -> if c > cnt' then (k, c) else acc) (h, 0) freq
         in MetricsStats minV maxV sumV cnt (sumV / fromIntegral cnt) mode maxGroupSum


-- | Filter dataset rows by timestamp predicate and recalculate stats
filterByTimestamp :: (Int -> Bool) -> MetricsData -> MetricsData
filterByTimestamp p metrics =
  let filtered = V.filter (\row -> maybe False (p . floor) (join $ V.headM row)) metrics.dataset
   in metrics{dataset = filtered, rowsCount = fromIntegral $ V.length filtered, stats = Just $ recalculateStats filtered}


-- | Trim data to a specific time range
trimToRange :: MetricsData -> UTCTime -> UTCTime -> MetricsData
trimToRange metrics fromTime toTime =
  let (fromE, toE) = (toPosix fromTime, toPosix toTime)
   in (filterByTimestamp (\ts -> ts >= fromE && ts <= toE) metrics){from = Just fromE, to = Just toE}


-- | Trim old data outside the sliding window
trimOldData :: UTCTime -> MetricsData -> MetricsData
trimOldData windowStart = filterByTimestamp (>= toPosix windowStart)


-- | Cleanup expired cache entries (LRU eviction)
cleanupExpiredCache :: DB es => Eff es Int
cleanupExpiredCache =
  maybe 0 fromOnly
    . viaNonEmpty head
    <$> PG.query
      [sql|
      WITH deleted AS (
        DELETE FROM query_cache
        WHERE last_accessed_at < now() - interval '4 hours'
        RETURNING id
      )
      SELECT COUNT(*)::int FROM deleted
    |]
      ()
