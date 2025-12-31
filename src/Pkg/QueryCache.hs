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
) where

import Data.Digest.XXHash (xxHash)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Types (MetricsData (..), MetricsStats (..))
import Pkg.DeriveUtils (AesonText (..))
import Pkg.Parser (SqlQueryCfg (..), calculateAutoBinWidth)
import Pkg.Parser.Expr (ToQueryText (..))
import Pkg.Parser.Stats (BinFunction (..), Section (..), Sources (..), SummarizeByClause (..), defaultBinSize)
import Relude
import System.DB (DB)


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
    , queryHash = show $ xxHash $ encodeUtf8 $ toQText sections
    , binInterval = extractBinInterval sqlCfg sections
    }


-- | Look up cache entry and determine cache result
lookupCache :: DB es => CacheKey -> (UTCTime, UTCTime) -> Eff es CacheResult
lookupCache key (reqFrom, reqTo) = do
  results <-
    PG.query
      [sql|
      SELECT project_id, source, query_hash, bin_interval, original_query,
             cached_from, cached_to, cached_data, hit_count
      FROM query_cache
      WHERE project_id = ? AND source = ? AND query_hash = ? AND bin_interval = ?
    |]
      (key.projectId, key.source, key.queryHash, key.binInterval)
  pure $ case results of
    [] -> CacheMiss
    ((pid, src, qh, bi, oq, cf, ct, AesonText cd, hc) : _) ->
      let entry = CacheEntry pid src qh bi oq cf ct cd hc
       in if
            | reqFrom >= cf && reqTo <= ct -> CacheHit entry
            | reqTo > ct && reqFrom >= cf -> PartialHit entry
            | reqFrom < cf -> CacheBypassed "Request extends before cached range"
            | otherwise -> CacheMiss


-- | Update or insert cache entry
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
      cached_from = EXCLUDED.cached_from,
      cached_to = EXCLUDED.cached_to,
      cached_data = EXCLUDED.cached_data,
      original_query = EXCLUDED.original_query,
      hit_count = query_cache.hit_count + 1,
      last_accessed_at = now(),
      updated_at = now()
  |]
      (key.projectId, key.source, key.queryHash, key.binInterval, originalQuery, fromTime, toTime, AesonText metricsData)


-- | Merge two MetricsData by timestamp, deduplicating by first column
mergeTimeseriesData :: MetricsData -> MetricsData -> MetricsData
mergeTimeseriesData cached new
  | V.null cached.dataset = new
  | V.null new.dataset = cached
  | otherwise =
      let dedupedRows = deduplicateByTimestamp $ V.modify (VA.sortBy cmpTs) $ cached.dataset <> new.dataset
       in cached
            { dataset = dedupedRows
            , rowsCount = fromIntegral $ V.length dedupedRows
            , from = min <$> cached.from <*> new.from <|> cached.from <|> new.from
            , to = max <$> cached.to <*> new.to <|> cached.to <|> new.to
            , stats = Just $ recalculateStats dedupedRows
            }
  where
    cmpTs a b = compare (join $ V.headM a) (join $ V.headM b)
    deduplicateByTimestamp rows
      | V.null rows = rows
      | otherwise = V.fromList $ foldr dedupe [] (V.toList rows)
    dedupe x [] = [x]
    dedupe x (y : ys)
      | join (V.headM x) == join (V.headM y) = y : ys
      | otherwise = x : y : ys


-- | Recalculate stats from dataset (takes second column as values)
recalculateStats :: V.Vector (V.Vector (Maybe Double)) -> MetricsStats
recalculateStats rows =
  let values = V.mapMaybe (\r -> V.headM (V.drop 1 r) >>= id) rows
   in if V.null values
        then MetricsStats 0 0 0 0 0 0 0
        else MetricsStats (V.minimum values) (V.maximum values) (V.sum values) (V.length values) (V.sum values / fromIntegral (V.length values)) (V.head values) (V.maximum values)


-- | Filter dataset rows by timestamp predicate
filterByTimestamp :: (Int -> Bool) -> MetricsData -> MetricsData
filterByTimestamp p metrics =
  let filtered = V.filter (\row -> maybe False (p . floor) (join $ V.headM row)) metrics.dataset
   in metrics{dataset = filtered, rowsCount = fromIntegral $ V.length filtered}


-- | Trim data to a specific time range
trimToRange :: MetricsData -> UTCTime -> UTCTime -> MetricsData
trimToRange metrics fromTime toTime =
  let fromE = floor $ utcTimeToPOSIXSeconds fromTime
      toE = floor $ utcTimeToPOSIXSeconds toTime
   in (filterByTimestamp (\ts -> ts >= fromE && ts <= toE) metrics){from = Just fromE, to = Just toE}


-- | Trim old data outside the sliding window
trimOldData :: UTCTime -> MetricsData -> MetricsData
trimOldData windowStart = filterByTimestamp (>= floor (utcTimeToPOSIXSeconds windowStart))
