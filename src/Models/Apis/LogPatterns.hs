module Models.Apis.LogPatterns (
  BaselineState (..),
  LogPattern (..),
  LogPatternId,
  LogPatternState (..),
  getLogPatterns,
  getLogPatternTexts,
  getLogPatternTextsByService,
  getLogPatternByHash,
  getNewLogPatterns,
  acknowledgeLogPatterns,
  acknowledgeMergedPatterns,
  UpsertPattern (..),
  upsertLogPattern,
  upsertLogPatternBatch,
  updateBaselineBatch,
  -- Hourly stats
  upsertHourlyStat,
  upsertHourlyStatBatch,
  BatchPatternStats (..),
  getBatchPatternStats,
  -- Pattern with current rate for spike detection
  LogPatternWithRate (..),
  getPatternsWithCurrentRates,
  getLogPatternsByIds,
  -- Field labels
  knownPatternFields,
  sourceFieldLabel,
  -- Volume check
  getTotalEventCount,
  -- Pruning
  pruneStalePatterns,
  pruneOldHourlyStats,
  autoAcknowledgeStaleNewPatterns,
)
where

import Control.Lens (view, _1, _2, _3, _4, _5, _6)
import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (BaselineState (..), DB, WrappedEnumSC (..))
import Relude hiding (id)
import Utils (truncateHour)


newtype LogPatternId = LogPatternId {unLogPatternId :: Int64}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, HI.DecodeValue, HI.EncodeValue, NFData, Ord, ToField)
  deriving anyclass (HI.DecodeRow)


data LogPatternState
  = LPSNew
  | LPSAcknowledged
  | LPSIgnored
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "LPS" LogPatternState


data LogPattern = LogPattern
  { id :: LogPatternId
  , projectId :: Projects.ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , logPattern :: Text
  , patternHash :: Text
  , sourceField :: Text
  , serviceName :: Maybe Text
  , logLevel :: Maybe Text
  , sampleMessage :: Maybe Text
  , traceId :: Maybe Text
  , state :: LogPatternState
  , firstSeenAt :: ZonedTime
  , lastSeenAt :: ZonedTime
  , occurrenceCount :: Int64
  , acknowledgedBy :: Maybe Projects.UserId
  , acknowledgedAt :: Maybe ZonedTime
  , baselineState :: BaselineState
  , baselineVolumeHourlyMean :: Maybe Double
  , baselineVolumeHourlyMad :: Maybe Double
  , baselineSamples :: Int
  , baselineUpdatedAt :: Maybe ZonedTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "log_patterns", PrimaryKey "id", FieldModifiers '[CamelToSnake]] LogPattern)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LogPattern


data UpsertPattern = UpsertPattern
  { projectId :: Projects.ProjectId
  , logPattern :: Text
  , hash :: Text
  , sourceField :: Text
  , serviceName :: Maybe Text
  , logLevel :: Maybe Text
  , traceId :: Maybe Text
  , sampleMessage :: Maybe Text
  , eventCount :: Int64
  }
  deriving stock (Generic)
  deriving anyclass (HI.EncodeRow, ToRow)


-- | Get all log patterns for a project (excludes merged patterns)
getLogPatterns :: DB es => Projects.ProjectId -> Int -> Int -> Eff es [LogPattern]
getLogPatterns pid limit offset = Hasql.interp [HI.sql| SELECT * FROM apis.log_patterns WHERE project_id = #{pid} AND canonical_id IS NULL ORDER BY last_seen_at DESC LIMIT #{limit} OFFSET #{offset} |]


-- | All pattern templates for a source field, used to seed Drain trees.
-- Capped at 5000: the DrainTree's maxLogGroups=1000, so loading more is wasteful.
-- ORDER BY last_seen_at DESC keeps the most recently active patterns as seeds.
getLogPatternTexts :: DB es => Projects.ProjectId -> Text -> Eff es [Text]
getLogPatternTexts pid sourceField = Hasql.interp [HI.sql| SELECT LEFT(log_pattern, 2000) FROM apis.log_patterns WHERE project_id = #{pid} AND source_field = #{sourceField} AND canonical_id IS NULL ORDER BY last_seen_at DESC NULLS LAST LIMIT 5000|]


-- | Per-service variant with MAX(last_seen_at) for change detection.
getLogPatternTextsByService :: DB es => Projects.ProjectId -> Text -> Text -> Eff es ([Text], Maybe UTCTime)
getLogPatternTextsByService pid sourceField svcName = do
  rows :: [(Text, Maybe UTCTime)] <- Hasql.interp [HI.sql| SELECT LEFT(log_pattern, 2000), last_seen_at FROM apis.log_patterns WHERE project_id = #{pid} AND source_field = #{sourceField} AND service_name = #{svcName} AND canonical_id IS NULL ORDER BY last_seen_at DESC NULLS LAST LIMIT 5000|]
  let texts = map (view _1) rows
      maxSeen = viaNonEmpty head [t | (_, Just t) <- rows]
  pure (texts, maxSeen)


-- | Get log pattern by unique key (project_id, source_field, pattern_hash)
getLogPatternByHash :: DB es => Projects.ProjectId -> Text -> Text -> Eff es (Maybe LogPattern)
getLogPatternByHash pid sourceField patHash = Hasql.interpOne [HI.sql| SELECT * FROM apis.log_patterns WHERE project_id = #{pid} AND source_field = #{sourceField} AND pattern_hash = #{patHash} |]


-- | Get new (unprocessed) log patterns for a project, for batch issue creation.
-- Skips patterns already merged into a canonical and those younger than 10 minutes
-- (gives the 5-min merge pass at least one full cycle before creating issues).
getNewLogPatterns :: (DB es, Time :> es) => Projects.ProjectId -> Int -> Eff es [LogPattern]
getNewLogPatterns pid limit = do
  now <- Time.currentTime
  Hasql.interp [HI.sql| SELECT * FROM apis.log_patterns WHERE project_id = #{pid} AND state = #{LPSNew} AND canonical_id IS NULL AND created_at < #{now}::timestamptz - INTERVAL '10 minutes' ORDER BY created_at ASC LIMIT #{limit} |]


-- | Acknowledge log patterns. Pass Nothing for system-triggered acknowledgments.
-- Matches on (project_id, source_field, pattern_hash) — the unique key — to avoid
-- cross-field collisions (e.g. url_path and exception sharing the same normalized hash).
acknowledgeLogPatterns :: (DB es, Time :> es) => Projects.ProjectId -> Maybe Projects.UserId -> V.Vector (Text, Text) -> Eff es Int64
acknowledgeLogPatterns pid uid fieldHashPairs
  | V.null fieldHashPairs = pure 0
  | otherwise = do
      now <- Time.currentTime
      let (fields, hashes) = V.unzip fieldHashPairs
      Hasql.interpExecute
        [HI.sql|
        UPDATE apis.log_patterns
        SET state = #{LPSAcknowledged}, acknowledged_by = #{uid}, acknowledged_at = #{now}
        WHERE project_id = #{pid} AND (source_field, pattern_hash) IN (SELECT unnest(#{fields}::text[]), unnest(#{hashes}::text[]))
      |]


-- | Acknowledge LPSNew patterns that have been merged (canonical_id set).
-- No issues needed since their canonical pattern represents them.
acknowledgeMergedPatterns :: (DB es, Time :> es) => Projects.ProjectId -> Eff es Int64
acknowledgeMergedPatterns pid = do
  now <- Time.currentTime
  Hasql.interpExecute
    [HI.sql|UPDATE apis.log_patterns SET state = #{LPSAcknowledged}, acknowledged_at = #{now}
    WHERE project_id = #{pid} AND state = #{LPSNew} AND canonical_id IS NOT NULL|]


upsertLogPattern :: (DB es, Time :> es) => UpsertPattern -> Eff es Int64
upsertLogPattern = upsertLogPatternBatch . pure


-- | Bulk update baselines for multiple patterns in a single query.
-- Matches on (project_id, source_field, pattern_hash) since pattern_hash alone is not unique across source fields.
updateBaselineBatch :: (DB es, Time :> es) => Projects.ProjectId -> V.Vector (Text, Text, BaselineState, Double, Double, Int) -> Eff es Int64
updateBaselineBatch pid rows
  | V.null rows = pure 0
  | otherwise = do
      now <- Time.currentTime
      let srcFields = V.map (view _1) rows
          hashes = V.map (view _2) rows
          states = V.map (view _3) rows
          means = V.map (view _4) rows
          mads = V.map (view _5) rows
          samples = V.map (view _6) rows
      Hasql.interpExecute
        [HI.sql|
              UPDATE apis.log_patterns lp
              SET baseline_state = v.state,
                  baseline_volume_hourly_mean = v.mean,
                  baseline_volume_hourly_mad = v.mad,
                  baseline_samples = v.samples,
                  baseline_updated_at = #{now}
              FROM (SELECT unnest(#{srcFields}::text[]) AS source_field, unnest(#{hashes}::text[]) AS hash, unnest(#{states}::text[]) AS state, unnest(#{means}::float8[]) AS mean, unnest(#{mads}::float8[]) AS mad, unnest(#{samples}::int[]) AS samples) v
              WHERE lp.project_id = #{pid} AND lp.source_field = v.source_field AND lp.pattern_hash = v.hash
            |]


-- | Batch version of upsertLogPattern using executeMany.
-- Pre-merges rows with the same (projectId, sourceField, hash) to avoid
-- "ON CONFLICT DO UPDATE command cannot affect row a second time".
upsertLogPatternBatch :: (DB es, Time :> es) => [UpsertPattern] -> Eff es Int64
upsertLogPatternBatch [] = pure 0
upsertLogPatternBatch ups = do
  now <- Time.currentTime
  sum
    <$> forM
      (dedup ups)
      ( \u' -> do
          let (u :: UpsertPattern) = cap u'
          let (uPid, uPat, uHash, uSrc, uSvc, uLvl, uTid, uMsg, uCnt) =
                (u.projectId, u.logPattern, u.hash, u.sourceField, u.serviceName, u.logLevel, u.traceId, u.sampleMessage, u.eventCount)
          Hasql.interpExecute
            [HI.sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, source_field, service_name, log_level, trace_id, sample_message, occurrence_count, last_seen_at)
        VALUES (#{uPid}, #{uPat}, #{uHash}, #{uSrc}, #{uSvc}, #{uLvl}, #{uTid}, #{uMsg}, #{uCnt}, #{now})
        ON CONFLICT (project_id, source_field, pattern_hash) DO UPDATE SET
          last_seen_at = EXCLUDED.last_seen_at,
          occurrence_count = apis.log_patterns.occurrence_count + EXCLUDED.occurrence_count,
          sample_message = COALESCE(EXCLUDED.sample_message, apis.log_patterns.sample_message),
          service_name = COALESCE(EXCLUDED.service_name, apis.log_patterns.service_name),
          log_level = COALESCE(EXCLUDED.log_level, apis.log_patterns.log_level),
          trace_id = COALESCE(EXCLUDED.trace_id, apis.log_patterns.trace_id)
  |]
      )
  where
    maxTextLen = 32000
    cap u = u{logPattern = T.take maxTextLen u.logPattern, sampleMessage = T.take maxTextLen <$> u.sampleMessage} :: UpsertPattern
    dedup = Map.elems . foldl' merge Map.empty
    merge acc u = Map.insertWith mergeUp (u.projectId, u.sourceField, u.hash) u acc
    mergeUp new old = old{eventCount = old.eventCount + new.eventCount, sampleMessage = old.sampleMessage <|> new.sampleMessage, serviceName = old.serviceName <|> new.serviceName, logLevel = old.logLevel <|> new.logLevel, traceId = old.traceId <|> new.traceId}


-- | Upsert hourly event count for a pattern into the pre-aggregated stats table.
-- Accumulates counts across multiple 5-min extraction runs within the same hour.
-- Note: SUM semantics mean job retries double-count. This is acceptable because
-- retries are rare and the median/MAD baseline is robust to occasional outliers.
upsertHourlyStat :: DB es => Projects.ProjectId -> Text -> Text -> UTCTime -> Int64 -> Eff es Int64
upsertHourlyStat pid sourceField patHash hourBucket count =
  upsertHourlyStatBatch [(pid, sourceField, patHash, hourBucket, count)]


-- | Batch version of upsertHourlyStat using executeMany.
upsertHourlyStatBatch :: DB es => [(Projects.ProjectId, Text, Text, UTCTime, Int64)] -> Eff es Int64
upsertHourlyStatBatch [] = pure 0
upsertHourlyStatBatch rows =
  sum
    <$> forM
      (Map.toList deduped)
      ( \((pid, sf, ph, hb), ec) ->
          Hasql.interpExecute
            [HI.sql| INSERT INTO apis.log_pattern_hourly_stats (project_id, source_field, pattern_hash, hour_bucket, event_count)
        VALUES (#{pid}, #{sf}, #{ph}, #{hb}, #{ec})
        ON CONFLICT (project_id, source_field, pattern_hash, hour_bucket)
        DO UPDATE SET event_count = apis.log_pattern_hourly_stats.event_count + EXCLUDED.event_count |]
      )
  where
    deduped = Map.fromListWith (+) [((pid, sf, ph, truncateHour hb), ec) | (pid, sf, ph, hb, ec) <- rows]


-- | Batch: computes median + MAD for all patterns in one query.
-- Joins on (source_field, pattern_hash) — pattern_hash alone is NOT unique across source fields.
data BatchPatternStats = BatchPatternStats
  { sourceField :: Text
  , patternHash :: Text
  , hourlyMedian :: Double
  , hourlyMADScaled :: Double
  , totalHours :: Int
  , totalEvents :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow)


getBatchPatternStats :: DB es => Projects.ProjectId -> UTCTime -> Int -> Eff es [BatchPatternStats]
getBatchPatternStats pid now hoursBack = Hasql.interp q
  where
    q =
      [HI.sql|
        WITH hourly_counts AS (
          SELECT source_field, pattern_hash, hour_bucket, event_count FROM apis.log_pattern_hourly_stats
          WHERE project_id = #{pid} AND hour_bucket >= #{now}::timestamptz - INTERVAL '1 hour' * #{hoursBack}
        ),
        median_calc AS (
          SELECT source_field, pattern_hash, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY event_count) AS median_val
          FROM hourly_counts GROUP BY source_field, pattern_hash
        ),
        mad_calc AS (
          SELECT hc.source_field, hc.pattern_hash, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(hc.event_count - mc.median_val)) AS mad_val
          FROM hourly_counts hc JOIN median_calc mc ON hc.source_field = mc.source_field AND hc.pattern_hash = mc.pattern_hash
          GROUP BY hc.source_field, hc.pattern_hash
        ),
        totals AS (
          SELECT source_field, pattern_hash, COUNT(*)::INT AS total_hours, COALESCE(SUM(event_count), 0)::BIGINT AS total_events
          FROM hourly_counts GROUP BY source_field, pattern_hash
        )
        -- 1.4826 = consistency factor (1/Φ⁻¹(3/4)) to convert MAD to std-dev equivalent under normality
        SELECT mc.source_field, mc.pattern_hash, COALESCE(mc.median_val, 0)::FLOAT, COALESCE(mad.mad_val * 1.4826, 0)::FLOAT,
          t.total_hours, t.total_events
        FROM median_calc mc
        JOIN mad_calc mad ON mc.source_field = mad.source_field AND mc.pattern_hash = mad.pattern_hash
        JOIN totals t ON mc.source_field = t.source_field AND mc.pattern_hash = t.pattern_hash
      |]


-- | Log pattern with current rate (for batch spike detection)
data LogPatternWithRate = LogPatternWithRate
  { patternId :: LogPatternId
  , projectId :: Projects.ProjectId
  , logPattern :: Text
  , patternHash :: Text
  , sourceField :: Text
  , serviceName :: Maybe Text
  , logLevel :: Maybe Text
  , sampleMessage :: Maybe Text
  , baselineState :: BaselineState
  , baselineMean :: Maybe Double
  , baselineMad :: Maybe Double
  , currentHourCount :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow)


-- | Get all established patterns with their current hour counts for spike detection.
-- Uses the current (partial) hour's count; caller projects to hourly rate.
-- Intentionally unbounded: the established filter is a natural cap and we need
-- completeness to avoid missing spikes. A LIMIT would silently skip patterns.
-- Scale ceiling: established patterns grow slowly (~weeks); expect <1k per project at steady state.
getPatternsWithCurrentRates :: DB es => Projects.ProjectId -> UTCTime -> Eff es [LogPatternWithRate]
getPatternsWithCurrentRates pid now =
  Hasql.interp q
  where
    q =
      [HI.sql|
        SELECT lp.id, lp.project_id, LEFT(lp.log_pattern, 2000), lp.pattern_hash, lp.source_field,
          lp.service_name, lp.log_level, LEFT(lp.sample_message, 2000),
          lp.baseline_state, lp.baseline_volume_hourly_mean, lp.baseline_volume_hourly_mad,
          COALESCE(hs.event_count, 0)::BIGINT + COALESCE(mhs.member_count, 0)::BIGINT AS current_hour_count
        FROM apis.log_patterns lp
        LEFT JOIN apis.log_pattern_hourly_stats hs
          ON hs.project_id = lp.project_id AND hs.source_field = lp.source_field
          AND hs.pattern_hash = lp.pattern_hash AND hs.hour_bucket = date_trunc('hour', #{now}::timestamptz)
        LEFT JOIN LATERAL (
          SELECT COALESCE(SUM(mh.event_count), 0) AS member_count
          FROM apis.log_patterns m
          JOIN apis.log_pattern_hourly_stats mh
            ON mh.project_id = m.project_id AND mh.source_field = m.source_field
            AND mh.pattern_hash = m.pattern_hash AND mh.hour_bucket = date_trunc('hour', #{now}::timestamptz)
          WHERE m.canonical_id = lp.id
        ) mhs ON TRUE
        WHERE lp.project_id = #{pid}
          AND lp.baseline_state = #{BSEstablished}
          AND lp.canonical_id IS NULL
      |]


-- | Get multiple patterns by IDs in a single query (avoids N+1)
getLogPatternsByIds :: DB es => V.Vector LogPatternId -> Eff es (V.Vector LogPattern)
getLogPatternsByIds ids
  | V.null ids = pure V.empty
  | otherwise = V.fromList <$> Hasql.interp [HI.sql| SELECT * FROM apis.log_patterns WHERE id = ANY(#{ids}) |]


-- | Canonical mapping of source field identifiers to human-readable labels.
--
-- >>> map fst knownPatternFields
-- ["body","summary","url_path","exception"]
knownPatternFields :: [(Text, Text)]
knownPatternFields = [("body", "Log body"), ("summary", "Event summary"), ("url_path", "URL path"), ("exception", "Exception message")]


-- | Human-readable label for a source field, falling back to the raw field name.
--
-- >>> sourceFieldLabel "summary"
-- "Event summary"
-- >>> sourceFieldLabel "unknown_field"
-- "unknown_field"
sourceFieldLabel :: Text -> Text
sourceFieldLabel f = fromMaybe f $ lookup f knownPatternFields


-- | Total event count across all patterns for a project within a time window.
getTotalEventCount :: DB es => Projects.ProjectId -> UTCTime -> Int -> Eff es Int64
getTotalEventCount pid now hoursBack =
  fromMaybe 0 <$> Hasql.interpOne [HI.sql| SELECT COALESCE(SUM(event_count), 0)::BIGINT FROM apis.log_pattern_hourly_stats WHERE project_id = #{pid} AND hour_bucket >= #{now}::timestamptz - INTERVAL '1 hour' * #{hoursBack} |]


-- | Delete acknowledged patterns not seen in staleDays
pruneStalePatterns :: DB es => Projects.ProjectId -> UTCTime -> Int -> Eff es Int64
pruneStalePatterns pid now staleDays = Hasql.interpExecute [HI.sql| DELETE FROM apis.log_patterns WHERE project_id = #{pid} AND last_seen_at < #{now}::timestamptz - INTERVAL '1 day' * #{staleDays} AND state = #{LPSAcknowledged} |]


-- | Delete hourly stats older than hoursBack (keeps baseline window + buffer)
pruneOldHourlyStats :: DB es => Projects.ProjectId -> UTCTime -> Int -> Eff es Int64
pruneOldHourlyStats pid now hoursBack = Hasql.interpExecute [HI.sql| DELETE FROM apis.log_pattern_hourly_stats WHERE project_id = #{pid} AND hour_bucket < #{now}::timestamptz - INTERVAL '1 hour' * #{hoursBack} |]


-- | Auto-acknowledge patterns stuck in 'new' state longer than staleDays.
-- Prevents unbounded accumulation for low-volume projects that never trigger processNewLogPatterns.
autoAcknowledgeStaleNewPatterns :: DB es => Projects.ProjectId -> UTCTime -> Int -> Eff es Int64
autoAcknowledgeStaleNewPatterns pid now staleDays = Hasql.interpExecute [HI.sql| UPDATE apis.log_patterns SET state = #{LPSAcknowledged}, acknowledged_at = #{now} WHERE project_id = #{pid} AND state = #{LPSNew} AND created_at < #{now}::timestamptz - INTERVAL '1 day' * #{staleDays} |]
