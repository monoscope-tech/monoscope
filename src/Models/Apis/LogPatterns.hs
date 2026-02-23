module Models.Apis.LogPatterns (
  LogPattern (..),
  LogPatternId,
  LogPatternState (..),
  getLogPatterns,
  getLogPatternTexts,
  getLogPatternByHash,
  acknowledgeLogPatterns,
  UpsertPattern (..),
  upsertLogPattern,
  updateBaseline,
  -- Hourly stats
  upsertHourlyStat,
  BatchPatternStats (..),
  getBatchPatternStats,
  getCurrentHourPatternCount,
  -- Pattern with current rate for spike detection
  LogPatternWithRate (..),
  getPatternsWithCurrentRates,
  getLogPatternById,
  getLogPatternsByIds,
)
where

import Data.Aeson qualified as AE
import Data.Time
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_select, _selectWhere)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Entity.Types qualified as DAT
import Database.PostgreSQL.Simple (FromRow, Only (Only, fromOnly), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Users
import Pkg.DeriveUtils (BaselineState (..), WrappedEnumSC (..))
import Relude hiding (id)
import System.Types (DB)


newtype LogPatternId = LogPatternId {unLogPatternId :: Int64}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, NFData, Ord, ToField)


data LogPatternState
  = LPSNew
  | LPSAcknowledged
  | LPSIgnored
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "LPS", DAE.CamelToSnake]] LogPatternState
  deriving (FromField, ToField) via WrappedEnumSC "LPS" LogPatternState


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
  , firstSeenAt :: ZonedTime
  , lastSeenAt :: ZonedTime
  , occurrenceCount :: Int64
  , state :: LogPatternState
  , acknowledgedBy :: Maybe Users.UserId
  , acknowledgedAt :: Maybe ZonedTime
  , baselineState :: BaselineState
  , baselineVolumeHourlyMean :: Maybe Double
  , baselineVolumeHourlyMad :: Maybe Double
  , baselineSamples :: Int
  , baselineUpdatedAt :: Maybe ZonedTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
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
  deriving anyclass (ToRow)


-- | Get all log patterns for a project
getLogPatterns :: DB es => Projects.ProjectId -> Int -> Int -> Eff es [LogPattern]
getLogPatterns pid limit offset = PG.query (_selectWhere @LogPattern [[DAT.field| project_id |]] <> " ORDER BY last_seen_at DESC LIMIT ? OFFSET ?") (pid, limit, offset)


getLogPatternTexts :: DB es => Projects.ProjectId -> Text -> Eff es [Text]
getLogPatternTexts pid sourceField = map fromOnly <$> PG.query [sql| SELECT log_pattern FROM apis.log_patterns WHERE project_id = ? AND source_field = ?|] (pid, sourceField)


-- | Get log pattern by hash
getLogPatternByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe LogPattern)
getLogPatternByHash pid hash = listToMaybe <$> PG.query (_selectWhere @LogPattern [[DAT.field| project_id |], [DAT.field| pattern_hash |]]) (pid, hash)


-- | Acknowledge log patterns
acknowledgeLogPatterns :: DB es => Projects.ProjectId -> Users.UserId -> V.Vector Text -> Eff es Int64
acknowledgeLogPatterns pid uid patternHashes
  | V.null patternHashes = pure 0
  | otherwise = PG.execute q (uid, pid, patternHashes)
  where
    q =
      [sql|
        UPDATE apis.log_patterns
        SET state = 'acknowledged', acknowledged_by = ?, acknowledged_at = NOW()
        WHERE project_id = ? AND pattern_hash = ANY(?)
      |]


upsertLogPattern :: DB es => UpsertPattern -> Eff es Int64
upsertLogPattern up =
  PG.execute
    [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, source_field, service_name, log_level, trace_id, sample_message, occurrence_count)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
        ON CONFLICT (project_id, source_field, pattern_hash) DO UPDATE SET
          last_seen_at = NOW(),
          occurrence_count = apis.log_patterns.occurrence_count + EXCLUDED.occurrence_count,
          sample_message = COALESCE(EXCLUDED.sample_message, apis.log_patterns.sample_message),
          service_name = COALESCE(EXCLUDED.service_name, apis.log_patterns.service_name),
          log_level = COALESCE(EXCLUDED.log_level, apis.log_patterns.log_level),
          trace_id = COALESCE(EXCLUDED.trace_id, apis.log_patterns.trace_id)
  |]
    up


updateBaseline :: DB es => Projects.ProjectId -> Text -> BaselineState -> Double -> Double -> Int -> Eff es Int64
updateBaseline pid patHash bState hourlyMean hourlyMad samples =
  PG.execute q (bState, hourlyMean, hourlyMad, samples, pid, patHash)
  where
    q =
      [sql|
        UPDATE apis.log_patterns
        SET baseline_state = ?,
            baseline_volume_hourly_mean = ?,
            baseline_volume_hourly_mad = ?,
            baseline_samples = ?,
            baseline_updated_at = NOW()
        WHERE project_id = ? AND pattern_hash = ?
      |]


-- | Upsert hourly event count for a pattern into the pre-aggregated stats table
upsertHourlyStat :: DB es => Projects.ProjectId -> Text -> UTCTime -> Int64 -> Eff es Int64
upsertHourlyStat pid patHash hourBucket count =
  PG.execute
    [sql| INSERT INTO apis.log_pattern_hourly_stats (project_id, pattern_hash, hour_bucket, event_count)
        VALUES (?, ?, date_trunc('hour', ?::timestamptz), ?)
        ON CONFLICT (project_id, pattern_hash, hour_bucket)
        DO UPDATE SET event_count = apis.log_pattern_hourly_stats.event_count + EXCLUDED.event_count |]
    (pid, patHash, hourBucket, count)


-- | Batch: computes median + MAD for all patterns in one query
data BatchPatternStats = BatchPatternStats
  { patternHash :: Text
  , hourlyMedian :: Double
  , hourlyMADScaled :: Double
  , totalHours :: Int
  , totalEvents :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


getBatchPatternStats :: DB es => Projects.ProjectId -> Int -> Eff es [BatchPatternStats]
getBatchPatternStats pid hoursBack = PG.query q (pid, hoursBack)
  where
    q =
      [sql|
        WITH hourly_counts AS (
          SELECT pattern_hash, hour_bucket, event_count FROM apis.log_pattern_hourly_stats
          WHERE project_id = ? AND hour_bucket >= NOW() - INTERVAL '1 hour' * ?
        ),
        median_calc AS (
          SELECT pattern_hash, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY event_count) AS median_val
          FROM hourly_counts GROUP BY pattern_hash
        ),
        mad_calc AS (
          SELECT hc.pattern_hash, PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(hc.event_count - mc.median_val)) AS mad_val
          FROM hourly_counts hc JOIN median_calc mc ON hc.pattern_hash = mc.pattern_hash GROUP BY hc.pattern_hash
        ),
        totals AS (
          SELECT pattern_hash, COUNT(*)::INT AS total_hours, COALESCE(SUM(event_count), 0)::INT AS total_events
          FROM hourly_counts GROUP BY pattern_hash
        )
        SELECT mc.pattern_hash, COALESCE(mc.median_val, 0)::FLOAT, COALESCE(mad.mad_val * 1.4826, 0)::FLOAT,
          t.total_hours, t.total_events
        FROM median_calc mc
        JOIN mad_calc mad ON mc.pattern_hash = mad.pattern_hash
        JOIN totals t ON mc.pattern_hash = t.pattern_hash
      |]


-- | Get current hour count for a pattern from hourly stats table
getCurrentHourPatternCount :: DB es => Projects.ProjectId -> Text -> Eff es Int
getCurrentHourPatternCount pid patHash =
  maybe 0 fromOnly
    . listToMaybe
    <$> PG.query
      [sql| SELECT COALESCE(event_count, 0)::INT FROM apis.log_pattern_hourly_stats
          WHERE project_id = ? AND pattern_hash = ? AND hour_bucket = date_trunc('hour', NOW()) |]
      (pid, patHash)


-- | Log pattern with current rate (for batch spike detection)
data LogPatternWithRate = LogPatternWithRate
  { patternId :: LogPatternId
  , projectId :: Projects.ProjectId
  , logPattern :: Text
  , patternHash :: Text
  , sourceField :: Text
  , baselineState :: BaselineState
  , baselineMean :: Maybe Double
  , baselineMad :: Maybe Double
  , currentHourCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


-- | Get all patterns with their current hour counts (all pattern types participate in spike detection)
getPatternsWithCurrentRates :: DB es => Projects.ProjectId -> Eff es [LogPatternWithRate]
getPatternsWithCurrentRates pid =
  PG.query q (Only pid)
  where
    q =
      [sql|
        SELECT lp.id, lp.project_id, lp.log_pattern, lp.pattern_hash, lp.source_field,
          lp.baseline_state, lp.baseline_volume_hourly_mean, lp.baseline_volume_hourly_mad,
          COALESCE(hs.event_count, 0)::INT AS current_hour_count
        FROM apis.log_patterns lp
        LEFT JOIN apis.log_pattern_hourly_stats hs
          ON hs.project_id = lp.project_id AND hs.pattern_hash = lp.pattern_hash
          AND hs.hour_bucket = date_trunc('hour', NOW())
        WHERE lp.project_id = ?
          AND lp.state != 'ignored' AND lp.baseline_state = 'established'
      |]


-- | Get a pattern by ID
getLogPatternById :: DB es => LogPatternId -> Eff es (Maybe LogPattern)
getLogPatternById lpid = listToMaybe <$> PG.query (_selectWhere @LogPattern [[DAT.field| id |]]) (Only lpid)


-- | Get multiple patterns by IDs in a single query (avoids N+1)
getLogPatternsByIds :: DB es => V.Vector LogPatternId -> Eff es (V.Vector LogPattern)
getLogPatternsByIds ids
  | V.null ids = pure V.empty
  | otherwise = V.fromList <$> PG.query (_select @LogPattern <> " WHERE id = ANY(?)") (Only ids)
