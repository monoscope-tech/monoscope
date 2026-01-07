module Models.Apis.LogPatterns (
  LogPattern (..),
  LogPatternId,
  LogPatternState (..),
  getLogPatterns,
  getLogPatternByHash,
  getNewLogPatterns,
  acknowledgeLogPatterns,
  ignoreLogPatterns,
  upsertLogPattern,
  updateLogPatternStats,
  updateBaseline,
  -- Pattern stats from otel_logs_and_spans
  PatternStats (..),
  getPatternStats,
  getCurrentHourPatternCount,
  -- Pattern with current rate for spike detection
  LogPatternWithRate (..),
  getPatternsWithCurrentRates,
  getLogPatternById,
)
where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Relude hiding (id)
import System.Types (DB)


newtype LogPatternId = LogPatternId {unLogPatternId :: Int64}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, NFData, Ord, ToField)


data LogPatternState
  = LPSNew
  | LPSAcknowledged
  | LPSIgnored
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LogPatternState


logPatternStateToText :: LogPatternState -> Text
logPatternStateToText LPSNew = "new"
logPatternStateToText LPSAcknowledged = "acknowledged"
logPatternStateToText LPSIgnored = "ignored"


parseLogPatternState :: (Eq s, IsString s) => s -> Maybe LogPatternState
parseLogPatternState "new" = Just LPSNew
parseLogPatternState "acknowledged" = Just LPSAcknowledged
parseLogPatternState "ignored" = Just LPSIgnored
parseLogPatternState _ = Nothing


instance ToField LogPatternState where
  toField = Escape . encodeUtf8 <$> logPatternStateToText


instance FromField LogPatternState where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseLogPatternState bs of
          Just s -> pure s
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected log pattern state, got " <> decodeUtf8 bs <> " instead."


data LogPattern = LogPattern
  { id :: LogPatternId
  , projectId :: Projects.ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , pattern :: Text
  , patternHash :: Text
  , serviceName :: Maybe Text
  , logLevel :: Maybe Text
  , sampleMessage :: Maybe Text
  , firstSeenAt :: ZonedTime
  , lastSeenAt :: ZonedTime
  , occurrenceCount :: Int64
  , state :: LogPatternState
  , acknowledgedBy :: Maybe Users.UserId
  , acknowledgedAt :: Maybe ZonedTime
  , baselineState :: Text
  , baselineVolumeHourlyMean :: Maybe Double
  , baselineVolumeHourlyStddev :: Maybe Double
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


-- | Get all log patterns for a project
getLogPatterns :: DB es => Projects.ProjectId -> Maybe LogPatternState -> Int -> Int -> Eff es [LogPattern]
getLogPatterns pid mstate limit offset = PG.query q (pid, maybe "%" logPatternStateToText mstate, limit, offset)
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at, pattern, pattern_hash,
               service_name, log_level, sample_message, first_seen_at, last_seen_at,
               occurrence_count, state, acknowledged_by, acknowledged_at,
               baseline_state, baseline_volume_hourly_mean, baseline_volume_hourly_stddev,
               baseline_samples, baseline_updated_at
        FROM apis.log_patterns
        WHERE project_id = ? AND state LIKE ?
        ORDER BY last_seen_at DESC
        LIMIT ? OFFSET ?
      |]


-- | Get log pattern by hash
getLogPatternByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe LogPattern)
getLogPatternByHash pid hash = do
  results <- PG.query q (pid, hash)
  return $ listToMaybe results
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at, pattern, pattern_hash,
               service_name, log_level, sample_message, first_seen_at, last_seen_at,
               occurrence_count, state, acknowledged_by, acknowledged_at,
               baseline_state, baseline_volume_hourly_mean, baseline_volume_hourly_stddev,
               baseline_samples, baseline_updated_at
        FROM apis.log_patterns
        WHERE project_id = ? AND pattern_hash = ?
      |]


-- | Get new (unacknowledged) log patterns for a project
getNewLogPatterns :: DB es => Projects.ProjectId -> Eff es [LogPattern]
getNewLogPatterns pid = PG.query q (pid,)
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at, pattern, pattern_hash,
               service_name, log_level, sample_message, first_seen_at, last_seen_at,
               occurrence_count, state, acknowledged_by, acknowledged_at,
               baseline_state, baseline_volume_hourly_mean, baseline_volume_hourly_stddev,
               baseline_samples, baseline_updated_at
        FROM apis.log_patterns
        WHERE project_id = ? AND state = 'new'
        ORDER BY first_seen_at DESC
      |]


-- | Acknowledge log patterns
acknowledgeLogPatterns :: DB es => Users.UserId -> V.Vector Text -> Eff es Int64
acknowledgeLogPatterns uid patternHashes
  | V.null patternHashes = pure 0
  | otherwise = PG.execute q (uid, patternHashes)
  where
    q =
      [sql|
        UPDATE apis.log_patterns
        SET state = 'acknowledged', acknowledged_by = ?, acknowledged_at = NOW()
        WHERE pattern_hash = ANY(?)
      |]


-- | Ignore log patterns (won't alert on them)
ignoreLogPatterns :: DB es => V.Vector Text -> Eff es Int64
ignoreLogPatterns patternHashes
  | V.null patternHashes = pure 0
  | otherwise = PG.execute q (patternHashes,)
  where
    q =
      [sql|
        UPDATE apis.log_patterns
        SET state = 'ignored'
        WHERE pattern_hash = ANY(?)
      |]


-- | Upsert a log pattern (insert or update occurrence count)
upsertLogPattern ::
  DB es =>
  Projects.ProjectId ->
  Text -> -- pattern
  Text -> -- pattern_hash
  Maybe Text -> -- service_name
  Maybe Text -> -- log_level
  Maybe Text -> -- sample_message
  Eff es Int64
upsertLogPattern pid pat patHash serviceName logLevel sampleMsg =
  PG.execute q (pid, pat, patHash, serviceName, logLevel, sampleMsg)
  where
    q =
      [sql|
        INSERT INTO apis.log_patterns (project_id, pattern, pattern_hash, service_name, log_level, sample_message)
        VALUES (?, ?, ?, ?, ?, ?)
        ON CONFLICT (project_id, pattern_hash) DO UPDATE SET
          last_seen_at = NOW(),
          occurrence_count = apis.log_patterns.occurrence_count + 1,
          sample_message = COALESCE(EXCLUDED.sample_message, apis.log_patterns.sample_message)
      |]


-- | Update log pattern statistics (occurrence count, last seen)
updateLogPatternStats :: DB es => Projects.ProjectId -> Text -> Int64 -> Eff es Int64
updateLogPatternStats pid patHash additionalCount =
  PG.execute q (additionalCount, pid, patHash)
  where
    q =
      [sql|
        UPDATE apis.log_patterns
        SET occurrence_count = occurrence_count + ?,
            last_seen_at = NOW()
        WHERE project_id = ? AND pattern_hash = ?
      |]


-- | Update baseline data for a log pattern
updateBaseline ::
  DB es =>
  Projects.ProjectId ->
  Text -> -- pattern_hash
  Text -> -- baseline_state ('learning' or 'established')
  Double -> -- hourly_mean
  Double -> -- hourly_stddev
  Int -> -- samples
  Eff es Int64
updateBaseline pid patHash bState hourlyMean hourlyStddev samples =
  PG.execute q (bState, hourlyMean, hourlyStddev, samples, pid, patHash)
  where
    q =
      [sql|
        UPDATE apis.log_patterns
        SET baseline_state = ?,
            baseline_volume_hourly_mean = ?,
            baseline_volume_hourly_stddev = ?,
            baseline_samples = ?,
            baseline_updated_at = NOW()
        WHERE project_id = ? AND pattern_hash = ?
      |]


-- ============================================================================
-- Log Pattern Occurrence Queries (from otel_logs_and_spans)
-- ============================================================================

-- | Stats for a log pattern from otel_logs_and_spans
data PatternStats = PatternStats
  { hourlyMean :: Double
  , hourlyStddev :: Double
  , totalHours :: Int
  , totalEvents :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


-- | Get pattern stats from otel_logs_and_spans
getPatternStats ::
  DB es =>
  Projects.ProjectId ->
  Text -> -- pattern (log_pattern value)
  Int -> -- hours to look back
  Eff es (Maybe PatternStats)
getPatternStats pid pattern hoursBack = do
  results <- PG.query q (pid, pattern, hoursBack)
  return $ listToMaybe results
  where
    q =
      [sql|
        WITH hourly_counts AS (
          SELECT
            date_trunc('hour', timestamp) AS hour_start,
            COUNT(*) AS event_count
          FROM otel_logs_and_spans
          WHERE project_id = ?::text
            AND log_pattern = ?
            AND timestamp >= NOW() - INTERVAL '1 hour' * ?
          GROUP BY date_trunc('hour', timestamp)
        )
        SELECT
          COALESCE(AVG(event_count), 0)::FLOAT AS hourly_mean,
          COALESCE(STDDEV(event_count), 0)::FLOAT AS hourly_stddev,
          COUNT(*)::INT AS total_hours,
          COALESCE(SUM(event_count), 0)::INT AS total_events
        FROM hourly_counts
      |]


-- | Get current hour count for a pattern
getCurrentHourPatternCount :: DB es => Projects.ProjectId -> Text -> Eff es Int
getCurrentHourPatternCount pid pattern = do
  results <- PG.query q (pid, pattern)
  case results of
    [Only count] -> return count
    _ -> return 0
  where
    q =
      [sql|
        SELECT COUNT(*)::INT
        FROM otel_logs_and_spans
        WHERE project_id = ?::text
          AND log_pattern = ?
          AND timestamp >= date_trunc('hour', NOW())
      |]


-- | Log pattern with current rate (for batch spike detection)
data LogPatternWithRate = LogPatternWithRate
  { patternId :: LogPatternId
  , projectId :: Projects.ProjectId
  , pattern :: Text
  , patternHash :: Text
  , baselineState :: Text
  , baselineMean :: Maybe Double
  , baselineStddev :: Maybe Double
  , currentHourCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


-- | Get all patterns with their current hour counts
getPatternsWithCurrentRates :: DB es => Projects.ProjectId -> Eff es [LogPatternWithRate]
getPatternsWithCurrentRates pid =
  PG.query q (pid, pid)
  where
    q =
      [sql|
        SELECT
          lp.id,
          lp.project_id,
          lp.pattern,
          lp.pattern_hash,
          lp.baseline_state,
          lp.baseline_volume_hourly_mean,
          lp.baseline_volume_hourly_stddev,
          COALESCE(counts.current_count, 0)::INT AS current_hour_count
        FROM apis.log_patterns lp
        LEFT JOIN (
          SELECT log_pattern, COUNT(*) AS current_count
          FROM otel_logs_and_spans
          WHERE project_id = ?::text
            AND timestamp >= date_trunc('hour', NOW())
            AND log_pattern IS NOT NULL
          GROUP BY log_pattern
        ) counts ON counts.log_pattern = lp.pattern
        WHERE lp.project_id = ?
          AND lp.state != 'ignored'
      |]


-- | Get a pattern by ID
getLogPatternById :: DB es => LogPatternId -> Eff es (Maybe LogPattern)
getLogPatternById lpid = do
  results <- PG.query q (lpid,)
  return $ listToMaybe results
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at, pattern, pattern_hash,
               service_name, log_level, sample_message, first_seen_at, last_seen_at,
               occurrence_count, state, acknowledged_by, acknowledged_at,
               baseline_state, baseline_volume_hourly_mean, baseline_volume_hourly_stddev,
               baseline_samples, baseline_updated_at
        FROM apis.log_patterns
        WHERE id = ?
      |]
