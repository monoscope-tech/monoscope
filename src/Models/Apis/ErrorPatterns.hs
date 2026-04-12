module Models.Apis.ErrorPatterns (
  ErrorPattern (..),
  ErrorPatternId (..),
  ErrorState (..),
  ATError (..),
  ErrorPatternL (..),
  -- Queries
  getErrorPatterns,
  getErrorPatternById,
  getErrorPatternByHash,
  updateOccurrenceCounts,
  propagateMergedCounts,
  updateOccurrenceCountsBatch,
  propagateMergedCountsBatch,
  updateErrorPatternState,
  getErrorPatternLByHash,
  bulkCalculateAndUpdateBaselines,
  resolveErrorPattern,
  batchUpsertErrorPatterns,
  upsertErrorPatternHourlyStats,
  updateErrorPatternSubscription,
  updateErrorPatternThreadIdsAndNotifiedAt,
  setErrorPatternAssignee,
  updateErrorPatternAnalysis,
  -- Error spike detection
  ErrorPatternWithCurrentRate (..),
  getErrorPatternsWithCurrentRates,
  findCanonicalMatch,
  getErrorPatternsByParentHash,
  -- Error Fingerprinting (re-exported from Pkg.ErrorFingerprint)
  EF.StackFrame (..),
  EF.ErrorHashes (..),
  EF.parseStackTrace,
  EF.normalizeStackTrace,
  EF.normalizeMessage,
  EF.computeErrorFingerprint,
  EF.computeErrorHashes,
  EF.isFrameworkTransportError,
)
where

import Data.Aeson qualified as AE
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_select, _selectWhere)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow qualified as FR
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (PGArray (..))

import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (BaselineState (..), DB, WrappedEnumSC (..))
import Pkg.ErrorFingerprint qualified as EF
import Relude hiding (id)
import Utils (truncateHour)


newtype ErrorPatternId = ErrorPatternId {unErrorPatternId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, NFData, Ord, ToField)


data ErrorState
  = ESNew
  | ESEscalating
  | ESOngoing
  | ESResolved
  | ESRegressed
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, ToField) via WrappedEnumSC "ES" ErrorState


data ErrorPattern = ErrorPattern
  { id :: ErrorPatternId
  , projectId :: Projects.ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , errorType :: Text
  , message :: Text
  , stacktrace :: Text
  , hash :: Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , environment :: Maybe Text
  , service :: Maybe Text
  , runtime :: Maybe Text
  , errorData :: ATError
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  , firstEventId :: Maybe UUID.UUID
  , lastEventId :: Maybe UUID.UUID
  , state :: ErrorState
  , assigneeId :: Maybe Projects.UserId
  , assignedAt :: Maybe ZonedTime
  , resolvedAt :: Maybe ZonedTime
  , regressedAt :: Maybe ZonedTime
  , regressionCount :: Int
  , subscribed :: Bool
  , notifyEveryMinutes :: Int
  , lastNotifiedAt :: Maybe ZonedTime
  , occurrences_1m :: Int -- snake_case fields match DB column names exactly
  , occurrences_5m :: Int
  , occurrences_1h :: Int
  , occurrences_24h :: Int
  , quietMinutes :: Int
  , resolutionThresholdMinutes :: Int
  , baselineState :: BaselineState
  , baselineSamples :: Int
  , baselineErrorRateMean :: Maybe Double
  , baselineErrorRateStddev :: Maybe Double
  , baselineUpdatedAt :: Maybe ZonedTime
  , isIgnored :: Bool
  , ignoredUntil :: Maybe ZonedTime
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  , rootCause :: Maybe Text
  , errorCategory :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "error_patterns", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ErrorPattern)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ErrorPattern


-- error pattern aggregated with number of occurrences and affected users
data ErrorPatternL = ErrorPatternL
  { base :: ErrorPattern
  , occurrences :: Int
  , userCount :: Int
  , lastOccurredAt :: Maybe ZonedTime -- hour granularity (bound to MAX(hour_bucket)), not exact event time
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


instance FromRow ErrorPatternL where
  fromRow = ErrorPatternL <$> FR.fromRow <*> FR.field <*> FR.field <*> FR.field


data ATError = ATError
  { projectId :: Maybe Projects.ProjectId
  , when :: UTCTime
  , errorType :: Text
  , rootErrorType :: Text
  , message :: Text
  , rootErrorMessage :: Text
  , stackTrace :: Text
  , hash :: Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , technology :: Maybe LogQueries.SDKTypes
  , requestMethod :: Maybe Text
  , requestPath :: Maybe Text
  , serviceName :: Maybe Text
  , environment :: Maybe Text
  , runtime :: Maybe Text
  , traceId :: Maybe Text
  , spanId :: Maybe Text
  , parentSpanId :: Maybe Text
  , endpointHash :: Maybe Text
  , userId :: Maybe Text
  , userEmail :: Maybe Text
  , userIp :: Maybe Text
  , sessionId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson ATError
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError


-- | Get error patterns for a project with optional state filter (excludes merged patterns)
getErrorPatterns :: DB es => Projects.ProjectId -> Maybe ErrorState -> Int -> Int -> Eff es [ErrorPattern]
getErrorPatterns pid mstate limit offset = case mstate of
  Nothing -> PG.query (_selectWhere @ErrorPattern [[field| project_id |]] <> " AND canonical_id IS NULL ORDER BY updated_at DESC LIMIT ? OFFSET ?") (pid, limit, offset)
  Just st -> PG.query (_selectWhere @ErrorPattern [[field| project_id |], [field| state |]] <> " AND canonical_id IS NULL ORDER BY updated_at DESC LIMIT ? OFFSET ?") (pid, st, limit, offset)


-- | Get error pattern by ID
getErrorPatternById :: DB es => ErrorPatternId -> Eff es (Maybe ErrorPattern)
getErrorPatternById eid = listToMaybe <$> PG.query (_selectWhere @ErrorPattern [[field| id |]]) (Only eid)


-- | Get error pattern by hash
getErrorPatternByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe ErrorPattern)
getErrorPatternByHash pid hash = listToMaybe <$> PG.query (_selectWhere @ErrorPattern [[field| project_id |], [field| hash |]]) (pid, hash)


-- | Get all error pattern children belonging to a parent hash family (used for rollup views).
-- Returns [] for empty/missing parent hash rather than scanning for empty-string rows.
getErrorPatternsByParentHash :: DB es => Projects.ProjectId -> Text -> Eff es [ErrorPattern]
getErrorPatternsByParentHash _pid pHash | T.null pHash = pure []
getErrorPatternsByParentHash pid pHash =
  PG.query (_selectWhere @ErrorPattern [[field| project_id |], [field| parent_hash |]] <> " ORDER BY created_at ASC") (pid, pHash)


getErrorPatternLByHash :: DB es => Projects.ProjectId -> Text -> UTCTime -> Eff es (Maybe ErrorPatternL)
getErrorPatternLByHash pid hash now = listToMaybe <$> PG.query q (now, pid, hash)
  where
    q =
      "SELECT e.*, COALESCE(ev.occurrences, 0)::INT, COALESCE(ev.user_count, 0)::INT, ev.last_occurred_at FROM ("
        <> _select @ErrorPattern
        <> [sql|) e LEFT JOIN LATERAL (
              SELECT SUM(event_count) AS occurrences, SUM(user_count) AS user_count, MAX(hour_bucket) AS last_occurred_at
              FROM apis.error_hourly_stats WHERE error_id = e.id AND hour_bucket >= ?::timestamptz - INTERVAL '30 days'
            ) ev ON true WHERE e.project_id = ? AND e.hash = ? |]


propagateMergedCounts :: DB es => Projects.ProjectId -> Eff es Int64
propagateMergedCounts pid = propagateMergedCountsBatch (V.singleton pid)


updateOccurrenceCounts :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int64
updateOccurrenceCounts pid = updateOccurrenceCountsBatch (V.singleton pid)


-- | Batch version: propagate merged counts for all given projects in a single query.
propagateMergedCountsBatch :: DB es => V.Vector Projects.ProjectId -> Eff es Int64
propagateMergedCountsBatch pids | V.null pids = pure 0
propagateMergedCountsBatch pids =
  PG.execute
    [sql|
    WITH snapshot AS (
      SELECT id, canonical_id, occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h
      FROM apis.error_patterns
      WHERE project_id = ANY(?::uuid[]) AND canonical_id IS NOT NULL
        AND (occurrences_1m > 0 OR occurrences_5m > 0 OR occurrences_1h > 0 OR occurrences_24h > 0)
    ),
    zeroed AS (
      UPDATE apis.error_patterns e SET occurrences_1m = 0, occurrences_5m = 0, occurrences_1h = 0, occurrences_24h = 0
      FROM snapshot s WHERE e.id = s.id
    )
    UPDATE apis.error_patterns c SET
      occurrences_1m = c.occurrences_1m + m.sum_1m,
      occurrences_5m = c.occurrences_5m + m.sum_5m,
      occurrences_1h = c.occurrences_1h + m.sum_1h,
      occurrences_24h = c.occurrences_24h + m.sum_24h
    FROM (SELECT canonical_id, SUM(occurrences_1m) as sum_1m, SUM(occurrences_5m) as sum_5m,
            SUM(occurrences_1h) as sum_1h, SUM(occurrences_24h) as sum_24h
          FROM snapshot GROUP BY canonical_id) m
    WHERE c.id = m.canonical_id AND c.project_id = ANY(?::uuid[]) |]
    (PGArray (V.toList pids), PGArray (V.toList pids))


-- | Batch version: decay occurrence counts for all given projects in a single query.
updateOccurrenceCountsBatch :: DB es => V.Vector Projects.ProjectId -> UTCTime -> Eff es Int64
updateOccurrenceCountsBatch pids _ | V.null pids = pure 0
updateOccurrenceCountsBatch pids now =
  PG.execute
    [sql|
      UPDATE apis.error_patterns SET
        occurrences_1m = 0,
        occurrences_5m = GREATEST(0, occurrences_5m - occurrences_1m),
        occurrences_1h = GREATEST(0, occurrences_1h - occurrences_5m),
        occurrences_24h = GREATEST(0, occurrences_24h - occurrences_1h),
        quiet_minutes = CASE WHEN occurrences_1m = 0 THEN quiet_minutes + 1 ELSE 0 END,
        state = CASE
          WHEN state IN ('new', 'escalating', 'ongoing', 'regressed') AND quiet_minutes + 1 >= resolution_threshold_minutes THEN 'resolved'
          WHEN state = 'regressed' AND regressed_at IS NOT NULL AND ?::timestamptz - regressed_at >= INTERVAL '7 days' THEN 'ongoing'
          ELSE state
        END,
        resolved_at = CASE
          WHEN state IN ('new', 'escalating', 'ongoing', 'regressed') AND quiet_minutes + 1 >= resolution_threshold_minutes THEN ?
          ELSE resolved_at
        END
      WHERE project_id = ANY(?::uuid[]) AND (state != 'resolved' OR occurrences_24h > 0)
    |]
    (now, now, PGArray (V.toList pids))


updateErrorPatternState :: DB es => ErrorPatternId -> ErrorState -> UTCTime -> Eff es Int64
updateErrorPatternState eid newState now = PG.execute q (newState, now, eid)
  where
    q =
      [sql| UPDATE apis.error_patterns SET state = ?, updated_at = ? WHERE id = ? |]


resolveErrorPattern :: DB es => ErrorPatternId -> UTCTime -> Eff es Int64
resolveErrorPattern eid now = PG.execute q (now, now, eid)
  where
    q = [sql| UPDATE apis.error_patterns SET state = 'resolved', resolved_at = ?, updated_at = ? WHERE id = ? |]


setErrorPatternAssignee :: DB es => ErrorPatternId -> Maybe Projects.UserId -> UTCTime -> Eff es Int64
setErrorPatternAssignee eid assigneeIdM now =
  PG.execute q (assigneeIdM, assigneeIdM, now, now, eid)
  where
    q =
      [sql|
        UPDATE apis.error_patterns SET
          assignee_id = ?,
          assigned_at = CASE WHEN ? IS NOT NULL THEN ?::timestamptz ELSE NULL END,
          updated_at = ?
        WHERE id = ?
      |]


updateErrorPatternAnalysis :: DB es => ErrorPatternId -> Text -> Text -> Eff es Int64
updateErrorPatternAnalysis eid rootCause errorCategory =
  PG.execute [sql| UPDATE apis.error_patterns SET root_cause = ?, error_category = ? WHERE id = ? |] (rootCause, errorCategory, eid)


updateErrorPatternSubscription :: DB es => ErrorPatternId -> Bool -> Int -> UTCTime -> Eff es Int64
updateErrorPatternSubscription eid subscribed notifyEveryMinutes now =
  PG.execute q (subscribed, notifyEveryMinutes, subscribed, now, eid)
  where
    q =
      [sql|
        UPDATE apis.error_patterns SET
          subscribed = ?,
          notify_every_minutes = ?,
          last_notified_at = CASE WHEN ? AND NOT subscribed THEN NULL ELSE last_notified_at END,
          updated_at = ?
        WHERE id = ?
      |]


updateErrorPatternThreadIdsAndNotifiedAt :: DB es => ErrorPatternId -> Maybe Text -> Maybe Text -> UTCTime -> Eff es Int64
updateErrorPatternThreadIdsAndNotifiedAt = updateErrorPatternThreadIds' True


updateErrorPatternThreadIds' :: DB es => Bool -> ErrorPatternId -> Maybe Text -> Maybe Text -> UTCTime -> Eff es Int64
updateErrorPatternThreadIds' updateNotifiedAt eid slackTs discordMsgId now =
  PG.execute
    [sql| UPDATE apis.error_patterns SET
        slack_thread_ts = COALESCE(?, slack_thread_ts),
        discord_message_id = COALESCE(?, discord_message_id),
        last_notified_at = CASE WHEN ? THEN ? ELSE last_notified_at END,
        updated_at = ? WHERE id = ? |]
    (slackTs, discordMsgId, updateNotifiedAt, now, now, eid)


-- | Bulk-update baselines for all active error patterns in a project using a single SQL CTE.
-- Reads pre-bucketed data from error_hourly_stats over the last 168 hours.
-- Note: the 'mad' CTE omits a direct JOIN back to error_patterns because it already joins 'stats',
-- which only contains non-resolved errors (filtered via state != 'resolved' in the stats CTE).
bulkCalculateAndUpdateBaselines :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int64
bulkCalculateAndUpdateBaselines pid now =
  PG.execute
    [sql|
      WITH stats AS (
        SELECT ehs.error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ehs.event_count) AS median_val,
          COUNT(*) AS total_hours
        FROM apis.error_hourly_stats ehs
        JOIN apis.error_patterns e ON e.id = ehs.error_id
        WHERE e.project_id = ? AND e.state != 'resolved' AND ehs.hour_bucket >= ?::timestamptz - INTERVAL '168 hours'
        GROUP BY ehs.error_id
      ),
      mad AS (
        SELECT ehs.error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(ehs.event_count - s.median_val)) * 1.4826 AS mad_scaled
        FROM apis.error_hourly_stats ehs
        JOIN stats s ON s.error_id = ehs.error_id
        WHERE ehs.hour_bucket >= ?::timestamptz - INTERVAL '168 hours' AND ehs.project_id = ?
        GROUP BY ehs.error_id
      )
      UPDATE apis.error_patterns SET
        baseline_error_rate_mean = s.median_val,
        baseline_error_rate_stddev = COALESCE(m.mad_scaled, 0),
        baseline_samples = s.total_hours::INT,
        baseline_state = CASE WHEN s.total_hours >= 24 THEN 'established' ELSE 'learning' END,
        baseline_updated_at = ?
      FROM stats s LEFT JOIN mad m ON m.error_id = s.error_id
      WHERE apis.error_patterns.id = s.error_id AND apis.error_patterns.project_id = ?
    |]
    (pid, now, now, pid, now, pid)


-- | Get all error patterns with their current hour counts (for batch spike detection)
data ErrorPatternWithCurrentRate = ErrorPatternWithCurrentRate
  { errorId :: ErrorPatternId
  , projectId :: Projects.ProjectId
  , errorType :: Text
  , message :: Text
  , service :: Maybe Text
  , state :: ErrorState
  , baselineState :: BaselineState
  , baselineMean :: Maybe Double
  , baselineStddev :: Maybe Double
  , currentHourCount :: Int
  , errorData :: ATError
  , stacktrace :: Text
  , hash :: Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


getErrorPatternsWithCurrentRates :: DB es => Projects.ProjectId -> UTCTime -> Eff es [ErrorPatternWithCurrentRate]
getErrorPatternsWithCurrentRates pid now =
  PG.query q (truncateHour now, pid)
  where
    q =
      [sql|
        SELECT
          e.id, e.project_id, e.error_type, LEFT(e.message, 2000), e.service, e.state,
          e.baseline_state, e.baseline_error_rate_mean, e.baseline_error_rate_stddev,
          COALESCE(counts.event_count, 0)::INT AS current_hour_count,
          e.error_data, LEFT(e.stacktrace, 8000), e.hash, e.parent_hash, e.is_framework, e.slack_thread_ts, e.discord_message_id
        FROM apis.error_patterns e
        LEFT JOIN apis.error_hourly_stats counts
          ON counts.error_id = e.id AND counts.project_id = e.project_id
          AND counts.hour_bucket = ?
        WHERE e.project_id = ? AND e.state != 'resolved' AND NOT e.is_ignored
      |]


-- | Find an existing canonical error pattern with matching (project_id, service, error_type, message).
-- Used for fast pre-merge before issue creation to prevent notification spam from identical errors across different spans.
findCanonicalMatch :: DB es => Projects.ProjectId -> Maybe Text -> Text -> Text -> Eff es (Maybe ErrorPatternId)
findCanonicalMatch pid service errorType msg =
  fmap fromOnly
    . listToMaybe
    <$> PG.query
      [sql| SELECT id FROM apis.error_patterns
        WHERE project_id = ? AND service IS NOT DISTINCT FROM ?
          AND error_type = ? AND message = ?
          AND canonical_id IS NULL AND merge_override = FALSE
        ORDER BY created_at ASC LIMIT 1 |]
      (pid, service, errorType, msg)


-- | Batch upsert error patterns using unnest arrays (single round-trip instead of N+1)
-- Groups by hash to avoid "ON CONFLICT DO UPDATE cannot affect row a second time" errors.
-- Returns hashes with their state for newly inserted ('new') or regressed patterns.
batchUpsertErrorPatterns :: DB es => Projects.ProjectId -> V.Vector ATError -> UTCTime -> Eff es [(Text, Text)]
batchUpsertErrorPatterns _pid errors _now | V.null errors = pure []
batchUpsertErrorPatterns pid errors now =
  filter (\(_, s) -> s == "new" || s == "regressed")
    <$> PG.query
      [sql| INSERT INTO apis.error_patterns (
            project_id, error_type, message, stacktrace, hash, parent_hash, is_framework,
            environment, service, runtime, error_data,
            first_trace_id, recent_trace_id,
            occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h)
          SELECT ?, u.error_type, u.message, u.stacktrace, u.hash, u.parent_hash, u.is_framework,
                 u.environment, u.service, u.runtime, u.error_data,
                 u.trace_id, u.trace_id, u.cnt, u.cnt, u.cnt, u.cnt
          FROM (SELECT unnest(?::text[]) AS error_type, unnest(?::text[]) AS message,
                       unnest(?::text[]) AS stacktrace, unnest(?::text[]) AS hash,
                       unnest(?::text[]) AS parent_hash, unnest(?::bool[]) AS is_framework,
                       unnest(?::text[]) AS environment, unnest(?::text[]) AS service,
                       unnest(?::text[]) AS runtime, unnest(?::jsonb[]) AS error_data,
                       unnest(?::text[]) AS trace_id, unnest(?::int[]) AS cnt) u
          ON CONFLICT (project_id, hash) DO UPDATE SET
            updated_at = ?,
            message = EXCLUDED.message,
            error_data = EXCLUDED.error_data,
            recent_trace_id = EXCLUDED.recent_trace_id,
            -- Both fields are deterministic from (error_type, message, stacktrace, service, runtime)
            -- so EXCLUDED always matches the existing row for the same hash. Write-through keeps
            -- semantics symmetric (no sticky-first vs sticky-true asymmetry) and lets a backfill/
            -- classifier upgrade take effect immediately.
            parent_hash = EXCLUDED.parent_hash,
            is_framework = EXCLUDED.is_framework,
            occurrences_1m = apis.error_patterns.occurrences_1m + EXCLUDED.occurrences_1m,
            occurrences_5m = apis.error_patterns.occurrences_5m + EXCLUDED.occurrences_1m,
            occurrences_1h = apis.error_patterns.occurrences_1h + EXCLUDED.occurrences_1m,
            occurrences_24h = apis.error_patterns.occurrences_24h + EXCLUDED.occurrences_1m,
            quiet_minutes = CASE WHEN apis.error_patterns.state = 'resolved' THEN 0 ELSE apis.error_patterns.quiet_minutes END,
            state = CASE WHEN apis.error_patterns.state = 'resolved' THEN 'regressed' ELSE apis.error_patterns.state END,
            regressed_at = CASE WHEN apis.error_patterns.state = 'resolved' THEN ? ELSE apis.error_patterns.regressed_at END,
            regression_count = CASE WHEN apis.error_patterns.state = 'resolved'
                                    THEN apis.error_patterns.regression_count + 1
                                    ELSE apis.error_patterns.regression_count END
          RETURNING hash, CASE
            WHEN xmax = 0 THEN 'new'
            WHEN state = 'regressed' AND regressed_at = ? THEN 'regressed'
            ELSE 'unchanged' END::text |]
      (pid, errorTypes, messages, stacktraces, hashes, parentHashes, isFrameworks, environments, services, runtimes, errorDatas, traceIds, counts, now, now, now)
  where
    -- Group by hash: keep last occurrence + sum count (avoids ON CONFLICT duplicate-row error)
    grouped = HM.elems $ V.foldl' (\m e -> HM.insertWith (\(a, n) (_, k) -> (a, n + k)) e.hash (e, 1 :: Int) m) HM.empty errors
    (errs, counts) = V.unzip $ V.fromList grouped
    errorTypes = V.map (.errorType) errs
    messages = V.map (.message) errs
    stacktraces = V.map (.stackTrace) errs
    hashes = V.map (.hash) errs
    parentHashes = V.map (.parentHash) errs
    isFrameworks = V.map (.isFramework) errs
    environments = V.map (.environment) errs
    services = V.map (.serviceName) errs
    runtimes = V.map (.runtime) errs
    errorDatas = V.map Aeson errs
    traceIds = V.map (.traceId) errs


-- | Batch upsert hourly rollup stats. Takes (hash, event_count, user_count) triples and
-- resolves error_id via JOIN on apis.error_patterns(project_id, hash).
upsertErrorPatternHourlyStats :: DB es => Projects.ProjectId -> UTCTime -> V.Vector (Text, Int, Int) -> Eff es Int64
upsertErrorPatternHourlyStats _pid _now stats | V.null stats = pure 0
upsertErrorPatternHourlyStats pid now stats =
  PG.execute
    [sql| INSERT INTO apis.error_hourly_stats (project_id, error_id, hour_bucket, event_count, user_count)
          SELECT e.project_id, e.id, ?, u.event_count, u.user_count
          FROM (SELECT unnest(?::text[]) AS hash, unnest(?::int[]) AS event_count, unnest(?::int[]) AS user_count) u
          JOIN apis.error_patterns e ON e.project_id = ? AND e.hash = u.hash
          ON CONFLICT (project_id, error_id, hour_bucket)
          DO UPDATE SET event_count = apis.error_hourly_stats.event_count + EXCLUDED.event_count,
                        user_count = apis.error_hourly_stats.user_count + EXCLUDED.user_count |]
    (truncateHour now, hashes, eventCounts, userCounts, pid)
  where
    (hashes, eventCounts, userCounts) = V.unzip3 stats
