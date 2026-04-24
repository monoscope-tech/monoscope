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
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow qualified as FR
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (BaselineState (..), DB, WrappedEnumSC (..))
import Pkg.ErrorFingerprint qualified as EF
import Relude hiding (id)
import Utils (truncateHour)


newtype ErrorPatternId = ErrorPatternId {unErrorPatternId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, HI.DecodeValue, HI.EncodeValue, NFData, Ord, ToField)
  deriving anyclass (HI.DecodeRow)


data ErrorState
  = ESNew
  | ESEscalating
  | ESOngoing
  | ESResolved
  | ESRegressed
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "ES" ErrorState


data ErrorPattern = ErrorPattern
  { id :: ErrorPatternId
  , projectId :: Projects.ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , errorType :: Text
  , message :: Text
  , stacktrace :: Text
  , hash :: Text
  , environment :: Maybe Text
  , service :: Maybe Text
  , runtime :: Maybe Text
  , errorData :: ATError
  , firstEventId :: Maybe UUID.UUID
  , lastEventId :: Maybe UUID.UUID
  , state :: ErrorState
  , assigneeId :: Maybe Projects.UserId
  , assignedAt :: Maybe ZonedTime
  , resolvedAt :: Maybe ZonedTime
  , regressedAt :: Maybe ZonedTime
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
  , subscribed :: Bool
  , notifyEveryMinutes :: Int
  , lastNotifiedAt :: Maybe ZonedTime
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  , regressionCount :: Int
  , canonicalId :: Maybe ErrorPatternId
  , embedding :: Maybe (V.Vector Float)
  , embeddingAt :: Maybe ZonedTime
  , mergeOverride :: Bool
  , rootCause :: Maybe Text
  , errorCategory :: Maybe Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
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


instance HI.DecodeRow ErrorPatternL where
  decodeRow = ErrorPatternL <$> HI.decodeRow <*> (HI.getOneColumn <$> HI.decodeRow) <*> (HI.getOneColumn <$> HI.decodeRow) <*> (HI.getOneColumn <$> HI.decodeRow)


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
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb ATError


-- | Get error patterns for a project with optional state filter (excludes merged patterns)
getErrorPatterns :: DB es => Projects.ProjectId -> Maybe ErrorState -> Int -> Int -> Eff es [ErrorPattern]
getErrorPatterns pid mstate limit offset = case mstate of
  Nothing -> Hasql.interp [HI.sql| SELECT * FROM apis.error_patterns WHERE project_id = #{pid} AND canonical_id IS NULL ORDER BY updated_at DESC LIMIT #{limit} OFFSET #{offset} |]
  Just st -> Hasql.interp [HI.sql| SELECT * FROM apis.error_patterns WHERE project_id = #{pid} AND state = #{st} AND canonical_id IS NULL ORDER BY updated_at DESC LIMIT #{limit} OFFSET #{offset} |]


-- | Get error pattern by ID
getErrorPatternById :: DB es => ErrorPatternId -> Eff es (Maybe ErrorPattern)
getErrorPatternById eid = Hasql.interpOne [HI.sql| SELECT * FROM apis.error_patterns WHERE id = #{eid} |]


-- | Get error pattern by hash
getErrorPatternByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe ErrorPattern)
getErrorPatternByHash pid eHash = Hasql.interpOne [HI.sql| SELECT * FROM apis.error_patterns WHERE project_id = #{pid} AND hash = #{eHash} |]


-- | Get all error pattern children belonging to a parent hash family (used for rollup views).
-- Returns [] for empty/missing parent hash rather than scanning for empty-string rows.
getErrorPatternsByParentHash :: DB es => Projects.ProjectId -> Text -> Eff es [ErrorPattern]
getErrorPatternsByParentHash _pid pHash | T.null pHash = pure []
getErrorPatternsByParentHash pid pHash =
  Hasql.interp [HI.sql| SELECT * FROM apis.error_patterns WHERE project_id = #{pid} AND parent_hash = #{pHash} ORDER BY created_at ASC |]


getErrorPatternLByHash :: DB es => Projects.ProjectId -> Text -> UTCTime -> Eff es (Maybe ErrorPatternL)
getErrorPatternLByHash pid eHash now =
  Hasql.interpOne
    [HI.sql|
    SELECT e.*, COALESCE(ev.occurrences, 0)::INT, COALESCE(ev.user_count, 0)::INT, ev.last_occurred_at
    FROM apis.error_patterns e LEFT JOIN LATERAL (
      SELECT SUM(event_count) AS occurrences, SUM(user_count) AS user_count, MAX(hour_bucket) AS last_occurred_at
      FROM apis.error_hourly_stats WHERE error_id = e.id AND hour_bucket >= #{now}::timestamptz - INTERVAL '30 days'
    ) ev ON true WHERE e.project_id = #{pid} AND e.hash = #{eHash} |]


propagateMergedCounts :: DB es => Projects.ProjectId -> Eff es Int64
propagateMergedCounts pid = propagateMergedCountsBatch (V.singleton pid)


updateOccurrenceCounts :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int64
updateOccurrenceCounts pid = updateOccurrenceCountsBatch (V.singleton pid)


-- | Batch version: propagate merged counts for all given projects in a single query.
propagateMergedCountsBatch :: DB es => V.Vector Projects.ProjectId -> Eff es Int64
propagateMergedCountsBatch pids | V.null pids = pure 0
propagateMergedCountsBatch pids =
  Hasql.interpExecute
    [HI.sql|
    WITH snapshot AS (
      SELECT id, canonical_id, occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h
      FROM apis.error_patterns
      WHERE project_id = ANY(#{pids}) AND canonical_id IS NOT NULL
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
    WHERE c.id = m.canonical_id AND c.project_id = ANY(#{pids}) |]


-- | Batch version: decay occurrence counts for all given projects in a single query.
updateOccurrenceCountsBatch :: DB es => V.Vector Projects.ProjectId -> UTCTime -> Eff es Int64
updateOccurrenceCountsBatch pids _ | V.null pids = pure 0
updateOccurrenceCountsBatch pids now =
  Hasql.interpExecute
    [HI.sql|
      UPDATE apis.error_patterns SET
        occurrences_1m = 0,
        occurrences_5m = GREATEST(0, occurrences_5m - occurrences_1m),
        occurrences_1h = GREATEST(0, occurrences_1h - occurrences_5m),
        occurrences_24h = GREATEST(0, occurrences_24h - occurrences_1h),
        quiet_minutes = CASE WHEN occurrences_1m = 0 THEN quiet_minutes + 1 ELSE 0 END,
        state = CASE
          WHEN state IN ('new', 'escalating', 'ongoing', 'regressed') AND quiet_minutes + 1 >= resolution_threshold_minutes THEN 'resolved'
          WHEN state = 'regressed' AND regressed_at IS NOT NULL AND #{now}::timestamptz - regressed_at >= INTERVAL '7 days' THEN 'ongoing'
          ELSE state
        END,
        resolved_at = CASE
          WHEN state IN ('new', 'escalating', 'ongoing', 'regressed') AND quiet_minutes + 1 >= resolution_threshold_minutes THEN #{now}
          ELSE resolved_at
        END
      WHERE project_id = ANY(#{pids}) AND (state != 'resolved' OR occurrences_24h > 0)
    |]


updateErrorPatternState :: DB es => ErrorPatternId -> ErrorState -> UTCTime -> Eff es Int64
updateErrorPatternState eid newState now =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET state = #{newState}, updated_at = #{now} WHERE id = #{eid} |]


resolveErrorPattern :: DB es => ErrorPatternId -> UTCTime -> Eff es Int64
resolveErrorPattern eid now =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET state = 'resolved', resolved_at = #{now}, updated_at = #{now} WHERE id = #{eid} |]


setErrorPatternAssignee :: DB es => ErrorPatternId -> Maybe Projects.UserId -> UTCTime -> Eff es Int64
setErrorPatternAssignee eid assigneeIdM now =
  Hasql.interpExecute
    [HI.sql|
        UPDATE apis.error_patterns SET
          assignee_id = #{assigneeIdM},
          assigned_at = CASE WHEN #{assigneeIdM} IS NOT NULL THEN #{now}::timestamptz ELSE NULL END,
          updated_at = #{now}
        WHERE id = #{eid}
      |]


updateErrorPatternAnalysis :: DB es => ErrorPatternId -> Text -> Text -> Eff es Int64
updateErrorPatternAnalysis eid rc eCat =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET root_cause = #{rc}, error_category = #{eCat} WHERE id = #{eid} |]


updateErrorPatternSubscription :: DB es => ErrorPatternId -> Bool -> Int -> UTCTime -> Eff es Int64
updateErrorPatternSubscription eid sub notifyMins now =
  Hasql.interpExecute
    [HI.sql|
        UPDATE apis.error_patterns SET
          subscribed = #{sub},
          notify_every_minutes = #{notifyMins},
          last_notified_at = CASE WHEN #{sub} AND NOT subscribed THEN NULL ELSE last_notified_at END,
          updated_at = #{now}
        WHERE id = #{eid}
      |]


updateErrorPatternThreadIdsAndNotifiedAt :: DB es => ErrorPatternId -> Maybe Text -> Maybe Text -> UTCTime -> Eff es Int64
updateErrorPatternThreadIdsAndNotifiedAt = updateErrorPatternThreadIds' True


updateErrorPatternThreadIds' :: DB es => Bool -> ErrorPatternId -> Maybe Text -> Maybe Text -> UTCTime -> Eff es Int64
updateErrorPatternThreadIds' updateNotifiedAt eid slackTs discordMsgId now =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET
        slack_thread_ts = COALESCE(#{slackTs}, slack_thread_ts),
        discord_message_id = COALESCE(#{discordMsgId}, discord_message_id),
        last_notified_at = CASE WHEN #{updateNotifiedAt} THEN #{now} ELSE last_notified_at END,
        updated_at = #{now} WHERE id = #{eid} |]


-- | Bulk-update baselines for all active error patterns in a project using a single SQL CTE.
-- Reads pre-bucketed data from error_hourly_stats over the last 168 hours.
-- Note: the 'mad' CTE omits a direct JOIN back to error_patterns because it already joins 'stats',
-- which only contains non-resolved errors (filtered via state != 'resolved' in the stats CTE).
bulkCalculateAndUpdateBaselines :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int64
bulkCalculateAndUpdateBaselines pid now =
  Hasql.interpExecute
    [HI.sql|
      WITH stats AS (
        SELECT ehs.error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ehs.event_count) AS median_val,
          COUNT(*) AS total_hours
        FROM apis.error_hourly_stats ehs
        JOIN apis.error_patterns e ON e.id = ehs.error_id
        WHERE e.project_id = #{pid} AND e.state != 'resolved' AND ehs.hour_bucket >= #{now}::timestamptz - INTERVAL '168 hours'
        GROUP BY ehs.error_id
      ),
      mad AS (
        SELECT ehs.error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(ehs.event_count - s.median_val)) * 1.4826 AS mad_scaled
        FROM apis.error_hourly_stats ehs
        JOIN stats s ON s.error_id = ehs.error_id
        WHERE ehs.hour_bucket >= #{now}::timestamptz - INTERVAL '168 hours' AND ehs.project_id = #{pid}
        GROUP BY ehs.error_id
      )
      UPDATE apis.error_patterns SET
        baseline_error_rate_mean = s.median_val,
        baseline_error_rate_stddev = COALESCE(m.mad_scaled, 0),
        baseline_samples = s.total_hours::INT,
        baseline_state = CASE WHEN s.total_hours >= 24 THEN 'established' ELSE 'learning' END,
        baseline_updated_at = #{now}
      FROM stats s LEFT JOIN mad m ON m.error_id = s.error_id
      WHERE apis.error_patterns.id = s.error_id AND apis.error_patterns.project_id = #{pid}
    |]


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
  deriving anyclass (FromRow, HI.DecodeRow)


getErrorPatternsWithCurrentRates :: DB es => Projects.ProjectId -> UTCTime -> Eff es [ErrorPatternWithCurrentRate]
getErrorPatternsWithCurrentRates pid now =
  Hasql.interp q
  where
    hourBucket = truncateHour now
    q =
      [HI.sql|
        SELECT
          e.id, e.project_id, e.error_type, LEFT(e.message, 2000), e.service, e.state,
          e.baseline_state, e.baseline_error_rate_mean, e.baseline_error_rate_stddev,
          COALESCE(counts.event_count, 0)::INT AS current_hour_count,
          e.error_data, LEFT(e.stacktrace, 8000), e.hash, e.parent_hash, e.is_framework, e.slack_thread_ts, e.discord_message_id
        FROM apis.error_patterns e
        LEFT JOIN apis.error_hourly_stats counts
          ON counts.error_id = e.id AND counts.project_id = e.project_id
          AND counts.hour_bucket = #{hourBucket}
        WHERE e.project_id = #{pid} AND e.state != 'resolved' AND NOT e.is_ignored
      |]


-- | Find an existing canonical error pattern with matching (project_id, service, error_type, message).
-- Used for fast pre-merge before issue creation to prevent notification spam from identical errors across different spans.
findCanonicalMatch :: DB es => Projects.ProjectId -> Maybe Text -> Text -> Text -> Eff es (Maybe ErrorPatternId)
findCanonicalMatch pid service eType msg =
  listToMaybe
    <$> Hasql.interp
      [HI.sql| SELECT id FROM apis.error_patterns
        WHERE project_id = #{pid} AND service IS NOT DISTINCT FROM #{service}
          AND error_type = #{eType} AND message = #{msg}
          AND canonical_id IS NULL AND merge_override = FALSE
        ORDER BY created_at ASC LIMIT 1 |]


-- | Batch upsert error patterns using unnest arrays (single round-trip instead of N+1)
-- Groups by hash to avoid "ON CONFLICT DO UPDATE cannot affect row a second time" errors.
-- Returns hashes with their state for newly inserted ('new') or regressed patterns.
batchUpsertErrorPatterns :: DB es => Projects.ProjectId -> V.Vector ATError -> UTCTime -> Eff es [(Text, Text)]
batchUpsertErrorPatterns _pid errors _now | V.null errors = pure []
batchUpsertErrorPatterns pid errors now =
  filter (\(_, s) -> s == "new" || s == "regressed")
    <$> Hasql.interp
      [HI.sql| INSERT INTO apis.error_patterns (
            project_id, error_type, message, stacktrace, hash, parent_hash, is_framework,
            environment, service, runtime, error_data,
            first_trace_id, recent_trace_id,
            occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h)
          SELECT #{pid}, u.error_type, u.message, u.stacktrace, u.hash, u.parent_hash, u.is_framework,
                 u.environment, u.service, u.runtime, u.error_data,
                 u.trace_id, u.trace_id, u.cnt, u.cnt, u.cnt, u.cnt
          FROM (SELECT unnest(#{errorTypes}::text[]) AS error_type, unnest(#{messages}::text[]) AS message,
                       unnest(#{stacktraces}::text[]) AS stacktrace, unnest(#{hashes}::text[]) AS hash,
                       unnest(#{parentHashes}::text[]) AS parent_hash, unnest(#{isFrameworks}::bool[]) AS is_framework,
                       unnest(#{environments}::text[]) AS environment, unnest(#{services}::text[]) AS service,
                       unnest(#{runtimes}::text[]) AS runtime, unnest(#{errorDatas}::jsonb[]) AS error_data,
                       unnest(#{traceIds}::text[]) AS trace_id, unnest(#{counts}::int[]) AS cnt) u
          ON CONFLICT (project_id, hash) DO UPDATE SET
            updated_at = #{now},
            message = EXCLUDED.message,
            error_data = EXCLUDED.error_data,
            recent_trace_id = EXCLUDED.recent_trace_id,
            parent_hash = EXCLUDED.parent_hash,
            is_framework = EXCLUDED.is_framework,
            occurrences_1m = apis.error_patterns.occurrences_1m + EXCLUDED.occurrences_1m,
            occurrences_5m = apis.error_patterns.occurrences_5m + EXCLUDED.occurrences_1m,
            occurrences_1h = apis.error_patterns.occurrences_1h + EXCLUDED.occurrences_1m,
            occurrences_24h = apis.error_patterns.occurrences_24h + EXCLUDED.occurrences_1m,
            quiet_minutes = CASE WHEN apis.error_patterns.state = 'resolved' THEN 0 ELSE apis.error_patterns.quiet_minutes END,
            state = CASE WHEN apis.error_patterns.state = 'resolved' THEN 'regressed' ELSE apis.error_patterns.state END,
            regressed_at = CASE WHEN apis.error_patterns.state = 'resolved' THEN #{now} ELSE apis.error_patterns.regressed_at END,
            regression_count = CASE WHEN apis.error_patterns.state = 'resolved'
                                    THEN apis.error_patterns.regression_count + 1
                                    ELSE apis.error_patterns.regression_count END
          RETURNING hash, CASE
            WHEN xmax = 0 THEN 'new'
            WHEN state = 'regressed' AND regressed_at = #{now} THEN 'regressed'
            ELSE 'unchanged' END::text |]
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
    errorDatas = V.map HI.AsJsonb errs
    traceIds = V.map (.traceId) errs


-- | Batch upsert hourly rollup stats. Takes (hash, event_count, user_count) triples and
-- resolves error_id via JOIN on apis.error_patterns(project_id, hash).
upsertErrorPatternHourlyStats :: DB es => Projects.ProjectId -> UTCTime -> V.Vector (Text, Int, Int) -> Eff es Int64
upsertErrorPatternHourlyStats _pid _now stats | V.null stats = pure 0
upsertErrorPatternHourlyStats pid now stats =
  Hasql.interpExecute
    [HI.sql|
          INSERT INTO apis.error_hourly_stats (project_id, error_id, hour_bucket, event_count, user_count)
          SELECT e.project_id, e.id, #{hourBucket}, u.event_count, u.user_count
          FROM (SELECT unnest(#{hashes}::text[]) AS hash, unnest(#{eventCounts}::int[]) AS event_count, unnest(#{userCounts}::int[]) AS user_count) u
          JOIN apis.error_patterns e ON e.project_id = #{pid} AND e.hash = u.hash
          ON CONFLICT (project_id, error_id, hour_bucket)
          DO UPDATE SET event_count = apis.error_hourly_stats.event_count + EXCLUDED.event_count,
                        user_count = apis.error_hourly_stats.user_count + EXCLUDED.user_count |]
  where
    hourBucket = truncateHour now
    (hashes, eventCounts, userCounts) = V.unzip3 stats
