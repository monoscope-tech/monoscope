module Models.Apis.Errors (
  Error (..),
  ErrorId (..),
  ErrorState (..),
  ErrorEvent (..),
  ErrorEventId,
  ATError (..),
  ErrorL (..),
  -- Queries
  getErrors,
  getErrorById,
  getErrorByHash,
  getActiveErrors,
  updateOccurrenceCounts,
  updateErrorState,
  updateErrorStateByProjectAndHash,
  getErrorLByHash,
  updateBaseline,
  resolveError,
  upsertErrorQueryAndParam,
  assignError,
  updateErrorSubscription,
  setErrorAssignee,
  -- Error Events (for baseline/spike detection)
  HourlyBucket (..),
  ErrorEventStats (..),
  ErrorWithCurrentRate (..),
  getHourlyErrorCounts,
  getCurrentHourErrorCount,
  getErrorEventStats,
  checkErrorSpike,
  getErrorsWithCurrentRates,
  -- Error Fingerprinting (Sentry-style)
  StackFrame (..),
  parseStackTrace,
  normalizeStackTrace,
  normalizeMessage,
  computeErrorFingerprint,
)
where

import Data.Aeson qualified as AE
import Data.Char (isSpace)
import Data.Default
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (BaselineState (..))
import Relude hiding (id)
import System.Types (DB)
import Text.RE.TDFA (RE, SearchReplace, ed, (*=~/))
import Utils (DBField (MkDBField), replaceAllFormats, toXXHash)


newtype ErrorId = ErrorId {unErrorId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, NFData, Ord, ToField)


newtype ErrorEventId = ErrorEventId {unErrorEventId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, NFData, Ord, ToField)


data ErrorState
  = ESNew
  | ESEscalating
  | ESOngoing
  | ESResolved
  | ESRegressed
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ErrorState


errorStateToText :: ErrorState -> Text
errorStateToText ESNew = "new"
errorStateToText ESEscalating = "escalating"
errorStateToText ESOngoing = "ongoing"
errorStateToText ESResolved = "resolved"
errorStateToText ESRegressed = "regressed"


parseErrorState :: (Eq s, IsString s) => s -> Maybe ErrorState
parseErrorState "new" = Just ESNew
parseErrorState "escalating" = Just ESEscalating
parseErrorState "ongoing" = Just ESOngoing
parseErrorState "resolved" = Just ESResolved
parseErrorState "regressed" = Just ESRegressed
parseErrorState _ = Nothing


instance ToField ErrorState where
  toField = Escape . encodeUtf8 <$> errorStateToText


instance FromField ErrorState where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseErrorState bs of
          Just s -> pure s
          Nothing -> pure ESNew


data Error = Error
  { id :: ErrorId
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
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  , firstEventId :: Maybe ErrorEventId
  , lastEventId :: Maybe ErrorEventId
  , state :: ErrorState
  , assigneeId :: Maybe Projects.UserId
  , assignedAt :: Maybe ZonedTime
  , resolvedAt :: Maybe ZonedTime
  , regressedAt :: Maybe ZonedTime
  , subscribed :: Bool
  , notifyEveryMinutes :: Int
  , lastNotifiedAt :: Maybe ZonedTime
  , occurrences1m :: Int
  , occurrences5m :: Int
  , occurrences1h :: Int
  , occurrences24h :: Int
  , quietMinutes :: Int
  , resolutionThresholdMinutes :: Int
  , baselineState :: BaselineState
  , baselineSamples :: Int
  , baselineErrorRateMean :: Maybe Double
  , baselineErrorRateStddev :: Maybe Double
  , baselineUpdatedAt :: Maybe ZonedTime
  , isIgnored :: Bool
  , ignoredUntil :: Maybe ZonedTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "errors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Error)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Error


-- error aggreted with number of occurrences and affected users
data ErrorL = ErrorL
  { id :: ErrorId
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
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  , firstEventId :: Maybe ErrorEventId
  , lastEventId :: Maybe ErrorEventId
  , state :: ErrorState
  , assigneeId :: Maybe Projects.UserId
  , assignedAt :: Maybe ZonedTime
  , resolvedAt :: Maybe ZonedTime
  , regressedAt :: Maybe ZonedTime
  , subscribed :: Bool
  , notifyEveryMinutes :: Int
  , lastNotifiedAt :: Maybe ZonedTime
  , occurrences1m :: Int
  , occurrences5m :: Int
  , occurrences1h :: Int
  , occurrences24h :: Int
  , quietMinutes :: Int
  , resolutionThresholdMinutes :: Int
  , baselineState :: BaselineState
  , baselineSamples :: Int
  , baselineErrorRateMean :: Maybe Double
  , baselineErrorRateStddev :: Maybe Double
  , baselineUpdatedAt :: Maybe ZonedTime
  , isIgnored :: Bool
  , ignoredUntil :: Maybe ZonedTime
  , occurrences :: Int
  , affectedUsers :: Int
  , lastOccurredAt :: Maybe ZonedTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "errors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Error)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ErrorL


data ATError = ATError
  { projectId :: Maybe Projects.ProjectId
  , when :: UTCTime
  , errorType :: Text
  , rootErrorType :: Text
  , message :: Text
  , rootErrorMessage :: Text
  , stackTrace :: Text
  , hash :: Text
  , technology :: Maybe RequestDumps.SDKTypes
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
  , sessionId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson ATError
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError


data ErrorEvent = ErrorEvent
  { id :: ErrorEventId
  , projectId :: Projects.ProjectId
  , errorId :: ErrorId
  , occurredAt :: ZonedTime
  , targetHash :: Text
  , errorType :: Text
  , message :: Text
  , stackTrace :: Text
  , serviceName :: Text
  , release :: Maybe Text
  , platformVersion :: Maybe Text
  , requestMethod :: Maybe Text
  , requestPath :: Maybe Text
  , endpointHash :: Maybe Text
  , traceId :: Maybe Text
  , spanId :: Maybe Text
  , parentSpanId :: Maybe Text
  , userId :: Maybe Text
  , userEmail :: Maybe Text
  , sessionId :: Maybe Text
  , sampleRate :: Double
  , ingestionId :: Maybe UUID.UUID
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "error_events", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ErrorEvent)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ErrorEvent


-- | Get errors for a project with optional state filter
getErrors :: DB es => Projects.ProjectId -> Maybe ErrorState -> Int -> Int -> Eff es [Error]
getErrors pid mstate limit offset = PG.query q (pid, maybe "%" errorStateToText mstate, limit, offset)
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at,
               error_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               first_trace_id, recent_trace_id, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
               subscribed, notify_every_minutes, last_notified_at,
               occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h,
               quiet_minutes, resolution_threshold_minutes,
               baseline_state, baseline_samples,
               baseline_error_rate_mean, baseline_error_rate_stddev, baseline_updated_at,
               is_ignored, ignored_until
        FROM apis.errors
        WHERE project_id = ? AND state LIKE ?
        ORDER BY updated_at DESC
        LIMIT ? OFFSET ?
      |]


-- | Get error by ID
getErrorById :: DB es => ErrorId -> Eff es (Maybe Error)
getErrorById eid = do
  results <- PG.query q (Only eid)
  return $ listToMaybe results
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at,
               error_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               first_trace_id, recent_trace_id, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
               subscribed, notify_every_minutes, last_notified_at,
               occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h,
               quiet_minutes, resolution_threshold_minutes,
               baseline_state, baseline_samples,
               baseline_error_rate_mean, baseline_error_rate_stddev, baseline_updated_at,
               is_ignored, ignored_until
        FROM apis.errors
        WHERE id = ?
      |]


-- | Get error by hash
getErrorByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Error)
getErrorByHash pid hash = do
  results <- PG.query q (pid, hash)
  return $ listToMaybe results
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at,
               error_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               first_trace_id, recent_trace_id, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
               subscribed, notify_every_minutes, last_notified_at,
               occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h,
               quiet_minutes, resolution_threshold_minutes,
               baseline_state, baseline_samples,
               baseline_error_rate_mean, baseline_error_rate_stddev, baseline_updated_at,
               is_ignored, ignored_until
        FROM apis.errors
        WHERE project_id = ? AND hash = ?
      |]


getErrorLByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe ErrorL)
getErrorLByHash pid hash = do
  results <- PG.query q (pid, hash)
  return $ listToMaybe results
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at,
               error_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               first_trace_id, recent_trace_id, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
               subscribed, notify_every_minutes, last_notified_at,
               occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h,
               quiet_minutes, resolution_threshold_minutes,
               baseline_state, baseline_samples,
               baseline_error_rate_mean, baseline_error_rate_stddev, baseline_updated_at,
               is_ignored, ignored_until,
               (SELECT COUNT(*) FROM apis.error_events WHERE target_hash = e.hash) AS occurrences,
               (SELECT COUNT(DISTINCT user_id) FROM apis.error_events WHERE target_hash = e.hash) AS affected_users,
               (SELECT MAX(occurred_at) FROM apis.error_events WHERE target_hash = e.hash) AS last_occurred_at
        FROM apis.errors e
        WHERE project_id = ? AND hash = ?
      |]


-- | Get active (non-resolved) errors
getActiveErrors :: DB es => Projects.ProjectId -> Eff es [Error]
getActiveErrors pid = PG.query q (Only pid)
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at,
               error_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               first_trace_id, recent_trace_id, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
               subscribed, notify_every_minutes, last_notified_at,
               occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h,
               quiet_minutes, resolution_threshold_minutes,
               baseline_state, baseline_samples,
               baseline_error_rate_mean, baseline_error_rate_stddev, baseline_updated_at,
               is_ignored, ignored_until
        FROM apis.errors
        WHERE project_id = ? AND state != 'resolved'
        ORDER BY updated_at DESC
      |]


-- | Update occurrence counts (called periodically to decay counts)
updateOccurrenceCounts :: DB es => Eff es Int64
updateOccurrenceCounts =
  PG.execute_ q
  where
    q =
      [sql|
        UPDATE apis.errors SET
          occurrences_1m = 0,
          occurrences_5m = CASE WHEN occurrences_5m > 0 THEN occurrences_5m - occurrences_1m ELSE 0 END,
          occurrences_1h = CASE WHEN occurrences_1h > 0 THEN occurrences_1h - occurrences_5m ELSE 0 END,
          occurrences_24h = CASE WHEN occurrences_24h > 0 THEN occurrences_24h - occurrences_1h ELSE 0 END,
          quiet_minutes = quiet_minutes + 1,
          state = CASE
            WHEN state IN ('new', 'escalating', 'ongoing') AND quiet_minutes >= resolution_threshold_minutes THEN 'resolved'
            ELSE state
          END,
          resolved_at = CASE
            WHEN state IN ('new', 'escalating', 'ongoing') AND quiet_minutes >= resolution_threshold_minutes THEN NOW()
            ELSE resolved_at
          END
        WHERE state != 'resolved' OR occurrences_24h > 0
      |]


updateErrorState :: DB es => ErrorId -> ErrorState -> Eff es Int64
updateErrorState eid newState = PG.execute q (errorStateToText newState, eid)
  where
    q =
      [sql| UPDATE apis.errors SET state = ?, updated_at = NOW() WHERE id = ? |]


updateErrorStateByProjectAndHash :: DB es => Projects.ProjectId -> Text -> ErrorState -> Eff es Int64
updateErrorStateByProjectAndHash pid hash newState = PG.execute q (errorStateToText newState, pid, hash)
  where
    q =
      [sql| UPDATE apis.errors SET state = ?, updated_at = NOW() WHERE project_id = ? AND hash = ? |]


resolveError :: DB es => ErrorId -> Eff es Int64
resolveError eid = PG.execute q (Only eid)
  where
    q = [sql| UPDATE apis.errors SET state = 'resolved', resolved_at = NOW(), updated_at = NOW() WHERE id = ? |]


assignError :: DB es => ErrorId -> Projects.UserId -> Eff es Int64
assignError eid uid = PG.execute q (uid, eid)
  where
    q = [sql| UPDATE apis.errors SET assignee_id = ?, assigned_at = NOW(), updated_at = NOW() WHERE id = ? |]


setErrorAssignee :: DB es => ErrorId -> Maybe Projects.UserId -> Eff es Int64
setErrorAssignee eid assigneeIdM =
  PG.execute q (assigneeIdM, assigneeIdM, eid)
  where
    q =
      [sql|
        UPDATE apis.errors SET
          assignee_id = ?,
          assigned_at = CASE WHEN ? IS NULL THEN NULL ELSE NOW() END,
          updated_at = NOW()
        WHERE id = ?
      |]


updateErrorSubscription :: DB es => ErrorId -> Bool -> Int -> Eff es Int64
updateErrorSubscription eid subscribed notifyEveryMinutes =
  PG.execute q (subscribed, notifyEveryMinutes, subscribed, eid)
  where
    q =
      [sql|
        UPDATE apis.errors SET
          subscribed = ?,
          notify_every_minutes = ?,
          last_notified_at = CASE WHEN ? THEN NULL ELSE last_notified_at END,
          updated_at = NOW()
        WHERE id = ?
      |]


-- | Update baseline data for an error
updateBaseline :: DB es => ErrorId -> BaselineState -> Double -> Double -> Int -> Eff es Int64
updateBaseline eid bState rateMean rateStddev samples =
  PG.execute q (bState, rateMean, rateStddev, samples, eid)
  where
    q =
      [sql|
        UPDATE apis.errors
        SET baseline_state = ?,
            baseline_error_rate_mean = ?,
            baseline_error_rate_stddev = ?,
            baseline_samples = ?,
            baseline_updated_at = NOW()
        WHERE id = ?
      |]


-- | Hourly bucket for error event aggregation
data HourlyBucket = HourlyBucket
  { hourStart :: UTCTime
  , eventCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


-- | Get hourly error counts for a specific error over a time range
-- Returns counts bucketed by hour for baseline calculation
getHourlyErrorCounts :: DB es => ErrorId -> Int -> Eff es [HourlyBucket]
getHourlyErrorCounts eid hoursBack =
  PG.query q (eid, hoursBack)
  where
    q =
      [sql|
        SELECT
          date_trunc('hour', occurred_at) AS hour_start,
          COUNT(*)::INT AS event_count
        FROM apis.error_events
        WHERE error_id = ?
          AND occurred_at >= NOW() - INTERVAL '1 hour' * ?
        GROUP BY date_trunc('hour', occurred_at)
        ORDER BY hour_start
      |]


-- | Get current hour error count for a specific error
getCurrentHourErrorCount :: DB es => ErrorId -> Eff es Int
getCurrentHourErrorCount eid = do
  results <- PG.query q (Only eid)
  case results of
    [Only count] -> return count
    _ -> return 0
  where
    q =
      [sql|
        SELECT COUNT(*)::INT
        FROM apis.error_events
        WHERE error_id = ?
          AND occurred_at >= date_trunc('hour', NOW())
      |]


-- | Get error event stats for baseline calculation
-- Returns median and MAD (Median Absolute Deviation) of hourly counts over the lookback period
-- Using median + MAD instead of mean + stddev for robustness against outliers/spikes
data ErrorEventStats = ErrorEventStats
  { hourlyMedian :: Double -- Actually stores median for robustness
  , hourlyMADScaled :: Double -- Actually stores MAD * 1.4826 (scaled to be comparable to stddev)
  , totalHours :: Int
  , totalEvents :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


getErrorEventStats :: DB es => ErrorId -> Int -> Eff es (Maybe ErrorEventStats)
getErrorEventStats eid hoursBack = do
  results <- PG.query q (eid, hoursBack)
  return $ listToMaybe results
  where
    q =
      [sql|
        WITH hourly_counts AS (
          SELECT
            date_trunc('hour', occurred_at) AS hour_start,
            COUNT(*) AS event_count
          FROM apis.error_events
          WHERE error_id = ?
            AND occurred_at >= NOW() - INTERVAL '1 hour' * ?
          GROUP BY date_trunc('hour', occurred_at)
        ),
        median_calc AS (
          SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY event_count) AS median_val
          FROM hourly_counts
        ),
        mad_calc AS (
          SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(hc.event_count - mc.median_val)) AS mad_val
          FROM hourly_counts hc, median_calc mc
        )
        SELECT
          COALESCE(mc.median_val, 0)::FLOAT AS hourly_median,
          COALESCE(mad.mad_val * 1.4826, 0)::FLOAT AS hourly_mad_scaled,
          (SELECT COUNT(*)::INT FROM hourly_counts) AS total_hours,
          (SELECT COALESCE(SUM(event_count), 0)::INT FROM hourly_counts) AS total_events
        FROM median_calc mc, mad_calc mad
      |]


-- | Check if an error is spiking compared to its baseline
-- Returns (isSpike, currentRate, zScore) if baseline is established
checkErrorSpike :: DB es => Error -> Eff es (Maybe (Bool, Double, Double))
checkErrorSpike err = do
  case (err.baselineState, err.baselineErrorRateMean, err.baselineErrorRateStddev) of
    (BSEstablished, Just mean, Just stddev) | stddev > 0 -> do
      currentCount <- getCurrentHourErrorCount err.id
      let currentRate = fromIntegral currentCount :: Double
          zScore = (currentRate - mean) / stddev
          -- Spike: > 3 std devs AND at least 5 more than mean (avoid noise on low-volume errors)
          isSpike = zScore > 3.0 && currentRate > mean + 5
      return $ Just (isSpike, currentRate, zScore)
    _ -> return Nothing


-- | Get all errors with their current hour counts (for batch spike detection)
data ErrorWithCurrentRate = ErrorWithCurrentRate
  { errorId :: ErrorId
  , projectId :: Projects.ProjectId
  , errorType :: Text
  , message :: Text
  , service :: Maybe Text
  , baselineState :: BaselineState
  , baselineMean :: Maybe Double
  , baselineStddev :: Maybe Double
  , currentHourCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


getErrorsWithCurrentRates :: DB es => Projects.ProjectId -> Eff es [ErrorWithCurrentRate]
getErrorsWithCurrentRates pid =
  PG.query q (pid)
  where
    q =
      [sql|
        SELECT
          e.id,
          e.project_id,
          e.error_type,
          e.message,
          e.service,
          e.baseline_state,
          e.baseline_error_rate_mean,
          e.baseline_error_rate_stddev,
          COALESCE(counts.current_count, 0)::INT AS current_hour_count
        FROM apis.errors e
        LEFT JOIN (
          SELECT error_id, COUNT(*) AS current_count
          FROM apis.error_events
          WHERE occurred_at >= date_trunc('hour', NOW())
          GROUP BY error_id
        ) counts ON counts.error_id = e.id
        WHERE e.project_id = ?
          AND e.state != 'resolved'
          AND e.is_ignored = false
      |]


-- | Upsert an error (insert or update on conflict)
upsertErrorQueryAndParam :: Projects.ProjectId -> ATError -> (Query, [DBField])
upsertErrorQueryAndParam pid err = (q, params)
  where
    q =
      [sql|
        INSERT INTO apis.errors (
          project_id, error_type, message, stacktrace, hash,
          environment, service, runtime, error_data,
          first_trace_id, recent_trace_id,
          occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 1, 1, 1, 1)
        ON CONFLICT (project_id, hash) DO UPDATE SET
          updated_at = NOW(),
          message = EXCLUDED.message,
          error_data = EXCLUDED.error_data,
          recent_trace_id = EXCLUDED.recent_trace_id,
          occurrences_1m = apis.errors.occurrences_1m + 1,
          occurrences_5m = apis.errors.occurrences_5m + 1,
          occurrences_1h = apis.errors.occurrences_1h + 1,
          occurrences_24h = apis.errors.occurrences_24h + 1,
          state = CASE
            WHEN apis.errors.state = 'resolved' THEN 'regressed'
            ELSE apis.errors.state
          END,
          regressed_at = CASE
            WHEN apis.errors.state = 'resolved' THEN NOW()
            ELSE apis.errors.regressed_at
          END
      |]
    params =
      [ MkDBField err.projectId
      , MkDBField err.errorType
      , MkDBField err.message
      , MkDBField err.stackTrace
      , MkDBField err.hash
      , MkDBField err.environment
      , MkDBField err.serviceName
      , MkDBField err.runtime
      , MkDBField err
      , MkDBField err.traceId
      , MkDBField err.traceId
      ]


-- =============================================================================
-- Error Fingerprinting
-- =============================================================================
-- Fingerprinting priority:
-- 1. Stack trace (if available and has in-app frames)
-- 2. Exception type + normalized message
-- 3. Normalized message only (fallback)
--
-- Stack trace normalization extracts:
-- - Module/package name
-- - Function name (cleaned per platform)
-- - Context line (whitespace normalized, max 120 chars)
--
-- Message normalization:
-- - Limits to first 2 non-empty lines
-- - Replaces UUIDs, IPs, emails, timestamps, numbers with placeholders

-- | Represents a parsed stack frame
data StackFrame = StackFrame
  { sfFilePath :: Text
  , sfModule :: Maybe Text
  , sfFunction :: Text
  , sfLineNumber :: Maybe Int
  , sfColumnNumber :: Maybe Int
  , sfContextLine :: Maybe Text
  , sfIsInApp :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Parse a stack trace into a list of stack frames based on runtime
--
-- >>> length $ parseStackTrace "nodejs" "at processTicksAndRejections (node:internal/process/task_queues:95:5)\nat handleRequest (/app/src/server.js:42:15)"
-- 2
--
-- >>> map sfFunction $ parseStackTrace "python" "File \"/app/main.py\", line 10, in main\nFile \"/app/utils.py\", line 5, in helper"
-- ["main","helper"]
--
-- >>> map sfIsInApp $ parseStackTrace "nodejs" "at handleRequest (/app/src/server.js:42:15)\nat processTicksAndRejections (node:internal/process/task_queues:95:5)"
-- [True,False]
--
-- >>> map sfFunction $ parseStackTrace "java" "at com.example.MyClass.doWork(MyClass.java:25)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1067)"
-- ["doWork","doDispatch"]
parseStackTrace :: Text -> Text -> [StackFrame]
parseStackTrace mSdk stackText =
  let lns = filter (not . T.null . T.strip) $ T.lines stackText
   in mapMaybe (parseStackFrame mSdk) lns


-- | Parse a single stack frame line based on SDK type
parseStackFrame :: Text -> Text -> Maybe StackFrame
parseStackFrame runtime line =
  let trimmed = T.strip line
   in case runtime of
        "go" -> parseGoFrame trimmed
        "nodejs" -> parseJsFrame trimmed
        "webjs" -> parseJsFrame trimmed
        "python" -> parsePythonFrame trimmed
        "java" -> parseJavaFrame trimmed
        "php" -> parsePhpFrame trimmed
        "dotnet" -> parseDotNetFrame trimmed
        _ -> parseGenericFrame trimmed


-- | Parse Go stack frame: "goroutine 1 [running]:" or "main.foo(0x1234)"
-- Format: package.function(args) or /path/to/file.go:123 +0x1f
parseGoFrame :: Text -> Maybe StackFrame
parseGoFrame line
  | "goroutine" `T.isPrefixOf` line = Nothing -- Skip goroutine headers
  | ".go:" `T.isInfixOf` line =
      -- File path line: /path/to/file.go:123 +0x1f
      let (pathPart, _) = T.breakOn " +" line
          (filePath, lineCol) = T.breakOnEnd ":" pathPart
          lineNum = readMaybe $ toString $ T.takeWhile (/= ':') lineCol
       in Just
            StackFrame
              { sfFilePath = T.dropEnd 1 filePath -- Remove trailing ':'
              , sfModule = extractGoModule filePath
              , sfFunction = ""
              , sfLineNumber = lineNum
              , sfColumnNumber = Nothing
              , sfContextLine = Nothing
              , sfIsInApp = isGoInApp filePath
              }
  | "(" `T.isInfixOf` line =
      -- Function call line: main.foo(0x1234, 0x5678)
      let (funcPart, _) = T.breakOn "(" line
       in Just
            StackFrame
              { sfFilePath = ""
              , sfModule = extractGoModuleFromFunc funcPart
              , sfFunction = extractGoFuncName funcPart
              , sfLineNumber = Nothing
              , sfColumnNumber = Nothing
              , sfContextLine = Nothing
              , sfIsInApp = isGoFuncInApp funcPart
              }
  | otherwise = Nothing
  where
    extractGoModule path =
      let parts = T.splitOn "/" path
       in if length parts > 2
            then Just $ T.intercalate "/" $ take 3 parts
            else Nothing

    extractGoModuleFromFunc func =
      let parts = T.splitOn "." func
       in if length parts > 1
            then T.intercalate "." <$> viaNonEmpty init parts
            else Nothing

    extractGoFuncName func =
      let parts = T.splitOn "." func
       in fromMaybe func $ viaNonEmpty last parts

    isGoInApp path =
      not
        $ any
          (`T.isInfixOf` path)
          ["go/src/", "pkg/mod/", "vendor/", "/runtime/", "/net/", "/syscall/"]

    isGoFuncInApp func =
      not
        $ any
          (`T.isPrefixOf` func)
          ["runtime.", "syscall.", "net.", "net/http.", "reflect."]


-- | Parse JavaScript stack frame
-- Formats:
--   at functionName (filePath:line:col)
--   at filePath:line:col
--   at async functionName (filePath:line:col)
parseJsFrame :: Text -> Maybe StackFrame
parseJsFrame line
  | "at " `T.isPrefixOf` T.strip line =
      let content = T.strip $ T.drop 3 $ T.strip line
          -- Handle "at async ..."
          content' =
            if "async " `T.isPrefixOf` content
              then T.drop 6 content
              else content
       in if "(" `T.isInfixOf` content'
            then parseJsWithParens content'
            else parseJsWithoutParens content'
  | otherwise = Nothing
  where
    parseJsWithParens txt =
      let (funcPart, rest) = T.breakOn " (" txt
          locationPart = T.dropAround (\c -> c == '(' || c == ')') rest
          (filePath, lineCol) = parseJsLocation locationPart
          (lineNum, colNum) = parseLineCol lineCol
       in Just
            StackFrame
              { sfFilePath = filePath
              , sfModule = extractJsModule filePath
              , sfFunction = cleanJsFunction funcPart
              , sfLineNumber = lineNum
              , sfColumnNumber = colNum
              , sfContextLine = Nothing
              , sfIsInApp = isJsInApp filePath
              }

    parseJsWithoutParens txt =
      let (filePath, lineCol) = parseJsLocation txt
          (lineNum, colNum) = parseLineCol lineCol
       in Just
            StackFrame
              { sfFilePath = filePath
              , sfModule = extractJsModule filePath
              , sfFunction = "<anonymous>"
              , sfLineNumber = lineNum
              , sfColumnNumber = colNum
              , sfContextLine = Nothing
              , sfIsInApp = isJsInApp filePath
              }

    parseJsLocation loc =
      -- Split from the right to handle paths with colons (Windows)
      let parts = T.splitOn ":" loc
          n = length parts
       in if n >= 3
            then (T.intercalate ":" $ take (n - 2) parts, T.intercalate ":" $ drop (n - 2) parts)
            else (loc, "")

    parseLineCol lc =
      let parts = T.splitOn ":" lc
       in case parts of
            [l, c] -> (readMaybe $ toString l, readMaybe $ toString c)
            [l] -> (readMaybe $ toString l, Nothing)
            _ -> (Nothing, Nothing)

    extractJsModule path =
      let baseName = fromMaybe path $ viaNonEmpty last $ T.splitOn "/" path
       in Just
            $ T.toLower
            $ fromMaybe baseName
            $ T.stripSuffix ".js" baseName
            <|> T.stripSuffix ".ts" baseName
            <|> T.stripSuffix ".mjs" baseName
            <|> T.stripSuffix ".cjs" baseName

    cleanJsFunction func =
      -- Remove namespacing: Object.foo.bar -> bar
      let parts = T.splitOn "." func
       in fromMaybe func $ viaNonEmpty last parts

    isJsInApp path =
      not
        $ any
          (`T.isInfixOf` path)
          ["node_modules/", "<anonymous>", "internal/", "node:"]


-- | Parse Python stack frame
-- Format: File "path/to/file.py", line 123, in function_name
parsePythonFrame :: Text -> Maybe StackFrame
parsePythonFrame line
  | "File \"" `T.isPrefixOf` T.strip line =
      let content = T.drop 6 $ T.strip line -- Remove 'File "'
          (filePath, rest) = T.breakOn "\"" content
          -- Parse ", line 123, in func_name"
          parts = T.splitOn ", " $ T.drop 2 rest -- Skip '",'
          lineNum = case find ("line " `T.isPrefixOf`) parts of
            Just p -> readMaybe $ toString $ T.drop 5 p
            Nothing -> Nothing
          funcName = case find ("in " `T.isPrefixOf`) parts of
            Just p -> T.drop 3 p
            Nothing -> "<module>"
       in Just
            StackFrame
              { sfFilePath = filePath
              , sfModule = extractPythonModule filePath
              , sfFunction = cleanPythonFunction funcName
              , sfLineNumber = lineNum
              , sfColumnNumber = Nothing
              , sfContextLine = Nothing
              , sfIsInApp = isPythonInApp filePath
              }
  | otherwise = Nothing
  where
    extractPythonModule path =
      let baseName = fromMaybe path $ viaNonEmpty last $ T.splitOn "/" path
          moduleName = fromMaybe baseName $ T.stripSuffix ".py" baseName
       in Just moduleName

    cleanPythonFunction func =
      -- Remove lambda indicators
      T.replace "<lambda>" "lambda"
        $ T.replace "<listcomp>" "listcomp"
        $ T.replace "<dictcomp>" "dictcomp" func

    isPythonInApp path =
      not
        $ any
          (`T.isInfixOf` path)
          ["site-packages/", "dist-packages/", "/lib/python", "<frozen"]


-- | Parse Java stack frame
-- Format: at com.example.Class.method(File.java:123)
parseJavaFrame :: Text -> Maybe StackFrame
parseJavaFrame line
  | "at " `T.isPrefixOf` T.strip line =
      let content = T.drop 3 $ T.strip line
          (qualifiedMethod, rest) = T.breakOn "(" content
          locationPart = T.dropAround (\c -> c == '(' || c == ')') rest
          (fileName, lineNum) = parseJavaLocation locationPart
          (moduleName, funcName) = splitJavaQualified qualifiedMethod
       in Just
            StackFrame
              { sfFilePath = fileName
              , sfModule = Just moduleName
              , sfFunction = cleanJavaFunction funcName
              , sfLineNumber = lineNum
              , sfColumnNumber = Nothing
              , sfContextLine = Nothing
              , sfIsInApp = isJavaInApp qualifiedMethod
              }
  | otherwise = Nothing
  where
    parseJavaLocation loc =
      let (file, lineStr) = T.breakOn ":" loc
       in (file, readMaybe $ toString $ T.drop 1 lineStr)

    splitJavaQualified qualified =
      let parts = T.splitOn "." qualified
       in if length parts > 1
            then case (viaNonEmpty init parts, viaNonEmpty last parts) of
              (Just ps, Just l) -> (T.intercalate "." ps, l)
              _ -> ("", qualified)
            else ("", qualified)

    cleanJavaFunction func =
      -- Remove generics: method<T> -> method
      T.takeWhile (/= '<') func

    isJavaInApp qualified =
      not
        $ any
          (`T.isPrefixOf` qualified)
          ["java.", "javax.", "sun.", "com.sun.", "jdk.", "org.springframework."]


-- | Parse PHP stack frame
-- Format: #0 /path/to/file.php(123): ClassName->methodName()
parsePhpFrame :: Text -> Maybe StackFrame
parsePhpFrame line
  | "#" `T.isPrefixOf` T.strip line =
      let content = T.drop 1 $ T.dropWhile (/= ' ') $ T.strip line -- Skip "#N "
          (pathPart, funcPart) = T.breakOn ": " content
          (filePath, lineNum) = parsePhpPath pathPart
          funcName = T.takeWhile (/= '(') $ T.drop 2 funcPart
       in Just
            StackFrame
              { sfFilePath = filePath
              , sfModule = extractPhpModule filePath
              , sfFunction = cleanPhpFunction funcName
              , sfLineNumber = lineNum
              , sfColumnNumber = Nothing
              , sfContextLine = Nothing
              , sfIsInApp = isPhpInApp filePath
              }
  | otherwise = Nothing
  where
    parsePhpPath path =
      let (file, lineStr) = T.breakOn "(" path
          lineNum = readMaybe $ toString $ T.takeWhile (/= ')') $ T.drop 1 lineStr
       in (file, lineNum)

    extractPhpModule path =
      let baseName = fromMaybe path $ viaNonEmpty last $ T.splitOn "/" path
       in Just $ fromMaybe baseName $ T.stripSuffix ".php" baseName

    cleanPhpFunction func =
      -- Remove {closure} markers
      T.replace "{closure}" "closure"
        $
        -- Simplify class::method or class->method
        let parts = T.splitOn "->" func
         in if length parts > 1
              then fromMaybe func $ viaNonEmpty last parts
              else
                let parts' = T.splitOn "::" func
                 in if length parts' > 1 then fromMaybe func $ viaNonEmpty last parts' else func

    isPhpInApp path =
      not
        $ any
          (`T.isInfixOf` path)
          ["/vendor/", "/phar://"]


-- | Parse .NET stack frame
-- Format: at Namespace.Class.Method(params) in /path/to/file.cs:line 123
parseDotNetFrame :: Text -> Maybe StackFrame
parseDotNetFrame line
  | "at " `T.isPrefixOf` T.strip line =
      let content = T.drop 3 $ T.strip line
          (methodPart, locationPart) = T.breakOn " in " content
          qualifiedMethod = T.takeWhile (/= '(') methodPart
          (moduleName, funcName) = splitDotNetQualified qualifiedMethod
          (filePath, lineNum) = parseDotNetLocation $ T.drop 4 locationPart
       in Just
            StackFrame
              { sfFilePath = filePath
              , sfModule = Just moduleName
              , sfFunction = cleanDotNetFunction funcName
              , sfLineNumber = lineNum
              , sfColumnNumber = Nothing
              , sfContextLine = Nothing
              , sfIsInApp = isDotNetInApp qualifiedMethod
              }
  | otherwise = Nothing
  where
    splitDotNetQualified qualified =
      let parts = T.splitOn "." qualified
       in if length parts > 1
            then case (viaNonEmpty init parts, viaNonEmpty last parts) of
              (Just ps, Just l) -> (T.intercalate "." ps, l)
              _ -> ("", qualified)
            else ("", qualified)

    parseDotNetLocation loc =
      let (path, lineStr) = T.breakOn ":line " loc
       in (path, readMaybe $ toString $ T.drop 6 lineStr)

    cleanDotNetFunction func =
      -- Remove generic arity: Method`1 -> Method
      T.takeWhile (/= '`') func

    isDotNetInApp qualified =
      not
        $ any
          (`T.isPrefixOf` qualified)
          ["System.", "Microsoft.", "Newtonsoft."]


-- | Generic stack frame parser for unknown formats
parseGenericFrame :: Text -> Maybe StackFrame
parseGenericFrame line =
  let trimmed = T.strip line
   in if T.null trimmed || "..." `T.isPrefixOf` trimmed
        then Nothing
        else
          Just
            StackFrame
              { sfFilePath = trimmed
              , sfModule = Nothing
              , sfFunction = extractGenericFunction trimmed
              , sfLineNumber = extractGenericLineNumber trimmed
              , sfColumnNumber = Nothing
              , sfContextLine = Nothing
              , sfIsInApp = True -- Assume in-app by default
              }
  where
    extractGenericFunction txt =
      -- Try to find function-like patterns
      let parts = T.words txt
       in case find (\p -> "(" `T.isInfixOf` p || "." `T.isInfixOf` p) parts of
            Just p -> T.takeWhile (/= '(') p
            Nothing -> fromMaybe "" $ listToMaybe parts

    extractGenericLineNumber txt =
      -- Look for :NUMBER or line NUMBER patterns
      let parts = T.splitOn ":" txt
       in case parts of
            _ : rest -> listToMaybe $ mapMaybe (readMaybe . toString . T.takeWhile (/= ':')) rest
            _ -> Nothing


-- | Normalize a stack trace for fingerprinting
-- Returns a list of normalized frame strings suitable for hashing
--
-- >>> normalizeStackTrace "nodejs" "at handleRequest (/app/src/server.js:42:15)\nat processTicksAndRejections (node:internal/process/task_queues:95:5)"
-- "server|handleRequest"
--
-- >>> normalizeStackTrace "python" "File \"/app/main.py\", line 10, in main\nFile \"/app/utils.py\", line 5, in helper"
-- "main|main\nutils|helper"
--
-- >>> normalizeStackTrace "java" "at com.example.MyClass.doWork(MyClass.java:25)\nat com.example.Service.process(Service.java:50)"
-- "com.example.MyClass|doWork\ncom.example.Service|process"
--
-- >>> normalizeStackTrace "unknown" "some random frame info"
-- "some"
normalizeStackTrace :: Text -> Text -> Text
normalizeStackTrace runtime stackText =
  let frames = parseStackTrace runtime stackText
      inAppFrames = filter sfIsInApp frames
      framesToUse = if null inAppFrames then frames else inAppFrames
      normalizedFrames = map normalizeFrame framesToUse
   in T.intercalate "\n" normalizedFrames
  where
    normalizeFrame :: StackFrame -> Text
    normalizeFrame frame =
      let modulePart = fromMaybe "" frame.sfModule
          funcPart = normalizeFunction frame.sfFunction
          -- Context line: normalize whitespace, truncate if > 120 chars
          contextPart = maybe "" normalizeContextLine frame.sfContextLine
       in T.intercalate "|" $ filter (not . T.null) [modulePart, funcPart, contextPart]

    normalizeFunction :: Text -> Text
    normalizeFunction func =
      -- Common normalizations across platforms:
      -- 1. Remove memory addresses (0x...)
      -- 2. Remove generic type parameters
      -- 3. Remove parameter types
      -- 4. Normalize anonymous/lambda markers
      let noAddr = T.unwords $ filter (not . ("0x" `T.isPrefixOf`)) $ T.words func
          noGenerics = T.takeWhile (/= '<') noAddr
          noParams = T.takeWhile (/= '(') noGenerics
       in T.strip noParams

    normalizeContextLine :: Text -> Text
    normalizeContextLine ctx =
      let normalized = T.unwords $ T.words ctx
       in if T.length normalized > 120
            then "" -- Skip overly long context lines (like Sentry does)
            else normalized


-- | Normalize an error message for fingerprinting
-- Limits to first 2 non-empty lines and replaces variable content
--
-- >>> normalizeMessage "Connection refused to 192.168.1.100:5432"
-- "Connection refused to {ipv4}{port}"
--
-- >>> normalizeMessage "User c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd not found"
-- "User {uuid} not found"
--
-- >>> normalizeMessage "Error 404: Resource 12345 not available\n\nDetails here\nMore info"
-- "Error {integer}: Resource {integer} not available Details here"
--
-- >>> normalizeMessage "   Trimmed message   "
-- "Trimmed message"
normalizeMessage :: Text -> Text
normalizeMessage msg =
  let lns = take 2 $ filter (not . T.null . T.strip) $ T.lines msg
      combined = T.intercalate " " $ map T.strip lns
      -- Replace variable patterns with placeholders
      normalized = replaceAllFormats combined
   in T.strip normalized


-- | Compute the error fingerprint hash using Sentry-style prioritization
-- Priority:
-- 1. Stack trace (if has meaningful in-app frames)
-- 2. Exception type + message
-- 3. Message only
--
-- With stack trace - uses projectId, exceptionType, and normalized stack:
-- >>> computeErrorFingerprint "proj1" (Just "svc") (Just "span") "nodejs" "TypeError" "msg" "at handler (/app/index.js:10:5)"
-- "269748a1"
--
-- Without stack trace but with exception type - uses projectId, service, span, type, and message:
-- >>> computeErrorFingerprint "proj1" (Just "user-service") (Just "/api/users") "nodejs" "ValidationError" "Invalid email format" ""
-- "af0aa163"
--
-- Without stack trace or exception type - uses projectId, service, span, and message only:
-- >>> computeErrorFingerprint "proj1" (Just "api") (Just "/health") "nodejs" "" "Connection refused to 192.168.1.1:5432" ""
-- "44e50418"
--
-- Same error from different IPs produces same fingerprint (IP normalized):
-- >>> computeErrorFingerprint "proj1" (Just "db") Nothing "python" "" "Connection refused to 10.0.0.1:5432" "" == computeErrorFingerprint "proj1" (Just "db") Nothing "python" "" "Connection refused to 172.16.0.50:5432" ""
-- True
--
-- Same stack trace produces same fingerprint regardless of message:
-- >>> computeErrorFingerprint "proj1" Nothing Nothing "nodejs" "Error" "message 1" "at handler (/app/index.js:10:5)" == computeErrorFingerprint "proj1" Nothing Nothing "nodejs" "Error" "different message" "at handler (/app/index.js:10:5)"
-- True
computeErrorFingerprint :: Text -> Maybe Text -> Maybe Text -> Text -> Text -> Text -> Text -> Text
computeErrorFingerprint projectIdText mService spanName runtime exceptionType message stackTrace =
  let
    -- Normalize components
    normalizedStack = normalizeStackTrace runtime stackTrace
    normalizedMsg = normalizeMessage message
    normalizedType = T.strip exceptionType

    -- Build fingerprint components based on priority
    fingerprintComponents =
      if hasUsableStackTrace normalizedStack
        then
          [ projectIdText
          , normalizedType
          , normalizedStack
          ]
        else
          if not (T.null normalizedType)
            then
              [ projectIdText
              , fromMaybe "" mService
              , fromMaybe "" spanName
              , normalizedType
              , normalizedMsg
              ]
            else
              [ projectIdText
              , fromMaybe "" mService
              , fromMaybe "" spanName
              , normalizedMsg
              ]

    -- Combine and hash
    combined = T.intercalate "|" $ filter (not . T.null) fingerprintComponents
   in
    toXXHash combined
  where
    hasUsableStackTrace :: Text -> Bool
    hasUsableStackTrace normalized =
      let lns = T.lines normalized
          nonEmptyLines = filter (not . T.null . T.strip) lns
       in length nonEmptyLines >= 1
