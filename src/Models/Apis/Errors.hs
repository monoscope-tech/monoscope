module Models.Apis.Errors (
  Error (..),
  ErrorId,
  ErrorState (..),
  ErrorEvent (..),
  ErrorEventId,
  ATError (..),
  -- Queries
  getErrors,
  getErrorById,
  getErrorByHash,
  getActiveErrors,
  updateOccurrenceCounts,
  updateErrorState,
  updateBaseline,
  resolveError,
  assignError,
  -- Error Events (for baseline/spike detection)
  HourlyBucket (..),
  ErrorEventStats (..),
  ErrorWithCurrentRate (..),
  getHourlyErrorCounts,
  getCurrentHourErrorCount,
  getErrorEventStats,
  checkErrorSpike,
  getErrorsWithCurrentRates,
)
where

import Data.Aeson qualified as AE
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Relude hiding (id)
import System.Types (DB)
import Utils (DBField (MkDBField))
import Models.Apis.RequestDumps qualified as RequestDumps
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Data.Default


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
  , exceptionType :: Text
  , message :: Text
  , stacktrace :: Text
  , hash :: Text
  , environment :: Text
  , service :: Maybe Text
  , runtime :: Maybe Text
  , errorData :: ATError
  , representativeMessage :: Maybe Text
  , firstEventId :: Maybe ErrorEventId
  , lastEventId :: Maybe ErrorEventId
  , state :: ErrorState
  , assigneeId :: Maybe Users.UserId
  , assignedAt :: Maybe ZonedTime
  , resolvedAt :: Maybe ZonedTime
  , regressedAt :: Maybe ZonedTime
  , occurrences1m :: Int
  , occurrences5m :: Int
  , occurrences1h :: Int
  , occurrences24h :: Int
  , quietMinutes :: Int
  , resolutionThresholdMinutes :: Int
  , baselineState :: Text
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


data ATError = ATError
  { projectId :: Maybe Projects.ProjectId
  , when :: UTCTime
  , errorType :: Text
  , rootErrorType :: Text
  , message :: Text
  , rootErrorMessage :: Text
  , stackTrace :: Text
  , hash :: Maybe Text
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
  , userIp :: Maybe Text
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
  , exceptionType :: Text
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
  , userIp :: Maybe Text
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
               exception_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               representative_message, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
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
               exception_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               representative_message, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
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
               exception_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               representative_message, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
               occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h,
               quiet_minutes, resolution_threshold_minutes,
               baseline_state, baseline_samples,
               baseline_error_rate_mean, baseline_error_rate_stddev, baseline_updated_at,
               is_ignored, ignored_until
        FROM apis.errors
        WHERE project_id = ? AND hash = ?
      |]


-- | Get active (non-resolved) errors
getActiveErrors :: DB es => Projects.ProjectId -> Eff es [Error]
getActiveErrors pid = PG.query q (Only pid)
  where
    q =
      [sql|
        SELECT id, project_id, created_at, updated_at,
               exception_type, message, stacktrace, hash,
               environment, service, runtime, error_data,
               representative_message, first_event_id, last_event_id,
               state, assignee_id, assigned_at, resolved_at, regressed_at,
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


resolveError :: DB es => ErrorId -> Eff es Int64
resolveError eid = PG.execute q (Only eid)
  where
    q = [sql| UPDATE apis.errors SET state = 'resolved', resolved_at = NOW(), updated_at = NOW() WHERE id = ? |]


assignError :: DB es => ErrorId -> Users.UserId -> Eff es Int64
assignError eid uid = PG.execute q (uid, eid)
  where
    q = [sql| UPDATE apis.errors SET assignee_id = ?, assigned_at = NOW(), updated_at = NOW() WHERE id = ? |]


-- | Update baseline data for an error
updateBaseline :: DB es => ErrorId -> Text -> Double -> Double -> Int -> Eff es Int64
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
-- Returns mean and stddev of hourly counts over the lookback period
data ErrorEventStats = ErrorEventStats
  { hourlyMean :: Double
  , hourlyStddev :: Double
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
        )
        SELECT
          COALESCE(AVG(event_count), 0)::FLOAT AS hourly_mean,
          COALESCE(STDDEV(event_count), 0)::FLOAT AS hourly_stddev,
          COUNT(*)::INT AS total_hours,
          COALESCE(SUM(event_count), 0)::INT AS total_events
        FROM hourly_counts
      |]


-- | Check if an error is spiking compared to its baseline
-- Returns (isSpike, currentRate, zScore) if baseline is established
checkErrorSpike :: DB es => Error -> Eff es (Maybe (Bool, Double, Double))
checkErrorSpike err = do
  case (err.baselineState, err.baselineErrorRateMean, err.baselineErrorRateStddev) of
    ("established", Just mean, Just stddev) | stddev > 0 -> do
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
  , exceptionType :: Text
  , message :: Text
  , service :: Maybe Text
  , baselineState :: Text
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
          e.exception_type,
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
upsertErrorQueryAndParam :: DB es => Projects.ProjectId -> RequestDump.ATError -> (Query, [DBField])
upsertErrorQueryAndParam pid err = (q, params)
  where
    q =
      [sql|
        INSERT INTO apis.errors (
          project_id, exception_type, message, stacktrace, hash,
          environment, service, runtime, error_data, occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1, 1, 1, 1)
        ON CONFLICT (hash) DO UPDATE SET
          updated_at = NOW(),
          representative_message = EXCLUDED.message,
          occurrences_1m = apis.errors.occurrences_1m + 1,
          occurrences_5m = apis.errors.occurrences_5m + 1,
          occurrences_1h = apis.errors.occurrences_1h + 1,
          occurrences_24h = apis.errors.occurrences_24h + 1,
          state = CASE
            WHEN apis.errors.state = 'resolved' THEN 'regressed'
            WHEN apis.errors.state = 'new' AND apis.errors.occurrences_1h > 10 THEN 'escalating'
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
      ]
