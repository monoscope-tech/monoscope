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
  updateOccurrenceCounts,
  updateErrorState,
  updateErrorStateByProjectAndHash,
  getErrorLByHash,
  bulkCalculateAndUpdateBaselines,
  resolveError,
  upsertErrorQueryAndParam,
  updateErrorSubscription,
  updateErrorThreadIds,
  updateErrorThreadIdsAndNotifiedAt,
  setErrorAssignee,
  -- Error Events (for spike detection)
  ErrorWithCurrentRate (..),
  getErrorsWithCurrentRates,
  -- Error Fingerprinting (re-exported from Pkg.ErrorFingerprint)
  EF.StackFrame (..),
  EF.parseStackTrace,
  EF.normalizeStackTrace,
  EF.normalizeMessage,
  EF.computeErrorFingerprint,
)
where

import Data.Aeson qualified as AE
import Data.Default
import Data.Time
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity (_selectWhere)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow qualified as FR
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Simple.Types (Query)
import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.LogPatterns (BaselineState (..))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (WrappedEnumSC (..))
import Pkg.ErrorFingerprint qualified as EF
import Relude hiding (id)
import System.Types (DB)
import Utils (DBField (MkDBField))


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
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, ToField) via WrappedEnumSC "ES" ErrorState


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
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "errors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Error)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Error


-- error aggregated with number of occurrences and affected users
data ErrorL = ErrorL
  { base :: Error
  , occurrences :: Int
  , affectedUsers :: Int
  , lastOccurredAt :: Maybe ZonedTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


instance FromRow ErrorL where
  fromRow = ErrorL <$> FR.fromRow <*> FR.field <*> FR.field <*> FR.field


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
  , environment :: Maybe Text
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
getErrors pid mstate limit offset = case mstate of
  Nothing -> PG.query (_selectWhere @Error [[field| project_id |]] <> " ORDER BY updated_at DESC LIMIT ? OFFSET ?") (pid, limit, offset)
  Just st -> PG.query (_selectWhere @Error [[field| project_id |], [field| state |]] <> " ORDER BY updated_at DESC LIMIT ? OFFSET ?") (pid, st, limit, offset)


-- | Get error by ID
getErrorById :: DB es => ErrorId -> Eff es (Maybe Error)
getErrorById eid = listToMaybe <$> PG.query (_selectWhere @Error [[field| id |]]) (Only eid)


-- | Get error by hash
getErrorByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Error)
getErrorByHash pid hash = listToMaybe <$> PG.query (_selectWhere @Error [[field| project_id |], [field| hash |]]) (pid, hash)


-- NOTE: Column list must match Error record fields. If Error changes, update this query.
getErrorLByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe ErrorL)
getErrorLByHash pid hash = listToMaybe <$> PG.query q (pid, pid, hash)
  where
    q =
      [sql|
        SELECT e.id, e.project_id, e.created_at, e.updated_at,
               e.error_type, e.message, e.stacktrace, e.hash,
               e.environment, e.service, e.runtime, e.error_data,
               e.first_trace_id, e.recent_trace_id, e.first_event_id, e.last_event_id,
               e.state, e.assignee_id, e.assigned_at, e.resolved_at, e.regressed_at,
               e.subscribed, e.notify_every_minutes, e.last_notified_at,
               e.occurrences_1m, e.occurrences_5m, e.occurrences_1h, e.occurrences_24h,
               e.quiet_minutes, e.resolution_threshold_minutes,
               e.baseline_state, e.baseline_samples,
               e.baseline_error_rate_mean, e.baseline_error_rate_stddev, e.baseline_updated_at,
               e.is_ignored, e.ignored_until,
               e.slack_thread_ts, e.discord_message_id,
               COALESCE(ev.occurrences, 0)::INT,
               COALESCE(ev.affected_users, 0)::INT,
               ev.last_occurred_at
        FROM apis.errors e
        LEFT JOIN (
          SELECT target_hash, COUNT(*) AS occurrences, COUNT(DISTINCT user_id) AS affected_users, MAX(occurred_at) AS last_occurred_at
          FROM apis.error_events WHERE project_id = ? GROUP BY target_hash
        ) ev ON ev.target_hash = e.hash
        WHERE e.project_id = ? AND e.hash = ?
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
          occurrences_5m = GREATEST(0, occurrences_5m - occurrences_1m),
          occurrences_1h = GREATEST(0, occurrences_1h - occurrences_5m),
          occurrences_24h = GREATEST(0, occurrences_24h - occurrences_1h),
          quiet_minutes = quiet_minutes + 1,
          state = CASE
            WHEN state IN ('new', 'escalating', 'ongoing') AND quiet_minutes + 1 >= resolution_threshold_minutes THEN 'resolved'
            ELSE state
          END,
          resolved_at = CASE
            WHEN state IN ('new', 'escalating', 'ongoing') AND quiet_minutes + 1 >= resolution_threshold_minutes THEN NOW()
            ELSE resolved_at
          END
        WHERE state != 'resolved' OR occurrences_24h > 0
      |]


updateErrorState :: DB es => ErrorId -> ErrorState -> Eff es Int64
updateErrorState eid newState = PG.execute q (newState, eid)
  where
    q =
      [sql| UPDATE apis.errors SET state = ?, updated_at = NOW() WHERE id = ? |]


updateErrorStateByProjectAndHash :: DB es => Projects.ProjectId -> Text -> ErrorState -> Eff es Int64
updateErrorStateByProjectAndHash pid hash newState = PG.execute q (newState, pid, hash)
  where
    q =
      [sql| UPDATE apis.errors SET state = ?, updated_at = NOW() WHERE project_id = ? AND hash = ? |]


resolveError :: DB es => ErrorId -> Eff es Int64
resolveError eid = PG.execute q (Only eid)
  where
    q = [sql| UPDATE apis.errors SET state = 'resolved', resolved_at = NOW(), updated_at = NOW() WHERE id = ? |]


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


updateErrorThreadIds :: DB es => ErrorId -> Maybe Text -> Maybe Text -> Eff es Int64
updateErrorThreadIds eid slackTs discordMsgId =
  PG.execute q (slackTs, discordMsgId, eid)
  where
    q =
      [sql|
        UPDATE apis.errors SET
          slack_thread_ts = COALESCE(?, slack_thread_ts),
          discord_message_id = COALESCE(?, discord_message_id),
          updated_at = NOW()
        WHERE id = ?
      |]


updateErrorThreadIdsAndNotifiedAt :: DB es => ErrorId -> Maybe Text -> Maybe Text -> Eff es Int64
updateErrorThreadIdsAndNotifiedAt eid slackTs discordMsgId =
  PG.execute q (slackTs, discordMsgId, eid)
  where
    q =
      [sql|
        UPDATE apis.errors SET
          slack_thread_ts = COALESCE(?, slack_thread_ts),
          discord_message_id = COALESCE(?, discord_message_id),
          last_notified_at = NOW(),
          updated_at = NOW()
        WHERE id = ?
      |]


-- | Bulk-update baselines for all active errors in a project using a single SQL CTE.
-- Computes median/MAD per error from error_events over the last 168 hours.
bulkCalculateAndUpdateBaselines :: DB es => Projects.ProjectId -> Eff es Int64
bulkCalculateAndUpdateBaselines pid =
  PG.execute
    [sql|
      WITH hourly_counts AS (
        SELECT ee.error_id, date_trunc('hour', ee.occurred_at) AS hour_start, COUNT(*) AS event_count
        FROM apis.error_events ee
        JOIN apis.errors e ON e.id = ee.error_id
        WHERE e.project_id = ? AND e.state != 'resolved' AND ee.occurred_at >= NOW() - INTERVAL '168 hours'
        GROUP BY ee.error_id, date_trunc('hour', ee.occurred_at)
      ),
      stats AS (
        SELECT error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY event_count) AS median_val,
          COUNT(*) AS total_hours
        FROM hourly_counts GROUP BY error_id
      ),
      mad AS (
        SELECT hc.error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(hc.event_count - s.median_val)) * 1.4826 AS mad_scaled
        FROM hourly_counts hc JOIN stats s ON s.error_id = hc.error_id
        GROUP BY hc.error_id
      )
      UPDATE apis.errors SET
        baseline_error_rate_mean = s.median_val,
        baseline_error_rate_stddev = COALESCE(m.mad_scaled, 0),
        baseline_samples = s.total_hours::INT,
        baseline_state = CASE WHEN s.total_hours >= 24 THEN 'established' ELSE 'learning' END,
        baseline_updated_at = NOW()
      FROM stats s LEFT JOIN mad m ON m.error_id = s.error_id
      WHERE apis.errors.id = s.error_id AND apis.errors.project_id = ?
    |]
    (pid, pid)


-- | Get all errors with their current hour counts (for batch spike detection)
data ErrorWithCurrentRate = ErrorWithCurrentRate
  { errorId :: ErrorId
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
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


getErrorsWithCurrentRates :: DB es => Projects.ProjectId -> Eff es [ErrorWithCurrentRate]
getErrorsWithCurrentRates pid =
  PG.query q (pid, pid)
  where
    q =
      [sql|
        SELECT
          e.id,
          e.project_id,
          e.error_type,
          e.message,
          e.service,
          e.state,
          e.baseline_state,
          e.baseline_error_rate_mean,
          e.baseline_error_rate_stddev,
          COALESCE(counts.current_count, 0)::INT AS current_hour_count,
          e.error_data,
          e.stacktrace,
          e.hash,
          e.slack_thread_ts,
          e.discord_message_id
        FROM apis.errors e
        LEFT JOIN (
          SELECT error_id, COUNT(*) AS current_count
          FROM apis.error_events
          WHERE project_id = ? AND occurred_at >= date_trunc('hour', NOW())
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
          quiet_minutes = CASE
            WHEN apis.errors.state = 'resolved' THEN 0
            ELSE apis.errors.quiet_minutes
          END,
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
      [ MkDBField pid
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
