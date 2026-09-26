-- | An issue is the one thing a user triages, and the only object any of this is
-- read from. Everything else in this area is either a detector feeding issues or
-- machinery delivering notifications about them:
--
--   * detectors — @apis.anomalies@ (API change, see "Models.Apis.ApiChanges"),
--     error patterns, log patterns, and query monitors. Evidence, not triage.
--   * issues — this module. One row per signal, carrying its whole history.
--   * delivery — "Models.Apis.Incidents": alert episodes and the Slack thread
--     ledger. An episode is a chapter of an issue and hangs off @issue_id@; it
--     has no page of its own and is read from the issue's timeline.
--
-- Issue types, one per detector: @ApiChange@ groups endpoint\/shape\/format drift
-- by endpoint; @RuntimeException@ is one issue per error pattern; @QueryAlert@ is
-- a monitor threshold breach, long-lived and reopened rather than duplicated per
-- firing; @LogPattern@ and @LogPatternRateChange@ cover log signals.
--
-- On the API-change detector specifically:
-- - docs\/anomaly-detection-system.md (architecture overview)
-- - docs\/anomaly-detection-triggers.sql (database trigger details)
module Models.Apis.Issues (
  IssuePayload (..),
  payloadType,
  payloadJson,
  parsePayload,
  issuePayload,

  -- * Core Types
  IssueId,
  IssueType (..),
  Issue (..),
  IssueL (..),

  -- * Issue Data Types
  APIChangeData (..),
  RuntimeExceptionData (..),
  QueryAlertData (..),
  ThresholdDirection (..),
  ChatMessageKind (..),
  IssueSeverity (..),
  LogPatternRateChangeData (..),
  LogPatternData (..),
  RateChangeDirection (..),

  -- * Database Operations
  insertIssue,
  insertIssueReturningId,
  reopenOrInsertIssueTx,
  selectIssueById,
  selectIssues,
  IssueProjection (..),
  IssueFilters (..),
  NullFilter (..),
  defIssueFilters,
  applyIssueScope,
  updateIssueWithNewAnomaly,
  updateIssueEnhancement,
  updateIssueCriticality,
  AckWindow (..),
  AckSet (..),
  ackUntil,
  indefiniteUntil,
  isSilenced,
  setAckState,
  ackCascade,
  expireAcks,
  setArchiveState,
  ArchiveWindow (..),
  expireArchives,
  wakeOnEscalation,
  setIssuePriority,
  SavedView (..),
  PerfKind (..),
  FrontendKind (..),
  UptimeData (..),
  CronFailure (..),
  CronData (..),
  createCronIssue,
  cronTargetHash,
  createUptimeIssue,
  uptimeTargetHash,
  FrontendData (..),
  FeedbackData (..),
  createFeedbackIssue,
  setSpamState,
  createFrontendIssue,
  PerformanceData (..),
  createPerformanceIssue,
  selectIssueViews,
  saveIssueView,
  deleteIssueView,
  setIssueAssignee,
  autoArchiveStaleDiscoveryIssues,
  selectIssueByHash,
  IssueScope (..),
  reopenIssue,
  bumpIssueUpdatedAt,
  issueNotifyDedupHours,

  -- * Conversion Functions
  createAPIChangeIssue,
  createQueryAlertIssue,
  createLogPatternIssue,
  createLogPatternRateChangeIssue,
  createNewErrorIssue,
  createErrorSpikeIssue,
  SpikeResult (..),

  -- * Utilities
  parseIssueType,
  hashPrefix,
  defaultRecommendedAction,
  isBoilerplateAction,
  queryAlertRecommendedAction,
  serviceLabel,
  showRounded,
  showRate,
  showPct,
  isNewEndpointOnly,

  -- * AI Conversations
  AIConversation (..),
  AIChatMessage (..),
  ConversationSummary (..),
  RoutineInterval,
  RoutineCategory (..),
  RoutineTemplate (..),
  RoutineSchedule,
  RoutineReport (..),
  RoutineDestination (..),
  RoutineRunStatus (..),
  RoutineRunCompletion (..),
  routineTemplates,
  routineIntervalMinutes,
  routineCadence,
  routineCategoryLabel,
  routineScheduleLabel,
  mkRoutineInterval,
  ScheduledRoutine (..),
  ClaimedRoutine (..),
  RoutineRun (..),
  ConversationType (..),
  getOrCreateConversation,
  listConversations,
  getConversation,
  conversationIssueId,
  conversationNeedsTitle,
  setConversationTitle,
  renameConversation,
  deleteConversation,
  upsertRoutine,
  installRoutineTemplate,
  claimRoutine,
  finishRoutineRun,
  listRoutineRuns,
  completeRoutine,
  listDueRoutines,
  pauseRoutine,
  cancelRoutineRun,
  resumeRoutine,
  deleteRoutine,
  setRoutineDestination,
  routineCanAct,
  routineRunCancelled,
  insertChatMessage,
  selectChatHistory,
  prepareSlackTurn,
  seedChatHistory,

  -- * Thread ID Helpers
  slackThreadToConversationId,
  slackScopedConversationId,
  textToConversationId,

  -- * Activity Log
  IssueEvent (..),
  EpisodeKind (..),
  ActivityEvent (..),
  parseActivityEvent,
  IssueActivity (..),
  logIssueActivity,
  recordIssueView,
  selectLatestStateEvent,
  selectIssueActivity,

  -- * Issue Summary (for reports/emails)
  IssueSummary (..),

  -- * Reports
  Report (..),
  ReportId,
  ReportListItem (..),
  addReport,
  reportHistoryByProject,
  getReportById,
  getLatestReportByType,
) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Char (isAscii, isPrint)
import Data.Default (Default)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID (UUIDEff, genUUID)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Time (Day (ModifiedJulianDay), DayOfWeek (..), UTCTime (..), addDays, addUTCTime, dayOfWeek, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Zones (utcTZ, utcToLocalTimeTZ)
import Data.Time.Zones.All qualified as TZ
import Data.UUID qualified as UUID
import Data.UUID.V5 qualified as UUID5
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Records (HasField)
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.ApiChanges (PayloadChange)
import Models.Apis.ApiChanges qualified as ApiChanges
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.LogPatterns (RateChangeDirection (..))
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), decodeEnumSC, rawSql, selectFrom)
import Pkg.Parser (ScopedQuery (..))
import Relude hiding (id)
import Servant (FromHttpApiData (..), ServerError, err500, errBody)
import System.Logging (logAttention)
import System.Types (DB)
import Utils (toXXHash)


type IssueId = UUIDId "issue"


-- | Issue types
data IssueType
  = ApiChange
  | RuntimeException
  | QueryAlert
  | LogPattern
  | LogPatternRateChange
  | Performance
  | Frontend
  | Uptime
  | Cron
  | Feedback
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField, ToSchema) via WrappedEnumSC ('Just "apis.issue_type") "" IssueType


-- | Why a cron monitor opened an issue.
data CronFailure = CFMissed | CFFailed
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display) via WrappedEnumSC 'Nothing "CF" CronFailure


-- | What a user wrote (@feedback.message@), who they are (@user.*@ / @feedback.contact_email@),
-- where (@url.full@ / @page.url@), and the error issue sharing its trace, if any.
data FeedbackData = FeedbackData
  { message :: Text
  , contactEmail :: Maybe Text
  , userName :: Maybe Text
  , pageUrl :: Maybe Text
  , sessionId :: Maybe Text
  , traceId :: Maybe Text
  , relatedErrorHash :: Maybe Text
  -- ^ The error issue's target hash (no @err:@ prefix), for @/issues/by_hash/…@.
  , observedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake FeedbackData


-- | A cron monitor that missed its window or checked in with @monitor.status = error@.
data CronData = CronData
  { monitorId :: Text
  , slug :: Text
  , name :: Text
  , failure :: CronFailure
  , expectedBy :: UTCTime
  , lastCheckinAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake CronData


-- | A failed uptime check: what was probed, what came back, and since when.
data UptimeData = UptimeData
  { checkId :: Text
  , name :: Text
  , url :: Text
  , expectedStatus :: Int
  , statusCode :: Maybe Int
  , reason :: Text
  , durationMs :: Int
  , downSince :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake UptimeData


-- | A frontend issue's shape, detected over the browser SDK's interaction spans.
data FrontendKind = FKRageClick | FKDeadClick
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display) via WrappedEnumSC 'Nothing "FK" FrontendKind


-- | The clicked element (@monoscope.display.label@, @target.*@), the page it was on
-- (@page.url@), and one sample session and trace.
data FrontendData = FrontendData
  { kind :: FrontendKind
  , element :: Text
  , selector :: Maybe Text
  , pageUrl :: Maybe Text
  , clickCount :: Int
  , sessionId :: Text
  , traceId :: Text
  , observedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake FrontendData


-- | A performance issue's shape, detected over completed traces' database spans.
data PerfKind = PKNPlusOne | PKSlowQuery
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display) via WrappedEnumSC 'Nothing "PK" PerfKind


-- | Span evidence for a performance issue: the query (@db.query.summary@ or a
-- normalised @db.query.text@), where it ran, and what it cost in one sample trace.
data PerformanceData = PerformanceData
  { kind :: PerfKind
  , transaction :: Maybe Text
  , parentSpan :: Maybe Text
  , query :: Text
  , dbSystem :: Maybe Text
  , repeatCount :: Int
  , durationImpactMs :: Double
  , traceId :: Text
  , observedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake PerformanceData


-- | Hash prefix used in otel_logs_and_spans hashes column
hashPrefix :: IssueType -> Maybe Text
hashPrefix = \case
  LogPattern -> Just "pat:"
  LogPatternRateChange -> Just "pat:"
  RuntimeException -> Just "err:"
  ApiChange -> Just "" -- endpoint hash is stored unprefixed on span hashes
  QueryAlert -> Nothing
  Performance -> Nothing -- detected over span shapes; spans carry no issue hash
  Frontend -> Nothing
  Uptime -> Nothing
  Cron -> Nothing
  Feedback -> Nothing


defaultRecommendedAction :: Text
defaultRecommendedAction = "Review the changes and update your integration accordingly."


-- | Every canned @recommendedAction@ we write when the LLM enhancer has not yet
-- produced a real one. They say nothing an on-call reader does not already know,
-- and they render in the page's subtitle slot — where Sentry and Datadog both put
-- the culprit — so the detail page suppresses them.
--
-- >>> isBoilerplateAction defaultRecommendedAction
-- True
-- >>> isBoilerplateAction "Review the query results and take appropriate action."
-- True
-- >>> isBoilerplateAction "Roll back the shipping service to 1.4.2."
-- False
isBoilerplateAction :: Text -> Bool
isBoilerplateAction a = a `elem` ([defaultRecommendedAction, queryAlertRecommendedAction] :: [Text])


queryAlertRecommendedAction :: Text
queryAlertRecommendedAction = "Review the query results and take appropriate action."


parseIssueType :: Text -> Maybe IssueType
parseIssueType = rightToMaybe . parseUrlPiece


-- | Issue severity. Encodes to "critical"/"warning"/"info"/"low", byte-identical
-- to the previous free-text column. Migration 0105 backfills legacy empty/NULL
-- rows and sets NOT NULL DEFAULT 'info', so the column is always one of these.
data IssueSeverity = Critical | Warning | Info | Low
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "" IssueSeverity


data IssueSummary = IssueSummary
  { id :: IssueId
  , title :: Text
  , critical :: Bool
  , severity :: IssueSeverity
  , issueType :: IssueType
  , activityBuckets :: Maybe [Int]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] IssueSummary


-- | Rounded-to-integer number with a unit suffix — the only numeric formatting
-- issue titles and prompts use.
--
-- >>> (showRounded "" (2.6 :: Double), showRate (2.4 :: Double), showPct (99.5 :: Double))
-- ("3","2/hr","100%")
showRounded :: RealFrac a => Text -> a -> Text
showRounded unit x = show (round x :: Int) <> unit


showRate, showPct :: RealFrac a => a -> Text
showRate = showRounded "/hr"
showPct = showRounded "%"


serviceLabel :: Maybe Text -> Text
serviceLabel = fromMaybe "unknown-service"


isNewEndpointOnly :: Issue -> Bool
isNewEndpointOnly issue = case issuePayload issue of
  Just (ApiChangeP d) -> all V.null ([d.newFields, d.deletedFields, d.modifiedFields] :: [V.Vector Text])
  _ -> False


-- | API Change issue data
data APIChangeData = APIChangeData
  { endpointMethod :: Text
  , endpointPath :: Text
  , endpointHost :: Text
  , anomalyHashes :: V.Vector Text
  , shapeChanges :: V.Vector AE.Value -- Simplified for now
  , formatChanges :: V.Vector AE.Value -- Simplified for now
  , newFields :: V.Vector Text
  , deletedFields :: V.Vector Text
  , modifiedFields :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson APIChangeData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake APIChangeData


-- | Runtime Exception issue data
data RuntimeExceptionData = RuntimeExceptionData
  { errorType :: Text
  , errorMessage :: Text
  , stackTrace :: Text
  , requestPath :: Maybe Text
  , requestMethod :: Maybe Text
  , occurrenceCount :: Int
  , firstSeen :: UTCTime
  , lastSeen :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson RuntimeExceptionData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake RuntimeExceptionData


-- | Whether an alert fires above or below its threshold. Encodes to
-- "above"/"below" (byte-identical to the previous free-text field).
data ThresholdDirection = Above | Below
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display) via WrappedEnumSC 'Nothing "" ThresholdDirection


-- | Query Alert issue data
data QueryAlertData = QueryAlertData
  { queryId :: Text
  , queryName :: Text
  , queryExpression :: Text
  , thresholdValue :: Double
  , actualValue :: Double
  , thresholdType :: ThresholdDirection
  , triggeredAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson QueryAlertData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake QueryAlertData


-- | Main Issue type
data Issue = Issue
  { id :: IssueId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , issueType :: IssueType
  , endpointHash :: Text
  , acknowledgedAt :: Maybe ZonedTime
  , acknowledgedBy :: Maybe Projects.UserId
  , archivedAt :: Maybe ZonedTime
  , title :: Text
  , service :: Maybe Text
  , critical :: Bool
  , severity :: IssueSeverity
  , affectedRequests :: Int
  , affectedClients :: Int
  , errorRate :: Maybe Double
  , recommendedAction :: Text
  , migrationComplexity :: Text -- "low", "medium", "high", "n/a"
  , issueData :: Aeson AE.Value
  , requestPayloads :: Aeson [PayloadChange]
  , responsePayloads :: Aeson [PayloadChange]
  , llmEnhancedAt :: Maybe UTCTime
  , llmEnhancementVersion :: Maybe Int
  , targetHash :: Text
  , environment :: Maybe Text
  , seqNum :: Int
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , -- Columns added after the original 0007 schema (cooldown_until 0075,
    -- last_notified_at 0085, acknowledged_until 0125). Field order no longer has
    -- to track attnum: every read goes through 'selectFrom' @Issue, which emits
    -- the column list from these fields. Read this row with @SELECT *@ and the
    -- next @ADD COLUMN@ breaks decoding on any binary that hasn't been redeployed
    -- yet — a 500 on issue reads for the length of a rolling deploy.
    cooldownUntil :: Maybe ZonedTime
  , lastNotifiedAt :: Maybe ZonedTime
  , acknowledgedUntil :: Maybe ZonedTime
  -- ^ End of the acknowledgement window (0125). Always set alongside
  -- @acknowledged_at@; 'indefiniteUntil' for an indefinite ack.
  , assigneeId :: Maybe Projects.UserId
  , archivedUntil :: Maybe ZonedTime
  , archiveUntilEscalating :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "issues", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Issue)


-- | Issue with aggregated event data (for list views).
-- The leading columns must match Issue's field declaration order (Generic DecodeRow).
data IssueL = IssueL
  { base :: Issue
  , eventCount :: Int
  , lastSeen :: UTCTime
  , latestStateEvent :: Maybe IssueEvent
  , activityBuckets :: V.Vector Int
  , usersCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


-- Generic HI.DecodeRow can't derive this: Issue has DecodeRow but not DecodeValue.
instance HI.DecodeRow IssueL where
  decodeRow = IssueL <$> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow


-- | Insert a single issue
-- ON CONFLICT dedup applies to all issue types on (project_id, target_hash, issue_type)
-- but only for open issues (not acknowledged/archived). Preserves occurrence_count and first_seen.
insertIssue :: DB es => Issue -> Eff es ()
insertIssue = void . insertIssueReturningId


-- | Return the persisted identity, including when this insert updates an open issue.
insertIssueReturningId :: DB es => Issue -> Eff es IssueId
insertIssueReturningId = fmap HI.getOneRow . Hasql.interp . insertIssueSql


insertIssueReturningIdTx :: Issue -> Tx.Transaction IssueId
insertIssueReturningIdTx = fmap HI.getOneRow . Tx.statement () . HI.interp True . insertIssueSql


-- | The issue a recurring signal belongs to: the existing one, reopened, or a
-- fresh row if the signal has never fired.
--
-- 'insertIssueSql' already upserts while an issue is /open/ — its partial unique
-- index covers only open rows — so a monitor that fired, was acknowledged, then
-- fired again used to start a brand new issue and strand its history on the old
-- one. A monitor's @target_hash@ is its id, so there is exactly one signal here
-- and it deserves exactly one long-lived row carrying every episode.
--
-- An acknowledgement is a snooze, not a dismissal: while @acknowledged_until@ is
-- still in the future the row keeps its ack and only its activity is bumped, so a
-- firing inside the window cannot un-silence what someone deliberately quieted.
-- Past that window the row is reopened and the upsert merges the fresh reading.
reopenOrInsertIssueTx :: UTCTime -> Issue -> Tx.Transaction IssueId
reopenOrInsertIssueTx now issue = do
  prior <-
    Hasql.queryTx @[IssueId]
      [HI.sql| SELECT id FROM apis.issues
               WHERE project_id = #{issue.projectId} AND target_hash = #{issue.targetHash}
                 AND issue_type = #{issue.issueType}::apis.issue_type
               ORDER BY created_at DESC LIMIT 1 |]
  case listToMaybe prior of
    Nothing -> insertIssueReturningIdTx issue
    Just iid -> do
      -- The window is re-checked inside the UPDATE rather than read first and
      -- acted on after: a user can acknowledge between the two, and clearing then
      -- would silently revert the snooze they had just asked for. No rows updated
      -- therefore means "still snoozed", which is also the honest answer when the
      -- ack landed a millisecond ago.
      --
      -- Clearing ack/archive is what lets the upsert below find the row at all:
      -- 'insertIssueSql's conflict target only covers open issues.
      cleared <-
        Hasql.queryTx @[IssueId]
          [HI.sql| UPDATE apis.issues SET acknowledged_at = NULL, acknowledged_by = NULL,
                     acknowledged_until = NULL, archived_at = NULL
                   WHERE id = #{iid} AND (acknowledged_until IS NULL OR acknowledged_until <= #{now})
                   RETURNING id |]
      if null cleared
        then iid <$ Hasql.executeTx (touchIssueSqlWith mempty now iid)
        else insertIssueReturningIdTx issue


insertIssueSql :: Issue -> HI.Sql
insertIssueSql i =
  [HI.sql|
INSERT INTO apis.issues (
  id, created_at, updated_at, project_id, issue_type, target_hash, parent_hash, is_framework, endpoint_hash,
  acknowledged_at, acknowledged_by, archived_at,
  title, service, environment, critical, severity,
  recommended_action, migration_complexity,
  issue_data, request_payloads, response_payloads,
  llm_enhanced_at, llm_enhancement_version, seq_num,
  affected_requests, affected_clients
) VALUES (#{i.id}, #{i.createdAt}, #{i.updatedAt}, #{i.projectId}, #{i.issueType}::apis.issue_type, #{i.targetHash}, #{i.parentHash}, #{i.isFramework}, #{i.endpointHash},
  #{i.acknowledgedAt}, #{i.acknowledgedBy}, #{i.archivedAt}, #{i.title}, #{i.service}, #{i.environment}, #{i.critical}, #{i.severity},
  #{i.recommendedAction}, #{i.migrationComplexity}, #{i.issueData}, #{i.requestPayloads}, #{i.responsePayloads}, #{i.llmEnhancedAt}, #{i.llmEnhancementVersion}, #{i.seqNum},
  #{max 1 i.affectedRequests}, #{max 1 i.affectedClients})
ON CONFLICT (project_id, target_hash, issue_type)
  WHERE acknowledged_at IS NULL AND archived_at IS NULL
DO UPDATE SET
  updated_at = EXCLUDED.updated_at,
  title = CASE WHEN apis.issues.issue_type = 'query_alert' THEN EXCLUDED.title ELSE apis.issues.title END,
  affected_requests = apis.issues.affected_requests + EXCLUDED.affected_requests,
  affected_clients = apis.issues.affected_clients + EXCLUDED.affected_clients,
  issue_data = EXCLUDED.issue_data
    || CASE WHEN jsonb_exists(apis.issues.issue_data, 'occurrence_count')
       THEN jsonb_build_object('occurrence_count', (apis.issues.issue_data->>'occurrence_count')::bigint + COALESCE((EXCLUDED.issue_data->>'occurrence_count')::bigint, 1))
       ELSE '{}'::jsonb END
    || CASE WHEN jsonb_exists(apis.issues.issue_data, 'first_seen')
       THEN jsonb_build_object('first_seen', apis.issues.issue_data->'first_seen')
       ELSE '{}'::jsonb END
    || CASE WHEN jsonb_exists(apis.issues.issue_data, 'first_seen_at')
       THEN jsonb_build_object('first_seen_at', apis.issues.issue_data->'first_seen_at')
       ELSE '{}'::jsonb END
RETURNING id
    |]


-- | Select issue by ID, scoped to its project: an id from another tenant reads as
-- 'Nothing' rather than leaking the row.
selectIssueById :: DB es => Projects.ProjectId -> IssueId -> Eff es (Maybe Issue)
selectIssueById pid iid =
  Hasql.interpOne (selectFrom @Issue <> [HI.sql| WHERE id = #{iid} AND project_id = #{pid} |])


-- | Which recurrence of a target hash 'selectIssueByHash' returns: the most recently
-- active one of any type, the newest-created one of a type (including acked/archived),
-- or the single open one of a type — 'insertIssue's partial unique index guarantees at
-- most one open row per (project, target, type), so that case needs no ordering.
data IssueScope = AnyIssue | OfType IssueType | OpenOfType IssueType
  deriving stock (Eq, Show)


selectIssueByHash :: DB es => Projects.ProjectId -> Text -> IssueScope -> Eff es (Maybe Issue)
selectIssueByHash pid tgtHash scope =
  Hasql.interpOne
    $ selectFrom @Issue
    <> [HI.sql| WHERE project_id = #{pid} AND target_hash = #{tgtHash}|]
    <> case scope of
      AnyIssue -> [HI.sql| ORDER BY updated_at DESC, id DESC|]
      OfType ty -> [HI.sql| AND issue_type = #{ty}::apis.issue_type ORDER BY created_at DESC|]
      OpenOfType ty -> [HI.sql| AND issue_type = #{ty}::apis.issue_type AND acknowledged_at IS NULL AND archived_at IS NULL|]
    <> [HI.sql| LIMIT 1 |]


-- | Bump updated_at and occurrence count; @extra@ appends further SET clauses
-- (each written with a leading comma).
touchIssue :: (DB es, Time :> es) => HI.Sql -> IssueId -> Eff es ()
touchIssue extra issueId = Time.currentTime >>= \now -> Hasql.interpExecute_ (touchIssueSqlWith extra now issueId)


touchIssueSqlWith :: HI.Sql -> UTCTime -> IssueId -> HI.Sql
touchIssueSqlWith extra now issueId =
  [HI.sql| UPDATE apis.issues SET updated_at = #{now}^{extra},
          issue_data = issue_data || jsonb_build_object('occurrence_count',
            COALESCE((issue_data->>'occurrence_count')::bigint, 1) + 1)
        WHERE id = #{issueId} |]


-- | Reopen a previously acknowledged/archived issue (clear ack/archive, bump occurrence count)
reopenIssue :: (DB es, Time :> es) => IssueId -> Eff es ()
reopenIssue = touchIssue [HI.sql|, acknowledged_at = NULL, acknowledged_by = NULL, acknowledged_until = NULL, archived_at = NULL|]


-- | Bump updated_at and occurrence count without clearing ack/archive (for already-open issues)
bumpIssueUpdatedAt :: (DB es, Time :> es) => IssueId -> Eff es ()
bumpIssueUpdatedAt = touchIssue mempty


-- | Tri-state predicate on a nullable column (@acknowledged_at@, @archived_at@).
data NullFilter = AnyValue | IsNull | IsNotNull
  deriving stock (Eq, Show)


-- | @AND pfx.col IS [NOT] NULL@ clause, or empty when the filter is 'AnyValue'.
sqlNullFilter :: HI.Sql -> HI.Sql -> NullFilter -> HI.Sql
sqlNullFilter pfx col = \case
  AnyValue -> mempty
  IsNull -> [HI.sql| AND ^{pfx}^{col} IS NULL|]
  IsNotNull -> [HI.sql| AND ^{pfx}^{col} IS NOT NULL|]


-- | Which row shape 'selectIssues' projects. 'PIssueL' adds per-issue event counts,
-- last-state event and activity buckets (the HTML list); 'PIssue' is the plain row
-- (the public API). The filter/count half is shared either way.
data IssueProjection r where
  PIssueL :: IssueProjection IssueL
  PIssue :: IssueProjection Issue


-- | Filter/pagination surface of the issue list. Named fields rather than a run of
-- positional @Maybe Bool@\/@Maybe Text@ arguments, which were silently swappable.
data IssueFilters = IssueFilters
  { ack :: NullFilter
  , archive :: NullFilter
  , spam :: NullFilter
  , services :: [Text]
  , service :: Maybe Text
  , environment :: Maybe Text
  , types :: [Text]
  , timeRange :: Maybe (UTCTime, UTCTime)
  , order :: Maybe Text
  -- ^ @-col@\/@+col@ over created_at\/updated_at\/title; anything else falls back to critical-first.
  , period :: Text
  -- ^ activity-bucket granularity, 'PIssueL' only: @"24h"@ = 24 hourly buckets, else 7 daily.
  , hideLowSeverity :: Bool
  -- ^ Inbox behaviour: drop @severity = 'low'@ so demoted silent drops don't clutter the list.
  , limit :: Int
  , offset :: Int
  }
  deriving stock (Generic, Show)


defIssueFilters :: IssueFilters
defIssueFilters =
  IssueFilters
    { ack = AnyValue
    , archive = AnyValue
    , spam = AnyValue
    , services = []
    , service = Nothing
    , environment = Nothing
    , types = []
    , timeRange = Nothing
    , order = Nothing
    , period = "7d"
    , hideLowSeverity = False
    , limit = 50
    , offset = 0
    }


-- | Adapt the app-wide investigation boundary to the issue store's filter shape.
-- Global and page-local service filters remain independent, so their intersection is
-- applied rather than one silently replacing the other.
-- Issues have no trace column, so trace selection remains available to the subsequent
-- evidence hand-off rather than broadening this list query.
--
-- >>> let t = UTCTime (ModifiedJulianDay 0) 0
-- >>> let scope = ScopedQuery (UUIDId $ UUID5.generateNamed UUID5.namespaceOID []) (Just t, Just t) (Just "production") (Just "checkout") (Just "trace-1")
-- >>> let filtered = applyIssueScope scope defIssueFilters{services = ["catalog"], environment = Just "staging"}
-- >>> (filtered.services, filtered.service, filtered.environment, filtered.timeRange == Just (t, t))
-- (["catalog"],Just "checkout",Just "production",True)
applyIssueScope :: ScopedQuery -> IssueFilters -> IssueFilters
applyIssueScope scope filters =
  filters
    { service = scope.service
    , environment = scope.environment
    , timeRange = case scope.timeRange of
        (Just fromTime, Just toTime) -> Just (fromTime, toTime)
        _ -> filters.timeRange
    }


-- | Select issues with filters, returning the rows and the total count for pagination.
selectIssues :: (DB es, Time :> es) => Projects.ProjectId -> IssueProjection r -> IssueFilters -> Eff es ([r], Int)
selectIssues pid projection f = do
  now <- Time.currentTime
  -- seriesStart/step are bound here (not via SQL NOW()) so charts honour the test clock.
  let (seriesStart, stepSql) = case f.period of
        "24h" -> (addUTCTime (-(23 * 3600)) now, [HI.sql|interval '1 hour'|])
        _ -> (UTCTime (addDays (-6) (utctDay now)) 0, [HI.sql|interval '1 day'|])
      orderBy pfx = rawSql case T.uncons =<< f.order of
        Just (s, c) | s == '-' || s == '+', c `elem` ["created_at", "updated_at", "title"] -> pfx <> c <> bool " ASC" " DESC" (s == '-')
        -- Output aliases of the IssueL projection; only that query (prefix "i.") has them.
        Just (s, c) | s == '-' || s == '+', pfx == "i.", c `elem` ["event_count", "users_count"] -> c <> bool " ASC" " DESC" (s == '-') <> ", i.created_at DESC"
        _ -> pfx <> "critical DESC, " <> pfx <> "created_at DESC"
      arrF pfx col xs = if null xs then mempty else [HI.sql| AND ^{pfx}^{col} = ANY(#{xs}::text[])|]
      mkFilters pfx =
        foldMap (\(s, e) -> [HI.sql| AND ^{pfx}created_at >= #{s} AND ^{pfx}created_at <= #{e}|]) f.timeRange
          <> sqlNullFilter pfx [HI.sql|acknowledged_at|] f.ack
          <> sqlNullFilter pfx [HI.sql|archived_at|] f.archive
          <> sqlNullFilter pfx [HI.sql|spam_at|] f.spam
          <> bool mempty [HI.sql| AND (^{pfx}severity IS NULL OR ^{pfx}severity != 'low')|] f.hideLowSeverity
          <> arrF pfx [HI.sql|service|] f.services
          <> foldMap (\service -> [HI.sql| AND ^{pfx}service = #{service}|]) f.service
          <> foldMap (\environment -> [HI.sql| AND ^{pfx}environment = #{environment}|]) f.environment
          <> arrF pfx [HI.sql|issue_type::text|] f.types
      iFilters = mkFilters [HI.sql|i.|]
      cFilters = mkFilters mempty
  issues <- case projection of
    PIssue ->
      Hasql.interp
        $ selectFrom @Issue
        <> [HI.sql| WHERE project_id = #{pid} ^{cFilters} ORDER BY ^{orderBy ""} LIMIT #{f.limit} OFFSET #{f.offset} |]
    PIssueL ->
      Hasql.interp
        [HI.sql|
        SELECT i.id, i.created_at, i.updated_at, i.project_id, i.issue_type,
          i.endpoint_hash, i.acknowledged_at, i.acknowledged_by, i.archived_at, i.title, i.service, i.critical,
          -- The leading columns must match Issue's field declaration order so Generic decodeRow lines up.
          -- Prefer stored severity (e.g. 'low' for silent drops); fall back to critical flag.
          COALESCE(NULLIF(i.severity, ''), CASE WHEN i.critical THEN 'critical' ELSE 'info' END),
          i.affected_requests::bigint, i.affected_clients::bigint, NULL::double precision,
          i.recommended_action, i.migration_complexity, i.issue_data, i.request_payloads, i.response_payloads,
          NULL::timestamp with time zone, NULL::bigint,
          i.target_hash, i.environment, i.seq_num::bigint, i.parent_hash, i.is_framework, i.cooldown_until, i.last_notified_at, i.acknowledged_until, i.assignee_id, i.archived_until, i.archive_until_escalating,
          CASE
            WHEN i.issue_type = 'runtime_exception' THEN COALESCE(err_ev.cnt, 0)
            WHEN i.issue_type IN ('log_pattern', 'log_pattern_rate_change') THEN COALESCE(lp_ev.cnt, 0)
            ELSE i.affected_requests
          END::bigint AS event_count,
          i.updated_at, lat.event,
          CASE
            WHEN i.issue_type = 'runtime_exception' THEN COALESCE(err_ev.buckets, '{}'::bigint[])
            WHEN i.issue_type IN ('log_pattern', 'log_pattern_rate_change') THEN COALESCE(lp_ev.buckets, '{}'::bigint[])
            ELSE '{}'::bigint[]
          END,
          COALESCE((SELECT ep.users_count FROM apis.error_patterns ep WHERE ep.project_id = i.project_id AND ep.hash = i.target_hash), 0)::bigint AS users_count
        FROM apis.issues i
        LEFT JOIN LATERAL (
          SELECT SUM(day_cnt)::bigint AS cnt, array_agg(day_cnt ORDER BY day) AS buckets FROM (
            SELECT d AS day, COALESCE(SUM(ehs.event_count), 0)::bigint AS day_cnt
            FROM generate_series(#{seriesStart}::timestamptz, #{now}::timestamptz, ^{stepSql}) d
            LEFT JOIN (apis.error_hourly_stats ehs
              JOIN apis.error_patterns ep ON ep.id = ehs.error_id AND ep.project_id = ehs.project_id
                AND ep.project_id = i.project_id
                AND ((i.is_framework AND ep.parent_hash = i.target_hash)
                  OR (NOT i.is_framework AND ep.hash = i.target_hash))
            ) ON ehs.hour_bucket >= d AND ehs.hour_bucket < d + ^{stepSql}
            GROUP BY d
          ) sub
        ) err_ev ON i.issue_type = 'runtime_exception'
        LEFT JOIN LATERAL (
          SELECT SUM(day_cnt)::bigint AS cnt, array_agg(day_cnt ORDER BY day) AS buckets FROM (
            SELECT d AS day, COALESCE(SUM(lhs.event_count), 0)::bigint AS day_cnt
            FROM generate_series(#{seriesStart}::timestamptz, #{now}::timestamptz, ^{stepSql}) d
            LEFT JOIN apis.log_pattern_hourly_stats lhs
              ON lhs.pattern_hash = i.target_hash AND lhs.project_id = i.project_id
              AND lhs.hour_bucket >= d AND lhs.hour_bucket < d + ^{stepSql}
            GROUP BY d
          ) sub
        ) lp_ev ON i.issue_type IN ('log_pattern', 'log_pattern_rate_change')
        LEFT JOIN LATERAL (
          SELECT a.event FROM apis.issue_activity_log a
          WHERE a.issue_id = i.id AND a.event IN ('resolved', 'auto_resolved', 'reopened', 'regressed', 'escalated', 'ack_expired')
          ORDER BY a.created_at DESC LIMIT 1
        ) lat ON TRUE
        WHERE i.project_id = #{pid} ^{iFilters}
        ORDER BY ^{orderBy "i."}
        LIMIT #{f.limit} OFFSET #{f.offset} |]
  total <-
    fromMaybe 0
      <$> Hasql.interpOne
        [HI.sql| SELECT COUNT(*)::bigint FROM apis.issues WHERE project_id = #{pid} ^{cFilters} |]
  pure (issues, total)


-- | Update issue with new anomaly data
updateIssueWithNewAnomaly :: (DB es, Time :> es) => IssueId -> APIChangeData -> Eff es ()
updateIssueWithNewAnomaly issueId newData = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.issues SET
        issue_data = issue_data || #{Aeson newData}::jsonb,
        affected_requests = affected_requests + 1,
        updated_at = #{now}
      WHERE id = #{issueId} |]


updateIssueEnhancement :: (DB es, Time :> es) => IssueId -> Text -> Text -> Text -> Eff es ()
updateIssueEnhancement issueId iTitle action complexity = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.issues SET
        title = #{iTitle}, recommended_action = #{action},
        migration_complexity = #{complexity}, updated_at = #{now}
      WHERE id = #{issueId} |]


-- | Update issue criticality and severity
updateIssueCriticality :: DB es => IssueId -> Bool -> IssueSeverity -> Eff es ()
updateIssueCriticality issueId isCritical sev =
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.issues SET critical = #{isCritical}, severity = #{sev} WHERE id = #{issueId} |]


-- | Dedup window for repeat notifications about the *same* issue. Distinct from
-- an acknowledgement, which is the user telling us to stop entirely.
issueNotifyDedupHours :: Int
issueNotifyDedupHours = 24


-- | How long an acknowledgement silences an issue.
--
-- @AckIndefinite@ silences until the issue regresses or someone un-acks it;
-- @AckFor n@ silences for @n@ minutes, after which 'expireAcks' returns the
-- issue to the Inbox and notifications resume. There is deliberately no third
-- "acked but still notifying" state — that ambiguity is what this replaces.
data AckWindow = AckIndefinite | AckFor Int
  deriving stock (Eq, Show)


-- | Far-future sentinel standing in for "no end". Matches the value monitors use
-- for an indefinite mute, and stays inside 'UTCTime' (Postgres @infinity@ has no
-- Haskell decoding).
indefiniteUntil :: UTCTime
indefiniteUntil = UTCTime (ModifiedJulianDay 100000) 0


-- | End instant of an acknowledgement window opened at @now@.
--
-- >>> import Data.Time (UTCTime (..), fromGregorian)
-- >>> ackUntil (UTCTime (fromGregorian 2026 1 1) 0) (AckFor 90)
-- 2026-01-01 01:30:00 UTC
-- >>> ackUntil (UTCTime (fromGregorian 2026 1 1) 0) AckIndefinite == indefiniteUntil
-- True
ackUntil :: UTCTime -> AckWindow -> UTCTime
ackUntil now = \case
  AckIndefinite -> indefiniteUntil
  AckFor mins -> addUTCTime (fromIntegral mins * 60) now


-- | What an acknowledgement records: when, by whom, and until when.
data AckSet = AckSet {at :: UTCTime, by :: Maybe Projects.UserId, window :: AckWindow}


-- | Acknowledge (@Just@) or un-acknowledge (@Nothing@) a batch of issues.
-- Acknowledging stamps the actor and the end of the silence window; un-acking
-- clears all three columns so the issue is back in the Inbox and notifiable.
setAckState :: DB es => Projects.ProjectId -> [IssueId] -> Maybe AckSet -> Eff es Int64
setAckState pid iids ackM
  | null iids = pure 0
  -- Several acknowledged recurrences of one signal may coexist, while the partial
  -- unique index permits only one open recurrence. Reopening a batch in one plain
  -- UPDATE therefore 500'd. Keep the newest selected recurrence actionable and
  -- archive competing selected or already-open rows before reopening it.
  | Nothing <- ackM = do
      let keepers =
            [HI.sql|
          WITH keepers AS (
            SELECT DISTINCT ON (project_id, target_hash, issue_type)
                   id, project_id, target_hash, issue_type
            FROM apis.issues
            WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[])
            ORDER BY project_id, target_hash, issue_type, updated_at DESC, id DESC
          )|]
      void
        $ Hasql.interpExecute
          ( keepers
              <> [HI.sql|
          UPDATE apis.issues i
          SET archived_at = app_now(), updated_at = app_now()
          FROM keepers k
          WHERE i.project_id = k.project_id
            AND i.target_hash = k.target_hash
            AND i.issue_type = k.issue_type
            AND i.id <> k.id
            AND (i.id = ANY(#{iids}::uuid[])
                 OR (i.acknowledged_at IS NULL AND i.archived_at IS NULL)) |]
          )
      Hasql.interpExecute
        ( keepers
            <> [HI.sql|
          UPDATE apis.issues i
          SET acknowledged_at = NULL, acknowledged_by = NULL,
              acknowledged_until = NULL, archived_at = NULL, updated_at = app_now()
          FROM keepers k
          WHERE i.id = k.id |]
        )
  | otherwise =
      Hasql.interpExecute
        [HI.sql|
          UPDATE apis.issues
          SET acknowledged_at = #{(.at) <$> ackM},
              acknowledged_by = #{ackM >>= (.by)},
              acknowledged_until = #{(\a -> ackUntil a.at a.window) <$> ackM},
              updated_at = COALESCE(#{(.at) <$> ackM}, updated_at)
          WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]


-- | Width of the endpoint hash that @idx_issues_project_endpoint_prefix@ indexes.
-- Migration 0130: a @target_hash@ is an 8-char endpoint hash optionally followed
-- by a 16-char field\/shape suffix.
hashPrefixWidth :: Int
hashPrefixWidth = 8


-- | Acknowledge issues by id, then sweep every sibling sharing their endpoint
-- prefix. Acking an endpoint's issue has to silence the field and shape issues
-- underneath it, or the same change keeps notifying through a different row.
--
-- Project-scoped: a @target_hash@ is content-derived, so two tenants seeing the
-- same endpoint shape produce the same 8-character hash and an unscoped sweep
-- would silence the other tenant's issues. Scoping is also what lets the sweep
-- use an index — every index on the table leads with @project_id@.
--
-- @LIKE@ against a parameter array is not sargable on its own, so the sweep also
-- narrows on @LEFT(target_hash, 8)@, the expression migration 0130 indexed for
-- exactly this shape. The LIKE stays as the exact test; the prefix equality only
-- picks the pages to look at. Measured on prod with EXPLAIN:
--
-- @
--   Seq Scan 11,438ms  ->  +project_id 375ms  ->  +prefix index 2.64ms
-- @
--
-- That narrowing is only a valid superset while every target is at least the 8
-- characters the index is built on: for a shorter target @LEFT(row, 8)@ is longer
-- than the target itself and the filter would drop rows the LIKE would match.
-- Hence the guard rather than an unconditional narrowing — an 8-char hash is what
-- the schema produces, but this also sweeps legacy hashes and must keep finding them.
ackCascade :: (DB es, Time :> es) => Projects.ProjectId -> Projects.UserId -> AckWindow -> [IssueId] -> Eff es [IssueId]
ackCascade pid uid window iids
  | null iids = pure []
  | otherwise = do
      now <- Time.currentTime
      -- The selected issues need no separate UPDATE: a target hash always matches
      -- its own prefix, so the sweep below already covers every row it was given.
      targets <-
        Hasql.interp @[Text]
          [HI.sql| SELECT target_hash FROM apis.issues WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]
      let prefixes = T.take hashPrefixWidth <$> targets
          prefixNarrowing
            | all ((>= hashPrefixWidth) . T.length) targets = [HI.sql| AND LEFT(target_hash, 8) = ANY(#{prefixes}) |]
            | otherwise = mempty
      -- Returns every row it touched, not the caller's selection: a swept sibling
      -- is just as acknowledged, and its own timeline has to say so.
      Hasql.interp @[IssueId]
        $ [HI.sql| UPDATE apis.issues
                   SET acknowledged_by = #{uid}, acknowledged_at = #{now},
                       acknowledged_until = #{ackUntil now window}, updated_at = #{now}
                   WHERE project_id = #{pid} |]
        <> prefixNarrowing
        <> [HI.sql| AND target_hash LIKE ANY(#{(<> "%") <$> targets}) RETURNING id |]


-- | Clear acknowledgements whose window has closed, returning the affected ids so
-- the caller can log 'IEAckExpired'. Nothing else clears a timed ack, so this is
-- what stops one from silently rotting out of sight forever.
expireAcks :: DB es => UTCTime -> Eff es [IssueId]
expireAcks now = do
  -- Expired recurrences can share a signal key. Archive all but the newest before
  -- reopening it, or the open-issue partial index rejects the whole expiry sweep.
  let expiredKeepers =
        [HI.sql|
      WITH expired AS (
        SELECT id, project_id, target_hash, issue_type, updated_at
        FROM apis.issues
        WHERE acknowledged_at IS NOT NULL AND archived_at IS NULL
          AND acknowledged_until <= #{now}
      ), keepers AS (
        SELECT DISTINCT ON (project_id, target_hash, issue_type)
               id, project_id, target_hash, issue_type
        FROM expired
        ORDER BY project_id, target_hash, issue_type, updated_at DESC, id DESC
      )|]
  void
    $ Hasql.interpExecute
      ( expiredKeepers
          <> [HI.sql|
      UPDATE apis.issues i
      SET archived_at = #{Just now}, updated_at = #{now}
      FROM keepers k
      WHERE i.project_id = k.project_id
        AND i.target_hash = k.target_hash
        AND i.issue_type = k.issue_type
        AND i.id <> k.id
        AND (i.id IN (SELECT id FROM expired)
             OR (i.acknowledged_at IS NULL AND i.archived_at IS NULL)) |]
      )
  -- The losers are archived above, so re-deriving keepers here yields exactly the
  -- rows that must be reopened.
  HI.getOneColumn
    <<$>> Hasql.interp
      ( expiredKeepers
          <> [HI.sql|
        UPDATE apis.issues i
        SET acknowledged_at = NULL, acknowledged_by = NULL, acknowledged_until = NULL,
            archived_at = NULL, updated_at = #{now}
        FROM keepers k
        WHERE i.id = k.id
        RETURNING i.id |]
      )


-- | True if a live acknowledgement for this (project, target, type) is still
-- silencing the signal. Detectors consult this before firing a fresh issue: an
-- ack means "don't tell me about this again", not merely "hide the old row".
isSilenced :: DB es => Projects.ProjectId -> Text -> IssueType -> UTCTime -> Eff es Bool
isSilenced pid tgt ty now =
  isJust @Int64
    <$> Hasql.interpOne
      [HI.sql|
        SELECT 1::bigint FROM apis.issues
        WHERE project_id = #{pid}
          AND target_hash = #{tgt}
          AND issue_type = #{ty}::apis.issue_type
          AND acknowledged_until > #{now}
        LIMIT 1 |]


-- | Set archive state on a batch of issues. @Just now@ archives, @Nothing@ unarchives.
-- | How long an archive holds. Sentry's "archive for N hours" and "until escalating":
-- a timed archive is lifted by 'expireArchives', an escalating one by 'wakeOnEscalation'.
data ArchiveWindow = ArchiveIndefinite | ArchiveFor Int | ArchiveUntilEscalating
  deriving stock (Eq, Show)


-- | The @window@ query value: minutes, or @escalating@.
--
-- >>> :set -XOverloadedStrings
-- >>> import Servant (parseQueryParam)
-- >>> map (parseQueryParam @ArchiveWindow) ["240", "escalating", "soon", "-5"]
-- [Right (ArchiveFor 240),Right ArchiveUntilEscalating,Left "archive window: minutes or 'escalating'",Left "archive window: minutes or 'escalating'"]
instance FromHttpApiData ArchiveWindow where
  parseQueryParam = \case
    "escalating" -> Right ArchiveUntilEscalating
    t | Just n <- readMaybe (toString t), n > 0 -> Right (ArchiveFor n)
    _ -> Left "archive window: minutes or 'escalating'"


-- | Spam leaves the Inbox with the archive and is listed only under the Spam tab; clearing it restores both.
setSpamState :: DB es => Projects.ProjectId -> [IssueId] -> Maybe UTCTime -> Eff es Int64
setSpamState pid iids at =
  Hasql.interpExecute [HI.sql| UPDATE apis.issues SET spam_at = #{at}, archived_at = #{at}, updated_at = COALESCE(#{at}, updated_at) WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]


setArchiveState :: DB es => Projects.ProjectId -> [IssueId] -> Maybe (UTCTime, ArchiveWindow) -> Eff es Int64
setArchiveState pid iids archiveM
  | null iids = pure 0
  | otherwise =
      Hasql.interpExecute
        [HI.sql|
          UPDATE apis.issues
          SET archived_at = #{mTs}, updated_at = COALESCE(#{mTs}, updated_at),
              archived_until = #{untilM}, archive_until_escalating = #{escalating}
          WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]
  where
    mTs = fst <$> archiveM
    untilM = archiveM >>= \case (t, ArchiveFor mins) -> Just (addUTCTime (fromIntegral mins * 60) t); _ -> Nothing
    escalating = (snd <$> archiveM) == Just ArchiveUntilEscalating


-- | Lift archives, returning the reopened ids. A reopen must not collide with an
-- already-open recurrence of the same signal (the open-issue partial unique index),
-- so such rows stay archived.
liftArchives :: DB es => HI.Sql -> Eff es [IssueId]
liftArchives cond =
  Hasql.interp
    [HI.sql|
      UPDATE apis.issues i
      SET archived_at = NULL, archived_until = NULL, archive_until_escalating = false
      WHERE i.archived_at IS NOT NULL AND ^{cond}
        AND NOT EXISTS (SELECT 1 FROM apis.issues o
                        WHERE o.project_id = i.project_id AND o.target_hash = i.target_hash AND o.issue_type = i.issue_type
                          AND o.id <> i.id AND o.archived_at IS NULL AND o.acknowledged_at IS NULL)
      RETURNING i.id |]


-- | Timed archives whose window has closed.
expireArchives :: DB es => UTCTime -> Eff es [IssueId]
expireArchives now = liftArchives [HI.sql| i.archived_until <= #{now} |]


-- | Runtime-exception issues archived "until escalating" for an error that just escalated.
wakeOnEscalation :: DB es => Projects.ProjectId -> Text -> Eff es [IssueId]
wakeOnEscalation pid errHash = liftArchives [HI.sql| i.archive_until_escalating AND i.project_id = #{pid} AND i.target_hash = #{errHash} AND i.issue_type = 'runtime_exception' |]


-- | A saved issue-list view: a name for a query string of list parameters.
data SavedView = SavedView {id :: UUID.UUID, name :: Text, query :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


selectIssueViews :: DB es => Projects.ProjectId -> Eff es [SavedView]
selectIssueViews pid = Hasql.interp [HI.sql| SELECT id, name, query FROM apis.issue_views WHERE project_id = #{pid} ORDER BY created_at |]


saveIssueView :: DB es => Projects.ProjectId -> Projects.UserId -> Text -> Text -> Eff es Int64
saveIssueView pid uid name q = Hasql.interpExecute [HI.sql| INSERT INTO apis.issue_views (project_id, name, query, created_by) VALUES (#{pid}, #{name}, #{q}, #{uid}) |]


deleteIssueView :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es Int64
deleteIssueView pid vid = Hasql.interpExecute [HI.sql| DELETE FROM apis.issue_views WHERE project_id = #{pid} AND id = #{vid} |]


-- | Priority is the issue's severity, set by hand.
setIssuePriority :: DB es => Projects.ProjectId -> [IssueId] -> IssueSeverity -> Eff es Int64
setIssuePriority pid iids sev = Hasql.interpExecute [HI.sql| UPDATE apis.issues SET severity = #{sev} WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]


setIssueAssignee :: DB es => Projects.ProjectId -> [IssueId] -> Maybe Projects.UserId -> Eff es Int64
setIssueAssignee pid iids uid = Hasql.interpExecute [HI.sql| UPDATE apis.issues SET assignee_id = #{uid} WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]


-- | Auto-archive open discovery-type issues (log_pattern, log_pattern_rate_change,
-- api_change) whose @updated_at@ is older than @days@. @insertIssue@ bumps
-- @updated_at@ on conflict, so an actively-drifting endpoint or firing pattern
-- never ages out — only dead signal does. Discovery issues have no "resolved"
-- column by design: the absence of new drift over time *is* the resolution.
autoArchiveStaleDiscoveryIssues :: DB es => Projects.ProjectId -> UTCTime -> Int -> Eff es Int64
autoArchiveStaleDiscoveryIssues pid now days =
  Hasql.interpExecute
    [HI.sql|
      UPDATE apis.issues
      SET archived_at = #{now}
      WHERE project_id = #{pid}
        AND acknowledged_at IS NULL
        AND archived_at IS NULL
        AND issue_type IN (
          'log_pattern'::apis.issue_type,
          'log_pattern_rate_change'::apis.issue_type,
          'api_change'::apis.issue_type
        )
        AND updated_at < #{now} - (INTERVAL '1 day' * #{days}) |]


-- | Create API Change issue from anomalies
createAPIChangeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> NonEmpty ApiChanges.AnomalyVM -> Eff es Issue
createAPIChangeIssue projectId endpointHash anomalies = do
  let firstAnomaly = head anomalies
      apiChangeData =
        APIChangeData
          { endpointMethod = fromMaybe "UNKNOWN" firstAnomaly.endpointMethod
          , endpointPath = fromMaybe "/" firstAnomaly.endpointUrlPath
          , endpointHost = fromMaybe "Unknown" firstAnomaly.endpointHost
          , anomalyHashes = V.fromList $ toList $ fmap (.targetHash) anomalies
          , shapeChanges = V.empty
          , formatChanges = V.empty
          , newFields = foldMap (.shapeNewUniqueFields) anomalies
          , deletedFields = foldMap (.shapeDeletedFields) anomalies
          , modifiedFields = foldMap (.shapeUpdatedFieldFormats) anomalies
          }
      breakingChanges = V.length apiChangeData.deletedFields + V.length apiChangeData.modifiedFields
      isCritical = breakingChanges > 0
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = endpointHash
      , parentHash = Nothing
      , isFramework = False
      , service = Just $ ApiChanges.detectService Nothing firstAnomaly.endpointUrlPath
      , critical = isCritical
      , severity = if isCritical then Critical else Warning
      , title =
          if any ((== ApiChanges.ATEndpoint) . (.anomalyType)) anomalies
            then "New endpoint detected: " <> apiChangeData.endpointMethod <> " " <> apiChangeData.endpointPath <> " on " <> apiChangeData.endpointHost
            else "API structure has changed"
      , recommendedAction = defaultRecommendedAction
      , migrationComplexity = if breakingChanges > 5 then "high" else if breakingChanges > 0 then "medium" else "low"
      , payload = ApiChangeP apiChangeData
      , timestamp = Just firstAnomaly.createdAt
      }


-- | Create Query Alert issue
createQueryAlertIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> Text -> Text -> Double -> Double -> ThresholdDirection -> Eff es Issue
createQueryAlertIssue projectId queryId queryName queryExpr threshold actual thresholdType = do
  now <- Time.currentTime
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = queryId
      , parentHash = Nothing
      , isFramework = False
      , service = Just "Monitoring"
      , critical = True
      , severity = Warning
      , title = queryName
      , recommendedAction = queryAlertRecommendedAction
      , migrationComplexity = "n/a"
      , payload =
          QueryAlertP
            QueryAlertData
              { queryId
              , queryName
              , queryExpression = queryExpr
              , thresholdValue = threshold
              , actualValue = actual
              , thresholdType
              , triggeredAt = now
              }
      , timestamp = Just (utcToZonedTime utc now)
      }


-- | Conversation type for AI chats
data ConversationType = CTAnomaly | CTTrace | CTLogExplorer | CTDashboard | CTSlackThread | CTDiscordThread | CTWeb
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default) -- required by Default AIConversation; first constructor = CTAnomaly
  deriving (Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "CT" ConversationType


-- | AI Conversation metadata
data AIConversation = AIConversation
  { id :: UUIDId "ai_conversation"
  , projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation" -- The contextual ID (issue_id, trace_id, etc.)
  , conversationType :: ConversationType
  , title :: Maybe Text
  , context :: Maybe (Aeson AE.Value) -- Initial context for the AI
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, ToRow)


-- | What a stored conversation entry represents. Execution events are rendered in
-- the transcript, but are deliberately not part of the language-model history.
data ChatMessageKind = ChatUser | ChatAssistant | ChatSystem | ChatExecutionEvent
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "Chat" ChatMessageKind


data AIChatMessage = AIChatMessage
  { id :: UUIDId "ai_chat"
  , projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"
  , role :: ChatMessageKind
  , content :: Text
  , widgets :: Maybe (Aeson AE.Value) -- Array of widget configs
  , metadata :: Maybe (Aeson AE.Value) -- Additional metadata
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, ToRow)


data ConversationSummary = ConversationSummary
  { conversationId :: UUIDId "conversation"
  , title :: Text
  , conversationType :: ConversationType
  , updatedAt :: UTCTime
  , templateKey :: Maybe Text
  , routineActive :: Bool
  , routineInterval :: Maybe RoutineInterval
  , routineNextRunAt :: Maybe UTCTime
  , routineRunningSince :: Maybe UTCTime
  , routineDestination :: Maybe RoutineDestination
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow)


newtype RoutineInterval = RoutineInterval Int
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromField, HI.DecodeValue, HI.EncodeValue, ToField)


routineIntervalMinutes :: RoutineInterval -> Int
routineIntervalMinutes (RoutineInterval minutes) = minutes


routineCadence :: RoutineInterval -> Text
routineCadence interval
  | minutes `mod` 10080 == 0 = frequency (minutes `div` 10080) "week"
  | minutes `mod` 1440 == 0 = frequency (minutes `div` 1440) "day"
  | minutes `mod` 60 == 0 = frequency (minutes `div` 60) "hour"
  | otherwise = frequency minutes "minute"
  where
    minutes = routineIntervalMinutes interval
    frequency 1 unit = "Every " <> unit
    frequency count unit = "Every " <> show count <> " " <> unit <> "s"


mkRoutineInterval :: Int -> Either Text RoutineInterval
mkRoutineInterval minutes
  | minutes < 5 = Left "Routine interval must be at least 5 minutes."
  | minutes > 10080 = Left "Routine interval must not exceed 10,080 minutes."
  | otherwise = Right $ RoutineInterval minutes


instance FromHttpApiData RoutineInterval where
  parseUrlPiece value =
    maybe (Left "Routine interval must be a whole number of minutes.") mkRoutineInterval $ readMaybe $ toString value


data RoutineSchedule
  = Every RoutineInterval
  | Daily TimeOfDay
  | Weekdays TimeOfDay
  | Weekly DayOfWeek TimeOfDay
  deriving stock (Eq, Generic, Show)


data RoutineScheduleKind = ScheduleInterval | ScheduleDaily | ScheduleWeekdays | ScheduleWeekly
  deriving stock (Eq, Generic, Read, Show)
  deriving (FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "Schedule" RoutineScheduleKind


data RoutineReport = ReportAlways | ReportFindings
  deriving stock (Eq, Generic, Read, Show)
  deriving (FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "Report" RoutineReport


data RoutineDestination = DestinationConversation | DestinationSlack
  deriving stock (Eq, Generic, Read, Show)
  deriving (FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "Destination" RoutineDestination


instance FromHttpApiData RoutineDestination where
  parseUrlPiece = \case
    "conversation" -> Right DestinationConversation
    "slack" -> Right DestinationSlack
    _ -> Left "Routine destination must be conversation or slack."


data RoutineRunStatus = RunRunning | RunSucceeded | RunNoFindings | RunFailed | RunTimedOut | RunCancelled
  deriving stock (Eq, Generic, Read, Show)
  deriving (FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "Run" RoutineRunStatus


data RoutineRunCompletion = CompletedSucceeded | CompletedNoFindings | CompletedFailed | CompletedTimedOut | CompletedCancelled
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "Completed" RoutineRunCompletion


data RoutineCategory = CategoryReliability | CategoryTelemetryQuality | CategoryDelivery | CategoryIncidents | CategoryCost | CategorySecurity
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)


routineCategoryLabel :: RoutineCategory -> Text
routineCategoryLabel = \case
  CategoryReliability -> "Reliability"
  CategoryTelemetryQuality -> "Telemetry quality"
  CategoryDelivery -> "Delivery"
  CategoryIncidents -> "Incidents"
  CategoryCost -> "Cost"
  CategorySecurity -> "Security"


data RoutineTemplate = RoutineTemplate
  { key :: Text
  , version :: Int
  , category :: RoutineCategory
  , title :: Text
  , description :: Text
  , prompt :: Text
  , schedule :: RoutineSchedule
  , reportWhen :: RoutineReport
  , requirements :: [Text]
  }
  deriving stock (Eq, Generic, Show)


routineTemplates :: [RoutineTemplate]
routineTemplates =
  [ template "daily-operations-briefing" CategoryReliability "Daily operations briefing" "Start the day with overnight incidents, regressions, slow endpoints, traffic anomalies, and unresolved issues." "Review this project's telemetry, issues, incidents, and endpoints since the previous business day. Rank only observed operational changes by impact, cite the supporting queries or records, and state the next useful check." (Weekdays $ TimeOfDay 9 0 0) ReportAlways ["Logs or traces", "Issues"]
  , template "weekly-reliability-review" CategoryReliability "Weekly reliability review" "Prioritize recurring failures, latency regressions, and reliability work from the last seven days." "Compare this project's last seven days with the preceding seven days. Report reliability changes, recurring failures, latency regressions, and three evidence-backed priorities." (Weekly Monday $ TimeOfDay 9 0 0) ReportAlways ["Logs or traces"]
  , template "error-regression-radar" CategoryReliability "Error regression radar" "Find error groups that returned or materially increased after a quiet period." "Find error patterns that returned after a quiet period or increased materially against their recent baseline. Cite counts, services, and example evidence. If none qualify, say so briefly." (Every $ RoutineInterval 60) ReportFindings ["Logs"]
  , template "slow-endpoint-watch" CategoryReliability "Slow endpoint watch" "Find endpoints whose tail latency became materially worse than their recent baseline." "Compare endpoint p95 and p99 latency in the latest hour with a representative recent baseline. Report only material degradations with enough traffic to be credible. If none qualify, say so briefly." (Every $ RoutineInterval 60) ReportFindings ["Traces"]
  , template "telemetry-gap-detector" CategoryTelemetryQuality "Telemetry gap detector" "Find active services that unexpectedly stopped sending logs, traces, or metrics." "Find services that were active recently but unexpectedly stopped sending one or more telemetry signals. Distinguish missing evidence from healthy behavior. If no credible gaps exist, say so briefly." (Every $ RoutineInterval 60) ReportFindings ["Logs, traces, or metrics"]
  , template "telemetry-quality-check" CategoryTelemetryQuality "Telemetry quality check" "Find missing or inconsistent service, environment, version, and trace attributes." "Audit recent telemetry for missing or inconsistent service names, environments, versions, trace correlation, and unstable high-cardinality attributes. Rank actionable instrumentation fixes. If no actionable defect exists, say so briefly." (Daily $ TimeOfDay 10 0 0) ReportFindings ["Logs or traces"]
  , template "traffic-drop-watch" CategoryReliability "Traffic drop watch" "Detect services and endpoints whose request volume unexpectedly falls or disappears." "Compare traffic during the latest 30 minutes with matching recent periods. Report only credible service or endpoint drops, accounting for ordinary low-volume variation and time-of-day patterns." (Every $ RoutineInterval 30) ReportFindings ["Logs or traces"]
  , template "dependency-degradation-digest" CategoryReliability "Dependency degradation digest" "Find downstream dependencies associated with new latency or error regressions." "Review outgoing spans and related errors from the latest hour. Report downstream dependencies with a material latency or error regression against their recent baseline, and name the affected callers." (Every $ RoutineInterval 60) ReportFindings ["Traces"]
  , template "new-service-endpoint-digest" CategoryDelivery "New service and endpoint digest" "Summarize newly observed services, routes, operations, and environments." "Find services and endpoints first observed since the previous run. Separate confirmed endpoint records from names inferred only from telemetry, and report the owning service and environment when known." (Daily $ TimeOfDay 10 0 0) ReportFindings ["Traces", "Endpoints"]
  , template "on-call-handoff" CategoryIncidents "On-call handoff" "Prepare a concise handoff of active issues, recoveries, regressions, and watch items." "Prepare an on-call handoff from this project's active issues, recent incident episodes, current telemetry regressions, and unresolved watch items. Cite records and evidence, and separate observed facts from recommended follow-up." (Weekdays $ TimeOfDay 17 0 0) ReportAlways ["Issues", "Incidents", "Logs or traces"]
  , template "incident-follow-up-ledger" CategoryIncidents "Incident follow-up ledger" "Track recent incident follow-ups and verify whether production evidence improved." "Review recent incident episodes and their related issues. Report unresolved follow-up work, repeated symptoms, and cases where production telemetry has not demonstrably returned to its prior baseline." (Daily $ TimeOfDay 11 0 0) ReportFindings ["Incidents", "Issues"]
  , template "alert-quality-review" CategoryIncidents "Alert quality review" "Find noisy, stale, duplicate, or unactionable monitors and missed symptoms." "Compare recent monitors, issues, and incident episodes. Identify noisy or duplicate alerts, monitors aimed at stale services or fields, and important incident symptoms that no monitor described. Support each recommendation with observed records." (Weekly Monday $ TimeOfDay 11 0 0) ReportFindings ["Monitors", "Issues", "Incidents"]
  , template "deployment-regression-review" CategoryDelivery "Deployment regression review" "Find reliability changes associated with newly observed service versions." "Compare recent service-version changes with errors, latency, and incident timing. Report only evidence-backed regressions, state when deployment evidence is unavailable, and never treat temporal proximity alone as causation." (Daily $ TimeOfDay 14 0 0) ReportFindings ["Logs or traces", "Service versions"]
  , template "noisy-log-sources" CategoryCost "Noisy log sources" "Find services and message patterns producing disproportionate log volume." "Rank services and recurring message shapes by recent log volume and estimated avoidable noise. Exclude high-volume signals that carry clear operational value, and recommend concrete sampling or log-level changes." (Daily $ TimeOfDay 12 0 0) ReportFindings ["Logs"]
  , template "trace-correlation-audit" CategoryTelemetryQuality "Trace correlation audit" "Find logs and spans that cannot be joined because trace context is missing or inconsistent." "Audit recent logs and spans for missing trace identifiers, broken parent relationships, and inconsistent service identity. Rank fixes by the amount of investigation context they would restore." (Weekly Monday $ TimeOfDay 10 0 0) ReportFindings ["Logs", "Traces"]
  , template "telemetry-cost-watch" CategoryCost "Telemetry cost watch" "Detect sudden volume and cardinality growth before it becomes an ingestion surprise." "Compare telemetry volume, attribute cardinality, and the noisiest sources with recent baselines. Report material cost-risk changes and identify the services, fields, or patterns responsible." (Daily $ TimeOfDay 13 0 0) ReportFindings ["Logs, traces, or metrics"]
  , template "authentication-failure-digest" CategorySecurity "Authentication failure digest" "Summarize unusual authentication and authorization failures without replacing security alerting." "Review authentication failures, authorization denials, and administrative actions in recent telemetry. Report unusual changes with service, environment, source, and supporting counts. Do not claim compromise from failures alone." (Every $ RoutineInterval 60) ReportFindings ["Security logs"]
  , template "weekly-slo-review" CategoryReliability "Weekly SLO review" "Review user-visible reliability signals and the work most likely to improve them." "Compare the last seven days of user-visible errors and latency with the preceding period. Summarize burn-like trends from available telemetry, clearly state when a formal SLO is unavailable, and recommend evidence-backed reliability priorities." (Weekly Monday $ TimeOfDay 9 30 0) ReportAlways ["Logs, traces, or metrics"]
  ]
  where
    template key category title description prompt schedule reportWhen requirements = RoutineTemplate{key, version = 1, category, title, description, prompt, schedule, reportWhen, requirements}


routineScheduleLabel :: RoutineSchedule -> Text
routineScheduleLabel = \case
  Every interval -> routineCadence interval
  Daily time -> "Daily at " <> clock time
  Weekdays time -> "Weekdays at " <> clock time
  Weekly weekday time -> "Every " <> show weekday <> " at " <> clock time
  where
    clock time = T.justifyRight 2 '0' (show time.todHour) <> ":" <> T.justifyRight 2 '0' (show time.todMin) <> " project time"


data ScheduledRoutine = ScheduledRoutine
  { id :: UUIDId "ai_routine"
  , scheduledAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data ClaimedRoutine = ClaimedRoutine
  { runId :: UUIDId "ai_routine_run"
  , projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"
  , claimedAt :: UTCTime
  , allowActions :: Bool
  , reportWhen :: RoutineReport
  , destination :: RoutineDestination
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RoutineRun = RoutineRun
  { id :: UUIDId "ai_routine_run"
  , conversationId :: UUIDId "conversation"
  , title :: Maybe Text
  , scheduledAt :: UTCTime
  , startedAt :: UTCTime
  , finishedAt :: Maybe UTCTime
  , status :: RoutineRunStatus
  , error :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | Get or create a conversation (race-condition safe via ON CONFLICT + RETURNING)
getOrCreateConversation :: (DB es, Error ServerError :> es, Time :> es) => Projects.ProjectId -> UUIDId "conversation" -> ConversationType -> AE.Value -> Eff es AIConversation
getOrCreateConversation pid convId convType ctx = do
  now <- Time.currentTime
  Hasql.interpOne
    [HI.sql| INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, context)
              VALUES (#{pid}, #{convId}, #{convType}, #{Aeson ctx}) ON CONFLICT (project_id, conversation_id) DO UPDATE SET updated_at = #{now}
              RETURNING id, project_id, conversation_id, conversation_type, title, context, created_at, updated_at |]
    >>= (`whenNothing` throwError err500{errBody = "getOrCreateConversation: RETURNING clause must return a row"})


listConversations :: DB es => Projects.ProjectId -> Eff es [ConversationSummary]
listConversations pid =
  Hasql.interp
    [HI.sql|SELECT c.conversation_id,
      COALESCE(NULLIF(c.title, ''), NULLIF(i.title, ''), NULLIF(first_message.content, ''), 'AI conversation'),
      c.conversation_type, c.updated_at, r.template_key,
      COALESCE(r.active, FALSE), r.interval_minutes, r.next_run_at, r.running_since, r.destination
      FROM apis.ai_conversations c
      LEFT JOIN apis.issues i ON i.project_id = c.project_id AND i.id = c.conversation_id
      LEFT JOIN apis.ai_routines r ON r.project_id = c.project_id AND r.conversation_id = c.conversation_id
      LEFT JOIN LATERAL (
        SELECT LEFT(m.content, 100) AS content FROM apis.ai_chat_messages m
        WHERE m.project_id = c.project_id AND m.conversation_id = c.conversation_id AND m.role = 'user'
        ORDER BY m.created_at LIMIT 1
      ) first_message ON TRUE
      WHERE c.project_id = #{pid}
      ORDER BY (r.id IS NOT NULL) DESC, c.updated_at DESC|]


getConversation :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es (Maybe ConversationSummary)
getConversation pid convId =
  Hasql.interpOne
    [HI.sql|SELECT c.conversation_id,
      COALESCE(NULLIF(c.title, ''), NULLIF(i.title, ''), NULLIF(first_message.content, ''), 'New chat'),
      c.conversation_type, c.updated_at, r.template_key,
      COALESCE(r.active, FALSE), r.interval_minutes, r.next_run_at, r.running_since, r.destination
      FROM apis.ai_conversations c
      LEFT JOIN apis.issues i ON i.project_id = c.project_id AND i.id = c.conversation_id
      LEFT JOIN apis.ai_routines r ON r.project_id = c.project_id AND r.conversation_id = c.conversation_id
      LEFT JOIN LATERAL (
        SELECT LEFT(m.content, 100) AS content FROM apis.ai_chat_messages m
        WHERE m.project_id = c.project_id AND m.conversation_id = c.conversation_id AND m.role = 'user'
        ORDER BY m.created_at LIMIT 1
      ) first_message ON TRUE
      WHERE c.project_id = #{pid} AND c.conversation_id = #{convId}|]


-- | Recover the typed anomaly subject at the persistence boundary. Other
-- conversation kinds cannot be accidentally passed to issue handlers.
conversationIssueId :: ConversationSummary -> Maybe IssueId
conversationIssueId conversation = case conversation.conversationType of
  CTAnomaly -> Just $ UUIDId conversation.conversationId.unUUIDId
  _ -> Nothing


conversationNeedsTitle :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es Bool
conversationNeedsTitle pid convId =
  fromMaybe False <$> Hasql.interpOne [HI.sql|SELECT EXISTS (SELECT 1 FROM apis.ai_conversations WHERE project_id = #{pid} AND conversation_id = #{convId} AND title IS NULL)::boolean|]


setConversationTitle :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Text -> Eff es ()
setConversationTitle pid convId title =
  void
    $ Hasql.interpExecute
      [HI.sql|UPDATE apis.ai_conversations SET title = #{T.take 100 $ T.strip title}
      WHERE project_id = #{pid} AND conversation_id = #{convId} AND title IS NULL|]


renameConversation :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Text -> Eff es ()
renameConversation pid convId title =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.ai_conversations SET title = #{T.take 100 $ T.strip title}
      WHERE project_id = #{pid} AND conversation_id = #{convId}|]


deleteConversation :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es ()
deleteConversation pid convId =
  Hasql.interpExecute_
    [HI.sql|WITH deleted_messages AS (
        DELETE FROM apis.ai_chat_messages
        WHERE project_id = #{pid} AND conversation_id = #{convId}
      )
      DELETE FROM apis.ai_conversations
      WHERE project_id = #{pid} AND conversation_id = #{convId}|]


upsertRoutine :: (DB es, Time :> es) => Projects.ProjectId -> UUIDId "conversation" -> RoutineInterval -> Eff es (Maybe ScheduledRoutine)
upsertRoutine pid convId interval = do
  now <- Time.currentTime
  let intervalMinutes = routineIntervalMinutes interval
  Hasql.interpOne
    [HI.sql|WITH saved AS (
        INSERT INTO apis.ai_routines AS routine (project_id, conversation_id, interval_minutes, next_run_at, allow_actions)
        SELECT #{pid}, #{convId}, #{intervalMinutes}, #{addUTCTime (fromIntegral $ intervalMinutes * 60) now}, TRUE
        FROM apis.ai_conversations WHERE project_id = #{pid} AND conversation_id = #{convId}
        ON CONFLICT (project_id, conversation_id) DO UPDATE SET
          interval_minutes = EXCLUDED.interval_minutes, active = TRUE,
          next_run_at = CASE WHEN routine.running_since IS NULL THEN EXCLUDED.next_run_at ELSE routine.next_run_at END,
          schedule_kind = #{ScheduleInterval}, schedule_hour = NULL, schedule_minute = NULL, schedule_weekday = NULL,
          allow_actions = TRUE, cancelled_at = NULL
        RETURNING id, next_run_at, running_since
      ) SELECT id, next_run_at FROM saved WHERE running_since IS NULL|]


installRoutineTemplate :: (DB es, Time :> es) => Projects.ProjectId -> UUIDId "conversation" -> Text -> RoutineTemplate -> Eff es (Maybe ScheduledRoutine)
installRoutineTemplate pid convId timezone template = do
  now <- Time.currentTime
  let (kind, intervalMinutes, hour, minute, weekday) = scheduleColumns template.schedule
      zone = if isJust (TZ.tzByName $ encodeUtf8 timezone) then timezone else "UTC"
      nextRunAt = nextRoutineAt template.schedule zone now now
  Hasql.interpOne
    [HI.sql|INSERT INTO apis.ai_routines AS routine
      (project_id, conversation_id, interval_minutes, timezone, next_run_at, template_key, template_version,
       schedule_kind, schedule_hour, schedule_minute, schedule_weekday, report_when, destination, allow_actions)
      SELECT #{pid}, #{convId}, #{intervalMinutes}, #{zone}, #{nextRunAt}, #{template.key}, #{template.version},
             #{kind}, #{hour}, #{minute}, #{weekday}, #{template.reportWhen}, #{DestinationConversation}, FALSE
      FROM apis.ai_conversations WHERE project_id = #{pid} AND conversation_id = #{convId}
      ON CONFLICT (project_id, conversation_id) DO UPDATE SET
        interval_minutes = EXCLUDED.interval_minutes, timezone = EXCLUDED.timezone, active = TRUE,
        next_run_at = EXCLUDED.next_run_at, template_key = EXCLUDED.template_key,
        template_version = EXCLUDED.template_version, schedule_kind = EXCLUDED.schedule_kind,
        schedule_hour = EXCLUDED.schedule_hour, schedule_minute = EXCLUDED.schedule_minute,
        schedule_weekday = EXCLUDED.schedule_weekday, report_when = EXCLUDED.report_when,
        destination = EXCLUDED.destination, allow_actions = FALSE, cancelled_at = NULL
      RETURNING id, next_run_at|]
  where
    scheduleColumns :: RoutineSchedule -> (RoutineScheduleKind, Int, Maybe Int, Maybe Int, Maybe Int)
    scheduleColumns = \case
      Every interval -> (ScheduleInterval, routineIntervalMinutes interval, Nothing, Nothing, Nothing)
      Daily time -> (ScheduleDaily, 1440, Just time.todHour, Just time.todMin, Nothing)
      Weekdays time -> (ScheduleWeekdays, 1440, Just time.todHour, Just time.todMin, Nothing)
      Weekly weekday time -> (ScheduleWeekly, 10080, Just time.todHour, Just time.todMin, Just $ isoWeekday weekday)

    isoWeekday :: DayOfWeek -> Int
    isoWeekday = \case Monday -> 1; Tuesday -> 2; Wednesday -> 3; Thursday -> 4; Friday -> 5; Saturday -> 6; Sunday -> 7


nextRoutineAt :: RoutineSchedule -> Text -> UTCTime -> UTCTime -> UTCTime
nextRoutineAt schedule timezone scheduledAt now = case schedule of
  Every interval -> addUTCTime (fromIntegral missed * seconds) firstCandidate
    where
      seconds = fromIntegral $ routineIntervalMinutes interval * 60
      firstCandidate = addUTCTime seconds scheduledAt
      missed = max 0 $ floor (diffUTCTime now firstCandidate / seconds) + 1
  _ -> fromMaybe (addUTCTime 604800 now) $ find matches [addUTCTime (fromIntegral n * 60) nextMinute | n <- [0 .. 10079]]
  where
    nextMinute = posixSecondsToUTCTime $ fromIntegral (((floor (utcTimeToPOSIXSeconds now) :: Integer) `div` 60 + 1) * 60)
    tz = fromMaybe utcTZ $ TZ.tzByName $ encodeUtf8 timezone
    matches instant =
      let LocalTime day (TimeOfDay hour minute _) = utcToLocalTimeTZ tz instant
       in hour == scheduledTime.todHour && minute == scheduledTime.todMin && case schedule of
            Daily{} -> True
            Weekdays{} -> dayOfWeek day `elem` ([Monday, Tuesday, Wednesday, Thursday, Friday] :: [DayOfWeek])
            Weekly weekday _ -> dayOfWeek day == weekday
            Every{} -> False
    scheduledTime = case schedule of Every{} -> midnight; Daily time -> time; Weekdays time -> time; Weekly _ time -> time
    midnight = TimeOfDay 0 0 0


-- | Lease a scheduled instant before any integration action runs. A stale lease can be
-- recovered after thirty minutes; ordinary duplicate jobs cannot overlap it.
claimRoutine :: DB es => UUIDId "ai_routine" -> UTCTime -> Eff es (Maybe ClaimedRoutine)
claimRoutine routineId scheduledAt =
  Hasql.interpOne
    [HI.sql|WITH claimed AS (
        UPDATE apis.ai_routines SET running_since = now()
        WHERE id = #{routineId} AND active AND next_run_at = #{scheduledAt}
          AND (running_since IS NULL OR running_since < now() - interval '30 minutes')
        RETURNING project_id, conversation_id, running_since, allow_actions, report_when, destination
      ), run AS (
        INSERT INTO apis.ai_routine_runs (routine_id, project_id, conversation_id, scheduled_at, started_at, status)
        SELECT #{routineId}, project_id, conversation_id, #{scheduledAt}, running_since, 'running' FROM claimed
        ON CONFLICT (routine_id, scheduled_at) DO UPDATE SET
          started_at = EXCLUDED.started_at, finished_at = NULL, status = 'running', findings = NULL, actions = NULL, error = NULL
          WHERE apis.ai_routine_runs.status = 'running'
        RETURNING id
      )
      SELECT run.id, claimed.project_id, claimed.conversation_id, claimed.running_since, claimed.allow_actions, claimed.report_when, claimed.destination
      FROM claimed CROSS JOIN run|]


finishRoutineRun :: DB es => UUIDId "ai_routine_run" -> UTCTime -> RoutineRunCompletion -> Maybe AE.Value -> Maybe AE.Value -> Maybe Text -> Eff es ()
finishRoutineRun runId startedAt status findings actions err =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.ai_routine_runs
      SET finished_at = now(), status = #{status}, findings = #{Aeson <$> findings}, actions = #{Aeson <$> actions}, error = #{err}
      WHERE id = #{runId} AND started_at = #{startedAt} AND status = 'running'|]


listRoutineRuns :: DB es => Projects.ProjectId -> Int -> Eff es [RoutineRun]
listRoutineRuns pid limit =
  Hasql.interp
    [HI.sql|SELECT run.id, run.conversation_id, conversation.title, run.scheduled_at,
      run.started_at, run.finished_at, run.status, run.error
      FROM apis.ai_routine_runs run
      LEFT JOIN apis.ai_conversations conversation
        ON conversation.project_id = run.project_id AND conversation.conversation_id = run.conversation_id
      WHERE run.project_id = #{pid}
      ORDER BY run.scheduled_at DESC LIMIT #{max 1 $ min 100 limit}|]


-- | Finish only the lease this worker owns, then schedule from its intended time so a
-- slow run never overlaps its successor or triggers a catch-up burst. Pausing keeps
-- the lease until this cleanup, but suppresses the successor.
completeRoutine :: (DB es, Time :> es) => UUIDId "ai_routine" -> UTCTime -> UTCTime -> Eff es (Maybe UTCTime)
completeRoutine routineId scheduledAt claimedAt = do
  now <- Time.currentTime
  timing <- routineTiming [HI.sql|id = #{routineId}|]
  let nextRunAt = timing <&> \(schedule, timezone) -> nextRoutineAt schedule timezone scheduledAt now
  join
    <$> Hasql.interpOne
      [HI.sql|UPDATE apis.ai_routines SET last_run_at = #{now}, running_since = NULL, cancelled_at = NULL,
        next_run_at = CASE WHEN active THEN #{nextRunAt} END
        WHERE id = #{routineId} AND next_run_at = #{scheduledAt} AND running_since = #{claimedAt}
        RETURNING CASE WHEN active THEN next_run_at END|]


-- | Recovery source for a queue insertion lost to a crash. Duplicate enqueues are safe:
-- 'claimRoutine' admits exactly one worker for each scheduled instant.
listDueRoutines :: DB es => Int -> Eff es [ScheduledRoutine]
listDueRoutines limit =
  Hasql.interp
    [HI.sql|SELECT id, next_run_at
      FROM apis.ai_routines WHERE active AND next_run_at <= now()
        AND (running_since IS NULL OR running_since < now() - interval '30 minutes')
      ORDER BY next_run_at LIMIT #{max 1 limit}|]


pauseRoutine :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es ()
pauseRoutine pid convId =
  void
    $ Hasql.interpExecute
      [HI.sql|UPDATE apis.ai_routines SET active = FALSE,
        next_run_at = CASE WHEN running_since IS NULL THEN NULL ELSE next_run_at END
        WHERE project_id = #{pid} AND conversation_id = #{convId}|]


cancelRoutineRun :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es ()
cancelRoutineRun pid convId =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.ai_routines SET cancelled_at = clock_timestamp()
      WHERE project_id = #{pid} AND conversation_id = #{convId} AND running_since IS NOT NULL|]


resumeRoutine :: (DB es, Time :> es) => Projects.ProjectId -> UUIDId "conversation" -> Eff es (Maybe ScheduledRoutine)
resumeRoutine pid convId = do
  now <- Time.currentTime
  routineTiming [HI.sql|project_id = #{pid} AND conversation_id = #{convId}|] >>= \case
    Nothing -> pure Nothing
    Just (schedule, timezone) -> do
      let nextRunAt = nextRoutineAt schedule timezone now now
      Hasql.interpOne
        [HI.sql|WITH resumed AS (
            UPDATE apis.ai_routines SET active = TRUE, cancelled_at = NULL,
              next_run_at = CASE WHEN running_since IS NULL THEN #{nextRunAt} ELSE next_run_at END
            WHERE project_id = #{pid} AND conversation_id = #{convId}
            RETURNING id, next_run_at, running_since
          ) SELECT id, next_run_at FROM resumed WHERE running_since IS NULL|]


deleteRoutine :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es ()
deleteRoutine pid convId =
  Hasql.interpExecute_
    [HI.sql|DELETE FROM apis.ai_routines WHERE project_id = #{pid} AND conversation_id = #{convId}|]


setRoutineDestination :: DB es => Projects.ProjectId -> UUIDId "conversation" -> RoutineDestination -> Eff es ()
setRoutineDestination pid convId destination =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.ai_routines
      SET destination = #{destination}, allow_actions = #{destination == DestinationSlack}
      WHERE project_id = #{pid} AND conversation_id = #{convId}|]


routineCanAct :: DB es => UUIDId "ai_routine" -> Eff es Bool
routineCanAct routineId =
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|SELECT active AND allow_actions AND cancelled_at IS NULL
        FROM apis.ai_routines WHERE id = #{routineId}|]


routineRunCancelled :: DB es => UUIDId "ai_routine" -> UTCTime -> Eff es Bool
routineRunCancelled routineId claimedAt =
  fromMaybe False
    <$> Hasql.interpOne
      [HI.sql|SELECT COALESCE(cancelled_at >= #{claimedAt}, FALSE)
        FROM apis.ai_routines WHERE id = #{routineId}|]


routineTiming :: DB es => HI.Sql -> Eff es (Maybe (RoutineSchedule, Text))
routineTiming predicate =
  (decode =<<)
    <$> Hasql.interpOne
      ([HI.sql|SELECT schedule_kind, interval_minutes, timezone, schedule_hour::bigint, schedule_minute::bigint, schedule_weekday::bigint FROM apis.ai_routines WHERE |] <> predicate)
  where
    decode (kind :: RoutineScheduleKind, interval :: RoutineInterval, timezone :: Text, hour :: Maybe Int, minute :: Maybe Int, weekday :: Maybe Int) = do
      schedule <- case kind of
        ScheduleInterval -> Just $ Every interval
        ScheduleDaily -> Daily <$> timeOfDay hour minute
        ScheduleWeekdays -> Weekdays <$> timeOfDay hour minute
        ScheduleWeekly -> Weekly <$> (weekday >>= dayFromISO) <*> timeOfDay hour minute
      pure (schedule, timezone)
    timeOfDay hour minute = TimeOfDay <$> hour <*> minute <*> pure 0
    dayFromISO = \case 1 -> Just Monday; 2 -> Just Tuesday; 3 -> Just Wednesday; 4 -> Just Thursday; 5 -> Just Friday; 6 -> Just Saturday; 7 -> Just Sunday; _ -> Nothing


-- | Insert a new chat message
insertChatMessage :: DB es => Projects.ProjectId -> UUIDId "conversation" -> ChatMessageKind -> Text -> Maybe AE.Value -> Maybe AE.Value -> Eff es ()
insertChatMessage pid convId chatRole chatContent widgetsM metadataM =
  Hasql.interpExecute_ $ insertChatMessageSql pid convId chatRole chatContent widgetsM metadataM


insertChatMessageSql :: Projects.ProjectId -> UUIDId "conversation" -> ChatMessageKind -> Text -> Maybe AE.Value -> Maybe AE.Value -> HI.Sql
insertChatMessageSql pid convId chatRole chatContent widgetsM metadataM =
  [HI.sql| WITH inserted AS (
              INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, widgets, metadata, created_at)
              VALUES (#{pid}, #{convId}, #{chatRole}, #{chatContent}, #{Aeson <$> widgetsM}, #{Aeson <$> metadataM}, clock_timestamp())
              RETURNING 1
            )
            UPDATE apis.ai_conversations SET updated_at = clock_timestamp()
            WHERE project_id = #{pid} AND conversation_id = #{convId} AND EXISTS (SELECT 1 FROM inserted) |]


-- | Persist a Slack question once, and exclude that turn from the history passed
-- to the model: the caller appends its current user message exactly once.
prepareSlackTurn :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Text -> Text -> Eff es [AIChatMessage]
prepareSlackTurn pid convId messageTs question =
  reverse
    <$> Hasql.interp
      [HI.sql|WITH inserted AS (
    INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, slack_message_ts, created_at)
    VALUES (#{pid}, #{convId}, 'user', #{question}, #{messageTs}, clock_timestamp())
    ON CONFLICT (project_id, conversation_id, slack_message_ts, role) WHERE slack_message_ts IS NOT NULL DO NOTHING
  ) SELECT id, project_id, conversation_id, role, content, widgets, metadata, created_at
    FROM apis.ai_chat_messages WHERE project_id = #{pid} AND conversation_id = #{convId}
      AND slack_message_ts IS DISTINCT FROM #{messageTs}
    ORDER BY created_at DESC, id DESC LIMIT 200|]


-- | Select the latest 200 messages, returned oldest first for the model.
selectChatHistory :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es [AIChatMessage]
selectChatHistory pid convId =
  reverse
    <$> Hasql.interp
      [HI.sql| SELECT id, project_id, conversation_id, role, content, widgets, metadata, created_at
            FROM apis.ai_chat_messages
            WHERE project_id = #{pid} AND conversation_id = #{convId}
            ORDER BY created_at DESC, id DESC
            LIMIT 200 |]


-- | Generate deterministic UUID v5 from text (uses OID namespace)
textToConversationId :: Text -> UUIDId "conversation"
textToConversationId = UUIDId . UUID5.generateNamed UUID5.namespaceOID . BS.unpack . encodeUtf8


slackThreadToConversationId :: Text -> Text -> UUIDId "conversation"
slackThreadToConversationId cid ts = textToConversationId (cid <> ":" <> ts)


slackScopedConversationId :: Projects.ProjectId -> Text -> Text -> Text -> UUIDId "conversation"
slackScopedConversationId pid teamId channelId threadTs =
  UUIDId $ UUID5.generateNamed pid.unUUIDId $ BS.unpack $ toStrict $ AE.encode (teamId, channelId, threadTs)


-- | Serialize first-contact history insertion on one connection. Failed fetches
-- never enter this transaction; concurrent successful fetches seed at most once.
seedChatHistory :: DB es => Projects.ProjectId -> UUIDId "conversation" -> [(ChatMessageKind, Text)] -> Eff es ()
seedChatHistory pid convId messages = Hasql.transaction TxS.ReadCommitted TxS.Write do
  locked <-
    Hasql.queryTx @[UUIDId "conversation"]
      [HI.sql|SELECT conversation_id FROM apis.ai_conversations
      WHERE project_id = #{pid} AND conversation_id = #{convId} FOR UPDATE|]
  existing <-
    Hasql.queryTx @[Bool]
      [HI.sql|SELECT EXISTS (SELECT 1 FROM apis.ai_chat_messages
      WHERE project_id = #{pid} AND conversation_id = #{convId})|]
  when (not (null locked) && not (or existing)) $ for_ messages \(role, content) ->
    Hasql.executeTx $ insertChatMessageSql pid convId role content Nothing Nothing


-- | Create an issue for a log pattern rate change
createLogPatternRateChangeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> LogPatterns.LogPatternWithRate -> SpikeResult -> Eff es Issue
createLogPatternRateChangeIssue projectId lp sr = do
  now <- Time.currentTime
  let changePercentVal = if sr.mean > 1 then min 9999 $ abs ((sr.currentRate / sr.mean) - 1) * 100 else 0
      dir = display sr.direction
      lvl = T.toLower $ fromMaybe "" lp.logLevel
      -- Silent drops on unknown/empty services are almost always deploy/pod-restart
      -- noise, not incidents. Demote them so the Inbox filter hides them by default.
      svcLabel = fromMaybe "unknown" lp.serviceName
      silentDrop = sr.direction == Drop && sr.currentRate == 0 && svcLabel `elem` ["", "unknown"]
      severity
        | silentDrop = Low
        | otherwise = case (sr.direction, lvl) of
            (Spike, "error") -> Critical
            (Spike, _) -> Warning
            (Drop, _) -> Info
      title =
        T.intercalate
          " · "
          [ svcLabel
          , T.take 40 lp.logPattern
          , dir <> " " <> showPct changePercentVal <> " (" <> showRate sr.currentRate <> " vs " <> showRate sr.mean <> ")"
          ]
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = lp.patternHash
      , parentHash = Nothing
      , isFramework = False
      , service = lp.serviceName
      , critical = not silentDrop && sr.direction == Spike && lvl == "error"
      , severity
      , title
      , recommendedAction = "Log pattern volume " <> dir <> " detected. Current: " <> showRate sr.currentRate <> ", Baseline: " <> showRate sr.mean <> " (" <> showRounded "" (abs sr.zScore) <> " std devs)."
      , migrationComplexity = "n/a"
      , payload =
          LogPatternRateChangeP
            LogPatternRateChangeData
              { patternHash = lp.patternHash
              , logPattern = lp.logPattern
              , sampleMessage = lp.sampleMessage
              , logLevel = lp.logLevel
              , serviceName = lp.serviceName
              , sourceField = lp.sourceField
              , currentRatePerHour = sr.currentRate
              , baselineMean = sr.mean
              , baselineMad = sr.mad
              , zScore = abs sr.zScore
              , changePercent = changePercentVal
              , changeDirection = sr.direction
              , detectedAt = now
              }
      , timestamp = Just $ utcToZonedTime utc now
      }


-- | Strip token-highlight markup (";neutral⇒", ";badge-*⇒"), collapse drain
-- placeholders ("{integer}", "{uuid}", "{*}"), and fall back to sample / service
-- when the remaining text is mostly non-printable. Keeps Slack/Discord titles readable.
--
-- >>> sanitizeLogPatternTitle "connection;neutral⇒refused" Nothing Nothing
-- "connection refused"
-- >>> sanitizeLogPatternTitle "req {integer} took {integer} ms" Nothing Nothing
-- "req took ms"
-- >>> sanitizeLogPatternTitle "" (Just "GET /users 500") (Just "api")
-- "api: GET /users 500"
sanitizeLogPatternTitle :: Text -> Maybe Text -> Maybe Text -> Text
sanitizeLogPatternTitle raw sampleM serviceM =
  let stripped =
        unwords
          $ words
          $ flipfoldl' (uncurry T.replace) raw
          $ [(m, " ") | m <- [";neutral⇒", ";badge-error⇒", ";badge-warning⇒", ";badge-info⇒", ";badge-success⇒"]]
          <> [(p, "") | p <- ["{integer}", "{uuid}", "{float}", "{*}", "{hex}"]]
      -- printable-ASCII ratio > 0.7, as integer arithmetic
      usable = not (T.null stripped) && 10 * T.length (T.filter (\c -> isPrint c && isAscii c) stripped) > 7 * T.length stripped
      fallback = fromMaybe "log event" $ ((\svc s -> svc <> ": " <> T.take 80 s) <$> serviceM <*> sampleM) <|> sampleM <|> serviceM
   in T.take 100 $ if usable then stripped else fallback


-- | Create an issue for a new log pattern
createLogPatternIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> LogPatterns.LogPattern -> Eff es Issue
createLogPatternIssue projectId lp = do
  let lvl = T.toLower $ fromMaybe "" lp.logLevel
      severity
        | lvl == "error" = Critical
        | lvl `elem` ["warning", "warn"] = Warning
        | otherwise = Info
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = lp.patternHash
      , parentHash = Nothing
      , isFramework = False
      , service = lp.serviceName
      , critical = lvl == "error"
      , severity
      , title = "New Log Pattern: " <> sanitizeLogPatternTitle lp.logPattern lp.sampleMessage lp.serviceName
      , recommendedAction = "A new log pattern has been detected. Review to ensure it's expected behavior."
      , migrationComplexity = "n/a"
      , payload =
          LogPatternP
            LogPatternData
              { patternHash = lp.patternHash
              , logPattern = lp.logPattern
              , sampleMessage = lp.sampleMessage
              , logLevel = lp.logLevel
              , serviceName = lp.serviceName
              , sourceField = lp.sourceField
              , firstSeenAt = zonedTimeToUTC lp.firstSeenAt
              , occurrenceCount = lp.occurrenceCount
              }
      , timestamp = Just lp.firstSeenAt
      }


-- | Log Pattern issue data (new pattern detected)
data LogPatternData = LogPatternData
  { patternHash :: Text
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , sourceField :: Text
  , firstSeenAt :: UTCTime
  , occurrenceCount :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson LogPatternData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LogPatternData


data SpikeResult = SpikeResult
  { currentRate :: Double
  , mean :: Double
  , mad :: Double
  , zScore :: Double
  , direction :: RateChangeDirection
  }
  deriving stock (Eq, Show)


-- | Log Pattern Rate Change issue data (volume spike/drop)
data LogPatternRateChangeData = LogPatternRateChangeData
  { patternHash :: Text
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , sourceField :: Text
  , currentRatePerHour :: Double
  , baselineMean :: Double
  , baselineMad :: Double
  , zScore :: Double -- standard deviations from baseline
  , changePercent :: Double -- percentage change from baseline
  , changeDirection :: RateChangeDirection
  , detectedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson LogPatternRateChangeData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LogPatternRateChangeData


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.Time (UTCTime (..), fromGregorian)
-- >>> let sampleAlert = QueryAlertP (QueryAlertData "q" "name" "expr" 1 2 Above (UTCTime (fromGregorian 2026 1 1) 0))


-- | An issue's payload together with the 'IssueType' that selects it.
--
-- The two used to travel as independent values — an 'IssueType' tag beside an
-- untyped @Aeson AE.Value@ — so every writer could pair them wrongly and every
-- reader had to guess which type to parse as. This makes the pairing the only
-- representable thing.
data IssuePayload
  = ApiChangeP APIChangeData
  | RuntimeExceptionP RuntimeExceptionData
  | QueryAlertP QueryAlertData
  | LogPatternP LogPatternData
  | LogPatternRateChangeP LogPatternRateChangeData
  | PerformanceP PerformanceData
  | FrontendP FrontendData
  | FeedbackP FeedbackData
  | UptimeP UptimeData
  | CronP CronData
  deriving stock (Generic, Show)


-- | Exhaustive on purpose: a sixth 'IssueType' must fail to compile here rather
-- than silently acquire a wrong tag.
payloadType :: IssuePayload -> IssueType
payloadType = \case
  ApiChangeP{} -> ApiChange
  RuntimeExceptionP{} -> RuntimeException
  QueryAlertP{} -> QueryAlert
  LogPatternP{} -> LogPattern
  LogPatternRateChangeP{} -> LogPatternRateChange
  PerformanceP{} -> Performance
  FrontendP{} -> Frontend
  FeedbackP{} -> Feedback
  UptimeP{} -> Uptime
  CronP{} -> Cron


-- | The @issue_data@ column's value: the *bare* per-type object, exactly as before
-- this sum existed.
--
-- Deliberately NOT a derived 'AE.ToJSON' on 'IssuePayload'. The tag lives in the
-- @issue_type@ column, and SQL both reads inside this object
-- (@issue_data->>'anomaly_hashes'@) and merges into it with @jsonb ||@ (see
-- 'bumpOccurrenceCount'). Any tagged or @contents@-wrapped encoding would nest the
-- fields a level deeper, so those merges would write keys at the wrong level and
-- every row already in the table would stop parsing.
payloadJson :: IssuePayload -> AE.Value
payloadJson = \case
  ApiChangeP d -> AE.toJSON d
  RuntimeExceptionP d -> AE.toJSON d
  QueryAlertP d -> AE.toJSON d
  LogPatternP d -> AE.toJSON d
  LogPatternRateChangeP d -> AE.toJSON d
  PerformanceP d -> AE.toJSON d
  FrontendP d -> AE.toJSON d
  FeedbackP d -> AE.toJSON d
  UptimeP d -> AE.toJSON d
  CronP d -> AE.toJSON d


-- | Pair a stored @issue_type@ with its @issue_data@. 'Nothing' means the two
-- columns disagree, which is a fact about rows written before the pairing was
-- enforced — not a case the constructors can produce.
--
-- The @issue_type@ tag selects the parser, so the same JSON under the wrong tag is
-- rejected rather than coerced:
--
-- >>> fmap payloadType (parsePayload QueryAlert (payloadJson sampleAlert))
-- Just QueryAlert
--
-- >>> isNothing $ parsePayload RuntimeException (payloadJson sampleAlert)
-- True
--
-- >>> isNothing $ parsePayload RuntimeException (AE.String "junk")
-- True
parsePayload :: IssueType -> AE.Value -> Maybe IssuePayload
parsePayload t v = case t of
  ApiChange -> wrap ApiChangeP
  RuntimeException -> wrap RuntimeExceptionP
  QueryAlert -> wrap QueryAlertP
  LogPattern -> wrap LogPatternP
  LogPatternRateChange -> wrap LogPatternRateChangeP
  Performance -> wrap PerformanceP
  Frontend -> wrap FrontendP
  Feedback -> wrap FeedbackP
  Uptime -> wrap UptimeP
  Cron -> wrap CronP
  where
    wrap :: AE.FromJSON a => (a -> IssuePayload) -> Maybe IssuePayload
    wrap f = case AE.fromJSON v of
      AE.Success d -> Just (f d)
      AE.Error _ -> Nothing


-- | 'parsePayload' over a stored row.
issuePayload :: Issue -> Maybe IssuePayload
issuePayload i = parsePayload i.issueType (getAeson i.issueData)


data MkIssueOpts = MkIssueOpts
  { projectId :: Projects.ProjectId
  , targetHash :: Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , service :: Maybe Text
  , critical :: Bool
  , severity :: IssueSeverity
  , title :: Text
  , recommendedAction :: Text
  , migrationComplexity :: Text
  , payload :: IssuePayload
  -- ^ Carries its own 'IssueType'; the two can no longer disagree.
  , timestamp :: Maybe ZonedTime
  }


mkIssue :: (Time :> es, UUIDEff :> es) => MkIssueOpts -> Eff es Issue
mkIssue opts = do
  issueId <- UUIDId <$> genUUID
  zonedNow <- maybe (utcToZonedTime utc <$> Time.currentTime) pure opts.timestamp
  pure
    Issue
      { id = issueId
      , createdAt = zonedNow
      , updatedAt = zonedNow
      , projectId = opts.projectId
      , issueType = payloadType opts.payload
      , targetHash = opts.targetHash
      , parentHash = opts.parentHash
      , isFramework = opts.isFramework
      , endpointHash = opts.targetHash
      , acknowledgedAt = Nothing
      , acknowledgedBy = Nothing
      , archivedAt = Nothing
      , title = opts.title
      , service = opts.service
      , environment = Nothing
      , critical = opts.critical
      , severity = opts.severity
      , recommendedAction = opts.recommendedAction
      , migrationComplexity = opts.migrationComplexity
      , affectedRequests = 0
      , affectedClients = 0
      , errorRate = Nothing
      , issueData = Aeson $ payloadJson opts.payload
      , requestPayloads = Aeson []
      , responsePayloads = Aeson []
      , llmEnhancedAt = Nothing
      , llmEnhancementVersion = Nothing
      , seqNum = 0 -- Auto-assigned by DB trigger
      , cooldownUntil = Nothing
      , lastNotifiedAt = Nothing
      , acknowledgedUntil = Nothing
      , assigneeId = Nothing
      , archivedUntil = Nothing
      , archiveUntilEscalating = False
      }


-- Activity Log

data IssueEvent
  = IECreated
  | IEAcknowledged
  | IEUnacknowledged
  | IEArchived
  | IEUnarchived
  | IEResolved
  | IEReopened
  | IERegressed
  | IEAssigned
  | IEUnassigned
  | IEAutoResolved
  | IEEscalated
  | IEAckExpired
  | IECommented
  | IEViewed
  | IELinked
  | IEMerged
  | IESpam
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField, ToSchema) via WrappedEnumSC 'Nothing "IE" IssueEvent


-- | What one alert episode recorded, as stored in @apis.incident_events@.
data EpisodeKind = EKAlert | EKObservation | EKReminder | EKDataUnavailable | EKRecovered | EKResolved
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "EK" EpisodeKind


-- | One thing that happened to an issue: a lifecycle transition someone (or the
-- ack sweeper) caused, or an alert episode the delivery pipeline recorded.
--
-- These were two tables telling one story out of order — the audit log knew an
-- issue was acknowledged at 09:12 and the episode ledger knew it fired again at
-- 09:40, and neither could say so. Interleaved they read as a sequence.
data ActivityEvent = Lifecycle IssueEvent | Episode EpisodeKind
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | The stored spelling of a timeline row. Episode kinds stay namespaced because
-- the two vocabularies overlap — @resolved@ means both.
--
-- >>> parseActivityEvent "acknowledged"
-- Just (Lifecycle IEAcknowledged)
-- >>> parseActivityEvent "episode:data_unavailable"
-- Just (Episode EKDataUnavailable)
-- >>> (parseActivityEvent "episode:nonsense", parseActivityEvent "nonsense")
-- (Nothing,Nothing)
parseActivityEvent :: Text -> Maybe ActivityEvent
parseActivityEvent raw = case T.stripPrefix "episode:" raw of
  Just kind -> Episode <$> decodeEnumSC @"EK" (toString kind)
  Nothing -> Lifecycle <$> rightToMaybe (parseUrlPiece raw)


data IssueActivity = IssueActivity
  { event :: ActivityEvent
  , createdBy :: Maybe Projects.UserId
  , createdAt :: UTCTime
  , metadata :: Maybe AE.Value
  -- ^ A comment's @body@, a link's @url@/@title@; the lifecycle's own details otherwise.
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


-- | The issue's most recent state-changing event, for the detail page's badge.
--
-- The list view derives the same thing as a LATERAL inside its paged query
-- ('selectIssues'); pulling that out into a shared helper would turn one join
-- into an N+1 across the page, so the single-issue case gets its own scalar. The
-- event set is the one that matters — the two must agree on *which* events count
-- as a state change, or the list and the detail page will disagree about whether
-- an issue has regressed.
selectLatestStateEvent :: DB es => IssueId -> Eff es (Maybe IssueEvent)
selectLatestStateEvent iid =
  -- Decoded as Text then parsed, the same way 'parseIssueType' handles its column:
  -- IssueEvent derives DecodeValue but not DecodeRow, so it cannot be a row on its own.
  (rightToMaybe . parseUrlPiece =<<)
    . listToMaybe @Text
    <$> Hasql.interp
      [HI.sql| SELECT a.event FROM apis.issue_activity_log a
               WHERE a.issue_id = #{iid} AND a.event IN ('resolved', 'auto_resolved', 'reopened', 'regressed', 'escalated', 'ack_expired')
               ORDER BY a.created_at DESC LIMIT 1 |]


logIssueActivity :: (DB es, Time :> es) => IssueId -> IssueEvent -> Maybe Projects.UserId -> Maybe AE.Value -> Eff es ()
logIssueActivity issueId event createdBy metadataM = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.issue_activity_log (issue_id, event, created_by, metadata, created_at)
    SELECT #{issueId}, #{event}, #{createdBy}, #{Aeson <$> metadataM}, #{now}
    WHERE EXISTS (SELECT 1 FROM apis.issues WHERE id = #{issueId}) |]


-- | An issue's whole timeline, newest first: its own lifecycle log plus every
-- event of every alert episode that hung off it. The episodes are reachable from
-- here and nowhere else, which is the point — an episode is a chapter of an
-- issue, not a thing to go and look at separately.
selectIssueActivity :: (DB es, Log :> es) => Projects.ProjectId -> IssueId -> Eff es [IssueActivity]
selectIssueActivity pid issueId = do
  rows <-
    Hasql.interp @[(Text, Maybe Projects.UserId, UTCTime, Maybe (HI.AsJsonb AE.Value))]
      [HI.sql| SELECT a.event::text, a.created_by, a.created_at, a.metadata
        FROM apis.issue_activity_log a
        JOIN apis.issues i ON i.id = a.issue_id
        WHERE a.issue_id = #{issueId} AND i.project_id = #{pid}
        UNION ALL
        SELECT 'episode:' || ev.event_kind, ev.actor_id, ev.observed_at, NULL::jsonb
        FROM apis.incident_events ev
        JOIN apis.incident_episodes ep ON ep.id = ev.episode_id
        WHERE ep.issue_id = #{issueId} AND ep.project_id = #{pid}
        ORDER BY 3 DESC LIMIT 200 |]
  -- A spelling neither vocabulary knows means the DB grew an event kind that
  -- 'ActivityEvent' has not; say so rather than dropping it off the timeline.
  let (unparsable, activities) = partitionEithers (toActivity <$> rows)
  unless (null unparsable)
    $ logAttention "ISSUE_TIMELINE_UNKNOWN_EVENT" (AE.object ["issue_id" AE..= issueId, "events" AE..= ordNub unparsable])
  pure activities
  where
    toActivity (raw, by, at, meta) = maybe (Left raw) (\e -> Right $ IssueActivity e by at ((\(HI.AsJsonb v) -> v) <$> meta)) (parseActivityEvent raw)


-- | Log that a user opened the issue, at most once a day, for the page's People list.
recordIssueView :: (DB es, Time :> es) => IssueId -> Projects.UserId -> Eff es ()
recordIssueView issueId uid = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.issue_activity_log (issue_id, event, created_by, created_at)
             SELECT #{issueId}, 'viewed', #{uid}, #{now}
             WHERE NOT EXISTS (SELECT 1 FROM apis.issue_activity_log
                               WHERE issue_id = #{issueId} AND created_by = #{uid} AND event = 'viewed'
                                 AND created_at > #{now}::timestamptz - INTERVAL '1 day') |]


-- Reports

type ReportId = UUIDId "report"


data Report = Report
  { id :: ReportId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , reportType :: Projects.ReportType
  , reportJson :: AE.Value
  , startTime :: UTCTime
  , endTime :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Report)


data ReportListItem = ReportListItem
  { id :: ReportId
  , createdAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , reportType :: Projects.ReportType
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ReportListItem)


addReport :: DB es => Report -> Eff es ()
addReport (r :: Report) =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.reports (id, created_at, updated_at, project_id, report_type, report_json, start_time, end_time)
      VALUES (#{r.id}, #{r.createdAt}, #{r.updatedAt}, #{r.projectId}, #{r.reportType}, #{r.reportJson}, #{r.startTime}, #{r.endTime}) |]


getReportById :: DB es => Projects.ProjectId -> ReportId -> Eff es (Maybe Report)
getReportById pid rid = Hasql.interpOne (selectFrom @Report <> [HI.sql| WHERE id = #{rid} AND project_id = #{pid} |])


reportHistoryByProject :: DB es => Projects.ProjectId -> Int -> Eff es [ReportListItem]
reportHistoryByProject pid page =
  Hasql.interp (selectFrom @ReportListItem <> [HI.sql| WHERE project_id = #{pid} ORDER BY created_at DESC LIMIT 20 OFFSET #{page * 20} |])


getLatestReportByType :: DB es => Projects.ProjectId -> Projects.ReportType -> Eff es (Maybe Report)
getLatestReportByType pid rType = Hasql.interpOne (selectFrom @Report <> [HI.sql| WHERE project_id = #{pid} AND report_type = #{rType} ORDER BY created_at DESC LIMIT 1 |])


createErrorSpikeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> ErrorPatterns.ErrorPatternWithCurrentRate -> Double -> Double -> Double -> Eff es Issue
createErrorSpikeIssue projectId errRate currentRate baselineMean zScore =
  let increasePercent = if baselineMean > 0 then ((currentRate / baselineMean) - 1) * 100 else 0
   in mkErrorIssue
        projectId
        errRate
        (round currentRate)
        (const $ "Error Spike: " <> errRate.errorType <> " (" <> showPct increasePercent <> " increase)")
        ("Error rate has spiked " <> showRounded "" zScore <> " standard deviations above baseline. Current: " <> showRate currentRate <> ", Baseline: " <> showRate baselineMean <> ". Investigate recent deployments or changes.")


-- | Create a new issue for an error pattern.
createNewErrorIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> ErrorPatterns.ErrorPattern -> Eff es Issue
createNewErrorIssue projectId err =
  mkErrorIssue
    projectId
    err
    1
    (\isFw -> (if isFw then "Framework Error: " else "New Error: ") <> err.errorType <> " - " <> T.take 80 err.message)
    "Investigate the new error and implement a fix."


-- | Fields shared by 'ErrorPatterns.ErrorPattern' and 'ErrorPatterns.ErrorPatternWithCurrentRate'.
type ErrorLike p =
  ( HasField "errorType" p Text
  , HasField "hash" p Text
  , HasField "isFramework" p Bool
  , HasField "message" p Text
  , HasField "parentHash" p (Maybe Text)
  , HasField "service" p (Maybe Text)
  , HasField "stacktrace" p Text
  )


-- | Build a RuntimeException issue from an error pattern. Framework/transport errors
-- key on the *parent* (broad) hash so per-route variants collapse into one issue via
-- the (project_id, target_hash, issue_type) ON CONFLICT index; app errors — and
-- framework errors with no parent hash — keep their narrow per-route identity. The
-- parent hash is always stored for UI rollup, and @mkTitle@ is told which hash won.
-- | One performance issue per (kind, service, transaction, query): the target hash is
-- that key, so the open-issue upsert folds repeat detections into one issue.
createPerformanceIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Maybe Text -> PerformanceData -> Eff es Issue
createPerformanceIssue projectId service d =
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = toXXHash $ T.intercalate "|" [display d.kind, fromMaybe "" service, fromMaybe "" d.transaction, d.query]
      , parentHash = Nothing
      , isFramework = False
      , service
      , critical = False
      , severity = Warning
      , title = case d.kind of
          PKNPlusOne -> "N+1 Query: " <> T.take 100 d.query
          PKSlowQuery -> "Slow DB Query: " <> T.take 100 d.query
      , recommendedAction = case d.kind of
          PKNPlusOne -> "Batch these queries: load the rows in one query (a join or an IN list) instead of one per item."
          PKSlowQuery -> "Check the query plan: add an index for its filter, or narrow what it reads."
      , migrationComplexity = "n/a"
      , timestamp = Nothing
      , payload = PerformanceP d
      }


-- | One downtime issue per check: the target hash is the check id, so a check that
-- stays down folds every failed probe into the open issue.
createUptimeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> UptimeData -> Eff es Issue
createUptimeIssue projectId d =
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = uptimeTargetHash d.checkId
      , parentHash = Nothing
      , isFramework = False
      , service = Just d.name
      , critical = True
      , severity = Critical
      , title = "Downtime detected for " <> d.url
      , recommendedAction = "The check at " <> d.url <> " is failing: " <> d.reason <> "."
      , migrationComplexity = "n/a"
      , timestamp = Nothing
      , payload = UptimeP d
      }


uptimeTargetHash :: Text -> Text
uptimeTargetHash = ("uptime:" <>)


-- | One open issue per cron monitor, missed or failed.
createCronIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> CronData -> Eff es Issue
createCronIssue projectId d =
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = cronTargetHash d.monitorId
      , parentHash = Nothing
      , isFramework = False
      , service = Nothing
      , critical = True
      , severity = Critical
      , title = bool "Cron failed: " "Cron missed: " (d.failure == CFMissed) <> d.name
      , recommendedAction = case d.failure of
          CFMissed -> "No check-in from " <> d.slug <> " in its window: check the job ran and can reach Monoscope."
          CFFailed -> "The job " <> d.slug <> " reported monitor.status = error on its last run."
      , migrationComplexity = "n/a"
      , timestamp = Nothing
      , payload = CronP d
      }


cronTargetHash :: Text -> Text
cronTargetHash = ("cron:" <>)


-- | One issue per feedback submission, keyed by its record id so a rescan cannot duplicate it.
createFeedbackIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Maybe Text -> Text -> FeedbackData -> Eff es Issue
createFeedbackIssue projectId service recordId d =
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = "feedback:" <> recordId
      , parentHash = Nothing
      , isFramework = False
      , service
      , critical = False
      , severity = Info
      , title = "Feedback: " <> T.take 100 d.message
      , recommendedAction = "Reply to the user, or mark it as spam."
      , migrationComplexity = "n/a"
      , timestamp = Just (utcToZonedTime utc d.observedAt)
      , payload = FeedbackP d
      }


-- | One frontend issue per (kind, page path, element).
createFrontendIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Maybe Text -> FrontendData -> Eff es Issue
createFrontendIssue projectId service d =
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash = toXXHash $ T.intercalate "|" [display d.kind, maybe "" (T.takeWhile (/= '?')) d.pageUrl, d.element, fromMaybe "" d.selector]
      , parentHash = Nothing
      , isFramework = False
      , service
      , critical = False
      , severity = Warning
      , title = bool "Dead Click: " "Rage Click: " (d.kind == FKRageClick) <> T.take 100 d.element
      , recommendedAction = case d.kind of
          FKRageClick -> "Users clicked this repeatedly: check it responds, shows progress, and is not disabled without saying why."
          FKDeadClick -> "Clicking this changed nothing on the page: wire up its handler or stop it looking clickable."
      , migrationComplexity = "n/a"
      , timestamp = Nothing
      , payload = FrontendP d
      }


mkErrorIssue :: (ErrorLike p, Time :> es, UUIDEff :> es) => Projects.ProjectId -> p -> Int -> (Bool -> Text) -> Text -> Eff es Issue
mkErrorIssue projectId p occurrences mkTitle recommendedAction = do
  now <- Time.currentTime
  let (isFramework, targetHash) = maybe (False, p.hash) (True,) (p.parentHash <* guard p.isFramework)
  mkIssue
    MkIssueOpts
      { projectId
      , targetHash
      , parentHash = p.parentHash
      , isFramework
      , service = p.service
      , critical = True
      , severity = Critical
      , title = mkTitle isFramework
      , recommendedAction
      , migrationComplexity = "n/a"
      , timestamp = Nothing
      , payload =
          RuntimeExceptionP
            RuntimeExceptionData
              { errorType = p.errorType
              , errorMessage = p.message
              , stackTrace = p.stacktrace
              , requestPath = Nothing
              , requestMethod = Nothing
              , occurrenceCount = occurrences
              , firstSeen = now
              , lastSeen = now
              }
      }
