-- | Issues module - User-facing representation of anomalies
--
-- This module is the primary interface for the anomaly detection system.
-- Issues are created from anomalies detected by database triggers and
-- background jobs. They represent actionable items for developers.
--
-- Issue Types:
-- - APIChange: Groups endpoint/shape/format changes by endpoint
-- - RuntimeException: Individual issues for each error pattern
-- - QueryAlert: Threshold violations from monitoring
--
-- For detailed documentation on the anomaly detection system, see:
-- - docs/anomaly-detection-system.md (architecture overview)
-- - docs/anomaly-detection-triggers.sql (database trigger details)
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
  ChatRole (..),
  IssueSeverity (..),
  LogPatternRateChangeData (..),
  LogPatternData (..),
  RateChangeDirection (..),

  -- * Database Operations
  insertIssue,
  selectIssueById,
  selectIssues,
  IssueProjection (..),
  IssueFilters (..),
  NullFilter (..),
  defIssueFilters,
  updateIssueWithNewAnomaly,
  updateIssueEnhancement,
  updateIssueCriticality,
  AckWindow (..),
  AckSet (..),
  ackUntil,
  indefiniteUntil,
  isSilenced,
  setAckState,
  expireAcks,
  setArchiveState,
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
  ConversationType (..),
  getOrCreateConversation,
  insertChatMessage,
  selectChatHistory,
  tryAcquireChatMigrationLock,
  releaseChatMigrationLock,

  -- * Thread ID Helpers
  slackThreadToConversationId,
  textToConversationId,

  -- * Activity Log
  IssueEvent (..),
  IssueActivity (..),
  logIssueActivity,
  selectLatestStateEvent,
  selectIssueActivity,

  -- * Issue Summary (for reports/emails)
  IssueSummary (..),
  toIssueSummary,

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
import Data.Hashable (hash)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Time (Day (ModifiedJulianDay), UTCTime (..), addDays, addUTCTime)
import Data.Time.LocalTime (ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
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
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Records (HasField)
import Hasql.Interpolate qualified as HI
import Models.Apis.Anomalies (PayloadChange)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.LogPatterns (RateChangeDirection (..))
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), rawSql, selectFrom)
import Relude hiding (id)
import Servant (FromHttpApiData (..), ServerError, err500, errBody)
import System.Types (DB)


type IssueId = UUIDId "issue"


-- | Issue types
data IssueType
  = ApiChange
  | RuntimeException
  | QueryAlert
  | LogPattern
  | LogPatternRateChange
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField, ToSchema) via WrappedEnumSC ('Just "apis.issue_type") "" IssueType


-- | Hash prefix used in otel_logs_and_spans hashes column
hashPrefix :: IssueType -> Maybe Text
hashPrefix = \case
  LogPattern -> Just "pat:"
  LogPatternRateChange -> Just "pat:"
  RuntimeException -> Just "err:"
  ApiChange -> Just "" -- endpoint hash is stored unprefixed on span hashes
  QueryAlert -> Nothing


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
isBoilerplateAction a = a `elem` [defaultRecommendedAction, queryAlertRecommendedAction]


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
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "" IssueSeverity


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


toIssueSummary :: IssueL -> IssueSummary
toIssueSummary IssueL{base, activityBuckets} =
  IssueSummary base.id base.title base.critical base.severity base.issueType (Just $ V.toList activityBuckets)


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
  Just (ApiChangeP d) -> all V.null [d.newFields, d.deletedFields, d.modifiedFields]
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
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


-- Generic HI.DecodeRow can't derive this: Issue has DecodeRow but not DecodeValue.
instance HI.DecodeRow IssueL where
  decodeRow = IssueL <$> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow


-- | Insert a single issue
-- ON CONFLICT dedup applies to all issue types on (project_id, target_hash, issue_type)
-- but only for open issues (not acknowledged/archived). Preserves occurrence_count and first_seen.
insertIssue :: DB es => Issue -> Eff es ()
insertIssue (i :: Issue) =
  Hasql.interpExecute_
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
touchIssue extra issueId = do
  now <- Time.currentTime
  Hasql.interpExecute_
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
  , services :: [Text]
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
    , services = []
    , types = []
    , timeRange = Nothing
    , order = Nothing
    , period = "7d"
    , hideLowSeverity = False
    , limit = 50
    , offset = 0
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
        _ -> pfx <> "critical DESC, " <> pfx <> "created_at DESC"
      arrF pfx col xs = if null xs then mempty else [HI.sql| AND ^{pfx}^{col} = ANY(#{xs}::text[])|]
      mkFilters pfx =
        foldMap (\(s, e) -> [HI.sql| AND ^{pfx}created_at >= #{s} AND ^{pfx}created_at <= #{e}|]) f.timeRange
          <> sqlNullFilter pfx [HI.sql|acknowledged_at|] f.ack
          <> sqlNullFilter pfx [HI.sql|archived_at|] f.archive
          <> bool mempty [HI.sql| AND (^{pfx}severity IS NULL OR ^{pfx}severity != 'low')|] f.hideLowSeverity
          <> arrF pfx [HI.sql|service|] f.services
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
          i.target_hash, NULL::text, i.seq_num::bigint, i.parent_hash, i.is_framework, i.cooldown_until, i.last_notified_at, i.acknowledged_until,
          CASE
            WHEN i.issue_type = 'runtime_exception' THEN COALESCE(err_ev.cnt, 0)
            WHEN i.issue_type IN ('log_pattern', 'log_pattern_rate_change') THEN COALESCE(lp_ev.cnt, 0)
            ELSE i.affected_requests
          END::bigint,
          i.updated_at, lat.event,
          CASE
            WHEN i.issue_type = 'runtime_exception' THEN COALESCE(err_ev.buckets, '{}'::bigint[])
            WHEN i.issue_type IN ('log_pattern', 'log_pattern_rate_change') THEN COALESCE(lp_ev.buckets, '{}'::bigint[])
            ELSE '{}'::bigint[]
          END
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
setArchiveState :: DB es => Projects.ProjectId -> [IssueId] -> Maybe UTCTime -> Eff es Int64
setArchiveState pid iids mTs
  | null iids = pure 0
  | otherwise =
      Hasql.interpExecute
        [HI.sql|
          UPDATE apis.issues
          SET archived_at = #{mTs}, updated_at = COALESCE(#{mTs}, updated_at)
          WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]


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
createAPIChangeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> NonEmpty Anomalies.AnomalyVM -> Eff es Issue
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
      , service = Just $ Anomalies.detectService Nothing firstAnomaly.endpointUrlPath
      , critical = isCritical
      , severity = if isCritical then Critical else Warning
      , title =
          if any ((== Anomalies.ATEndpoint) . (.anomalyType)) anomalies
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
      , title = queryName <> " threshold " <> display thresholdType <> " " <> show threshold
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
data ConversationType = CTAnomaly | CTTrace | CTLogExplorer | CTDashboard | CTSlackThread | CTDiscordThread
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default) -- required by Default AIConversation; first constructor = CTAnomaly
  deriving (Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "CT" ConversationType


-- | AI Conversation metadata
data AIConversation = AIConversation
  { id :: UUIDId "ai_conversation"
  , projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation" -- The contextual ID (issue_id, trace_id, etc.)
  , conversationType :: ConversationType
  , context :: Maybe (Aeson AE.Value) -- Initial context for the AI
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, ToRow)


-- | Author of a stored AI chat message. Encodes to "user"/"assistant"/"system"
-- (byte-identical to the previous free-text column).
data ChatRole = ChatUser | ChatAssistant | ChatSystem
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "Chat" ChatRole


data AIChatMessage = AIChatMessage
  { id :: UUIDId "ai_chat"
  , projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"
  , role :: ChatRole
  , content :: Text
  , widgets :: Maybe (Aeson AE.Value) -- Array of widget configs
  , metadata :: Maybe (Aeson AE.Value) -- Additional metadata
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, ToRow)


-- | Get or create a conversation (race-condition safe via ON CONFLICT + RETURNING)
getOrCreateConversation :: (DB es, Error ServerError :> es, Time :> es) => Projects.ProjectId -> UUIDId "conversation" -> ConversationType -> AE.Value -> Eff es AIConversation
getOrCreateConversation pid convId convType ctx = do
  now <- Time.currentTime
  Hasql.interpOne
    [HI.sql| INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, context)
              VALUES (#{pid}, #{convId}, #{convType}, #{Aeson ctx}) ON CONFLICT (project_id, conversation_id) DO UPDATE SET updated_at = #{now}
              RETURNING id, project_id, conversation_id, conversation_type, context, created_at, updated_at |]
    >>= (`whenNothing` throwError err500{errBody = "getOrCreateConversation: RETURNING clause must return a row"})


-- | Insert a new chat message
insertChatMessage :: DB es => Projects.ProjectId -> UUIDId "conversation" -> ChatRole -> Text -> Maybe AE.Value -> Maybe AE.Value -> Eff es ()
insertChatMessage pid convId chatRole chatContent widgetsM metadataM =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, widgets, metadata)
            VALUES (#{pid}, #{convId}, #{chatRole}, #{chatContent}, #{Aeson <$> widgetsM}, #{Aeson <$> metadataM}) |]


-- | Select chat history for a conversation (oldest first)
selectChatHistory :: DB es => UUIDId "conversation" -> Eff es [AIChatMessage]
selectChatHistory convId =
  Hasql.interp
    [HI.sql| SELECT id, project_id, conversation_id, role, content, widgets, metadata, created_at
            FROM apis.ai_chat_messages
            WHERE conversation_id = #{convId}
            ORDER BY created_at ASC
            LIMIT 200 |]


-- | Generate deterministic UUID v5 from text (uses OID namespace)
textToConversationId :: Text -> UUIDId "conversation"
textToConversationId = UUIDId . UUID5.generateNamed UUID5.namespaceOID . BS.unpack . encodeUtf8


slackThreadToConversationId :: Text -> Text -> UUIDId "conversation"
slackThreadToConversationId cid ts = textToConversationId (cid <> ":" <> ts)


chatMigrationLockKey :: UUIDId "conversation" -> Int64
chatMigrationLockKey convId = fromIntegral @Int @Int64 $ abs $ hash $ show convId.unwrap


-- | Try to acquire an advisory lock for chat migration to prevent race conditions.
-- Returns True if lock was acquired, False if already locked (another request is migrating).
-- Uses PostgreSQL advisory locks which are automatically released on connection close.
tryAcquireChatMigrationLock :: DB es => UUIDId "conversation" -> Eff es Bool
tryAcquireChatMigrationLock convId = or <$> Hasql.interp [HI.sql| SELECT pg_try_advisory_lock(#{chatMigrationLockKey convId}) |]


-- | Release a chat migration advisory lock so that a failed migration can be retried
-- on a subsequent event instead of being silently blocked for the connection's lifetime.
releaseChatMigrationLock :: DB es => UUIDId "conversation" -> Eff es ()
releaseChatMigrationLock convId = Hasql.interpExecute_ [HI.sql| SELECT pg_advisory_unlock(#{chatMigrationLockKey convId}) |]


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
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField, ToSchema) via WrappedEnumSC 'Nothing "IE" IssueEvent


data IssueActivity = IssueActivity
  { id :: Int64
  , issueId :: IssueId
  , event :: IssueEvent
  , createdBy :: Maybe Projects.UserId
  , metadata :: Maybe (Aeson AE.Value)
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)


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


selectIssueActivity :: DB es => Projects.ProjectId -> IssueId -> Eff es [IssueActivity]
selectIssueActivity pid issueId =
  Hasql.interp
    [HI.sql| SELECT a.id, a.issue_id, a.event, a.created_by, a.metadata, a.created_at
        FROM apis.issue_activity_log a
        JOIN apis.issues i ON i.id = a.issue_id
        WHERE a.issue_id = #{issueId} AND i.project_id = #{pid}
        ORDER BY a.created_at DESC LIMIT 200 |]


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
