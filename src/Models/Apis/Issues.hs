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
  selectIssueByIdScoped,
  selectIssues,
  selectIssuesByFilters,
  findOpenIssueForEndpoint,
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
  selectLatestIssueByHash,
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
  issueIdText,
  parseIssueType,
  hashPrefix,
  defaultRecommendedAction,
  serviceLabel,
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
  discordThreadToConversationId,

  -- * Activity Log
  IssueEvent (..),
  IssueActivity (..),
  logIssueActivity,
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
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), idToText, rawSql, selectFrom)
import Relude hiding (id)
import Servant (FromHttpApiData (..), ServerError, err500, errBody)
import System.Types (DB)


type IssueId = UUIDId "issue"


issueIdText :: IssueId -> Text
issueIdText = idToText


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


showRate :: Double -> Text
showRate x = show (round x :: Int) <> "/hr"


showPct :: RealFrac a => a -> Text
showPct x = show (round x :: Int) <> "%"


serviceLabel :: Maybe Text -> Text
serviceLabel = fromMaybe "unknown-service"


isNewEndpointOnly :: Issue -> Bool
isNewEndpointOnly issue =
  issue.issueType == ApiChange && case AE.fromJSON (getAeson issue.issueData) of
    AE.Success (d :: APIChangeData) -> all V.null [d.newFields, d.deletedFields, d.modifiedFields]
    AE.Error _ -> False


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


-- | Select issue by ID
selectIssueById :: DB es => IssueId -> Eff es (Maybe Issue)
selectIssueById iid = Hasql.interpOne (selectFrom @Issue <> [HI.sql| WHERE id = #{iid} |])


selectIssueByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
selectIssueByHash pid tgtHash = Hasql.interpOne (selectFrom @Issue <> [HI.sql| WHERE project_id = #{pid} AND target_hash = #{tgtHash} ORDER BY updated_at DESC, id DESC LIMIT 1 |])


-- | Find most recent RuntimeException issue for a given hash (including acknowledged/archived)
selectLatestIssueByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
selectLatestIssueByHash pid tgtHash =
  Hasql.interpOne (selectFrom @Issue <> [HI.sql| WHERE project_id = #{pid} AND target_hash = #{tgtHash} AND issue_type = #{RuntimeException}::apis.issue_type ORDER BY created_at DESC LIMIT 1 |])


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


-- | @AND pfx.col IS [NOT] NULL@ clause, or empty if the filter is unset. Shared
-- between 'selectIssues' (prefixed, joined query) and 'selectIssuesByFilters'
-- (unprefixed, single-table query — pass @mempty@ for @pfx@).
sqlNullFilter :: HI.Sql -> HI.Sql -> Maybe Bool -> HI.Sql
sqlNullFilter pfx col = foldMap (bool [HI.sql| AND ^{pfx}^{col} IS NULL|] [HI.sql| AND ^{pfx}^{col} IS NOT NULL|])


-- | Select issues with filters, returns issues and total count for pagination
-- period: "24h" = 24 hourly buckets, "7d" = 7 daily buckets (default)
selectIssues :: (DB es, Time :> es) => Projects.ProjectId -> Maybe Bool -> Maybe Bool -> Int -> Int -> Maybe (UTCTime, UTCTime) -> Maybe Text -> Text -> [Text] -> [Text] -> Eff es ([IssueL], Int)
selectIssues pid isAcknowledged isArchived limit offset timeRangeM sortM period serviceFilters typeFilters = do
  now <- Time.currentTime
  -- period controls bucket granularity: "24h" = hourly, "7d" = daily.
  -- seriesStart/step are bound here (not via SQL NOW()) so charts honour the test clock.
  let (seriesStart, stepSql) = case period of
        "24h" -> (addUTCTime (-(23 * 3600)) now, [HI.sql|interval '1 hour'|])
        _ -> (UTCTime (addDays (-6) (utctDay now)) 0, [HI.sql|interval '1 day'|])
      -- Inbox tab (unacked + unarchived) hides severity='low' so demoted silent drops don't clutter the view.
      isInbox = isAcknowledged == Just False && isArchived == Just False
      orderBy = rawSql case T.uncons =<< sortM of
        Just (s, c) | s == '-' || s == '+', c `elem` ["created_at", "updated_at", "title"] -> "i." <> c <> bool " ASC" " DESC" (s == '-')
        _ -> "i.critical DESC, i.created_at DESC"
      arrF pfx col xs = if null xs then mempty else [HI.sql| AND ^{pfx}^{col} = ANY(#{xs}::text[])|]
      mkFilters pfx =
        foldMap (\(s, e) -> [HI.sql| AND ^{pfx}created_at >= #{s} AND ^{pfx}created_at <= #{e}|]) timeRangeM
          <> sqlNullFilter pfx [HI.sql|acknowledged_at|] isAcknowledged
          <> sqlNullFilter pfx [HI.sql|archived_at|] isArchived
          <> bool mempty [HI.sql| AND (^{pfx}severity IS NULL OR ^{pfx}severity != 'low')|] isInbox
          <> arrF pfx [HI.sql|service|] serviceFilters
          <> arrF pfx [HI.sql|issue_type::text|] typeFilters
      iFilters = mkFilters [HI.sql|i.|]
      cFilters = mkFilters mempty
  issues <-
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
        ORDER BY ^{orderBy}
        LIMIT #{limit} OFFSET #{offset} |]
  total <-
    fromMaybe 0
      <$> Hasql.interpOne
        [HI.sql| SELECT COUNT(*)::bigint FROM apis.issues WHERE project_id = #{pid} ^{cFilters} |]
  pure (issues, total)


-- | Find open issue for endpoint
findOpenIssueForEndpoint :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
findOpenIssueForEndpoint pid tgtHash =
  Hasql.interpOne (selectFrom @Issue <> [HI.sql| WHERE project_id = #{pid} AND issue_type = #{ApiChange}::apis.issue_type AND target_hash = #{tgtHash} AND acknowledged_at IS NULL AND archived_at IS NULL LIMIT 1 |])


-- | Update issue with new anomaly data
updateIssueWithNewAnomaly :: (DB es, Time :> es) => IssueId -> APIChangeData -> Eff es ()
updateIssueWithNewAnomaly issueId newData = do
  now <- Time.currentTime
  let jdata = Aeson newData
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.issues SET
        issue_data = issue_data || #{jdata}::jsonb,
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
expireAcks now =
  HI.getOneColumn
    <<$>> Hasql.interp
      [HI.sql|
        UPDATE apis.issues
        SET acknowledged_at = NULL, acknowledged_by = NULL, acknowledged_until = NULL
        WHERE acknowledged_at IS NOT NULL AND archived_at IS NULL AND acknowledged_until <= #{now}
        RETURNING id |]


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


-- | Scoped lookup: returns Nothing if the issue belongs to a different project.
selectIssueByIdScoped :: DB es => Projects.ProjectId -> IssueId -> Eff es (Maybe Issue)
selectIssueByIdScoped pid iid =
  Hasql.interpOne (selectFrom @Issue <> [HI.sql| WHERE id = #{iid} AND project_id = #{pid} |])


-- | List issues with the filter/pagination surface the public API needs.
-- Returns raw 'Issue' rows (not 'IssueL' view rows) with a total count.
selectIssuesByFilters
  :: DB es
  => Projects.ProjectId
  -> Maybe Bool -- isAcknowledged: Just True = ack only, Just False = unack only, Nothing = any
  -> Maybe Bool -- isArchived
  -> Maybe Text -- issueType text (NULL/empty = any)
  -> Maybe Text -- service (NULL/empty = any)
  -> Int -- limit
  -> Int -- offset
  -> Eff es ([Issue], Int)
selectIssuesByFilters pid isAck isArch tyM svcM limit offset = do
  let eqF col = foldMap (\v -> if T.null v then mempty else [HI.sql| AND ^{col} = #{v}|])
      whereSql =
        [HI.sql| WHERE project_id = #{pid}|]
          <> sqlNullFilter mempty [HI.sql|acknowledged_at|] isAck
          <> sqlNullFilter mempty [HI.sql|archived_at|] isArch
          <> eqF [HI.sql|issue_type::text|] tyM
          <> eqF [HI.sql|service|] svcM
  rows <- Hasql.interp (selectFrom @Issue <> whereSql <> [HI.sql| ORDER BY updated_at DESC LIMIT #{limit} OFFSET #{offset} |])
  total <- fromMaybe 0 <$> Hasql.interpOne ([HI.sql| SELECT COUNT(*)::bigint FROM apis.issues |] <> whereSql)
  pure (rows, total)


-- | Create API Change issue from anomalies
createAPIChangeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> V.Vector Anomalies.AnomalyVM -> Eff es Issue
createAPIChangeIssue projectId endpointHash anomalies = do
  let firstAnomaly = V.head anomalies
      apiChangeData =
        APIChangeData
          { endpointMethod = fromMaybe "UNKNOWN" firstAnomaly.endpointMethod
          , endpointPath = fromMaybe "/" firstAnomaly.endpointUrlPath
          , endpointHost = fromMaybe "Unknown" firstAnomaly.endpointHost
          , anomalyHashes = V.map (.targetHash) anomalies
          , shapeChanges = V.empty
          , formatChanges = V.empty
          , newFields = V.concatMap (.shapeNewUniqueFields) anomalies
          , deletedFields = V.concatMap (.shapeDeletedFields) anomalies
          , modifiedFields = V.concatMap (.shapeUpdatedFieldFormats) anomalies
          }
      breakingChanges = V.length apiChangeData.deletedFields + V.length apiChangeData.modifiedFields
      isCritical = breakingChanges > 0
  mkIssue
    MkIssueOpts
      { projectId
      , issueType = ApiChange
      , targetHash = endpointHash
      , parentHash = Nothing
      , isFramework = False
      , service = Just $ Anomalies.detectService Nothing firstAnomaly.endpointUrlPath
      , critical = isCritical
      , severity = if isCritical then Critical else Warning
      , title =
          if V.any ((== Anomalies.ATEndpoint) . (.anomalyType)) anomalies
            then "New endpoint detected: " <> apiChangeData.endpointMethod <> " " <> apiChangeData.endpointPath <> " on " <> apiChangeData.endpointHost
            else "API structure has changed"
      , recommendedAction = defaultRecommendedAction
      , migrationComplexity = if breakingChanges > 5 then "high" else if breakingChanges > 0 then "medium" else "low"
      , issueData = apiChangeData
      , timestamp = Just firstAnomaly.createdAt
      }


-- | Create Query Alert issue
createQueryAlertIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> Text -> Text -> Double -> Double -> ThresholdDirection -> Eff es Issue
createQueryAlertIssue projectId queryId queryName queryExpr threshold actual thresholdType = do
  now <- Time.currentTime
  mkIssue
    MkIssueOpts
      { projectId
      , issueType = QueryAlert
      , targetHash = queryId
      , parentHash = Nothing
      , isFramework = False
      , service = Just "Monitoring"
      , critical = True
      , severity = Warning
      , title = queryName <> " threshold " <> display thresholdType <> " " <> show threshold
      , recommendedAction = "Review the query results and take appropriate action."
      , migrationComplexity = "n/a"
      , issueData =
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
  let ctxJ = Aeson ctx
  Hasql.interpOne
    [HI.sql| INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, context)
              VALUES (#{pid}, #{convId}, #{convType}, #{ctxJ}) ON CONFLICT (project_id, conversation_id) DO UPDATE SET updated_at = #{now}
              RETURNING id, project_id, conversation_id, conversation_type, context, created_at, updated_at |]
    >>= (`whenNothing` throwError err500{errBody = "getOrCreateConversation: RETURNING clause must return a row"})


-- | Insert a new chat message
insertChatMessage :: DB es => Projects.ProjectId -> UUIDId "conversation" -> ChatRole -> Text -> Maybe AE.Value -> Maybe AE.Value -> Eff es ()
insertChatMessage pid convId chatRole chatContent widgetsM metadataM = do
  let widgetsJ = Aeson <$> widgetsM
      metaJ = Aeson <$> metadataM
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, widgets, metadata)
            VALUES (#{pid}, #{convId}, #{chatRole}, #{chatContent}, #{widgetsJ}, #{metaJ}) |]


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


discordThreadToConversationId :: Text -> UUIDId "conversation"
discordThreadToConversationId = textToConversationId


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
      , issueType = LogPatternRateChange
      , targetHash = lp.patternHash
      , parentHash = Nothing
      , isFramework = False
      , service = lp.serviceName
      , critical = not silentDrop && sr.direction == Spike && lvl == "error"
      , severity
      , title
      , recommendedAction = "Log pattern volume " <> dir <> " detected. Current: " <> showRate sr.currentRate <> ", Baseline: " <> showRate sr.mean <> " (" <> show (round (abs sr.zScore) :: Int) <> " std devs)."
      , migrationComplexity = "n/a"
      , issueData =
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
  let replacements =
        [(m, " ") | m <- [";neutral⇒", ";badge-error⇒", ";badge-warning⇒", ";badge-info⇒", ";badge-success⇒"]]
          <> [(p, "") | p <- ["{integer}", "{uuid}", "{float}", "{*}", "{hex}"]]
      stripped = unwords $ words $ foldl' (\t (a, b) -> T.replace a b t) raw replacements
      -- printable-ASCII ratio > 0.7, as integer arithmetic
      usable = not (T.null stripped) && 10 * T.length (T.filter (\c -> isPrint c && isAscii c) stripped) > 7 * T.length stripped
      fallback = case (serviceM, sampleM) of
        (Just svc, Just s) -> svc <> ": " <> T.take 80 s
        (_, Just s) -> s
        (Just svc, _) -> svc
        _ -> "log event"
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
      , issueType = LogPattern
      , targetHash = lp.patternHash
      , parentHash = Nothing
      , isFramework = False
      , service = lp.serviceName
      , critical = lvl == "error"
      , severity
      , title = "New Log Pattern: " <> sanitizeLogPatternTitle lp.logPattern lp.sampleMessage lp.serviceName
      , recommendedAction = "A new log pattern has been detected. Review to ensure it's expected behavior."
      , migrationComplexity = "n/a"
      , issueData =
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


data MkIssueOpts a = MkIssueOpts
  { projectId :: Projects.ProjectId
  , issueType :: IssueType
  , targetHash :: Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , service :: Maybe Text
  , critical :: Bool
  , severity :: IssueSeverity
  , title :: Text
  , recommendedAction :: Text
  , migrationComplexity :: Text
  , issueData :: a
  , timestamp :: Maybe ZonedTime
  }


mkIssue :: (AE.ToJSON a, Time :> es, UUIDEff :> es) => MkIssueOpts a -> Eff es Issue
mkIssue opts = do
  issueId <- UUIDId <$> genUUID
  zonedNow <- maybe (utcToZonedTime utc <$> Time.currentTime) pure opts.timestamp
  pure
    Issue
      { id = issueId
      , createdAt = zonedNow
      , updatedAt = zonedNow
      , projectId = opts.projectId
      , issueType = opts.issueType
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
      , issueData = Aeson $ AE.toJSON opts.issueData
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


logIssueActivity :: (DB es, Time :> es) => IssueId -> IssueEvent -> Maybe Projects.UserId -> Maybe AE.Value -> Eff es ()
logIssueActivity issueId event createdBy metadataM = do
  now <- Time.currentTime
  let metaJ = Aeson <$> metadataM
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.issue_activity_log (issue_id, event, created_by, metadata, created_at)
    SELECT #{issueId}, #{event}, #{createdBy}, #{metaJ}, #{now}
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
  , reportType :: Text
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
  , reportType :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ReportListItem)


addReport :: DB es => Report -> Eff es ()
addReport (r :: Report) =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.reports (id, created_at, updated_at, project_id, report_type, report_json, start_time, end_time)
      VALUES (#{r.id}, #{r.createdAt}, #{r.updatedAt}, #{r.projectId}, #{r.reportType}, #{r.reportJson}, #{r.startTime}, #{r.endTime}) |]


getReportById :: DB es => ReportId -> Eff es (Maybe Report)
getReportById rid = Hasql.interpOne (selectFrom @Report <> [HI.sql| WHERE id = #{rid} |])


reportHistoryByProject :: DB es => Projects.ProjectId -> Int -> Eff es [ReportListItem]
reportHistoryByProject pid page = do
  let off = page * 20
  Hasql.interp [HI.sql| SELECT id, created_at, project_id, report_type FROM apis.reports WHERE project_id = #{pid} ORDER BY created_at DESC LIMIT 20 OFFSET #{off} |]


getLatestReportByType :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Report)
getLatestReportByType pid rType = Hasql.interpOne (selectFrom @Report <> [HI.sql| WHERE project_id = #{pid} AND report_type = #{rType} ORDER BY created_at DESC LIMIT 1 |])


createErrorSpikeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> ErrorPatterns.ErrorPatternWithCurrentRate -> Double -> Double -> Double -> Eff es Issue
createErrorSpikeIssue projectId errRate currentRate baselineMean zScore =
  let increasePercent = if baselineMean > 0 then ((currentRate / baselineMean) - 1) * 100 else 0
   in mkErrorIssue
        projectId
        errRate
        (round currentRate)
        (const $ "Error Spike: " <> errRate.errorType <> " (" <> show (round increasePercent :: Int) <> "% increase)")
        ("Error rate has spiked " <> show (round zScore :: Int) <> " standard deviations above baseline. Current: " <> show (round currentRate :: Int) <> "/hr, Baseline: " <> show (round baselineMean :: Int) <> "/hr. Investigate recent deployments or changes.")


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
      , issueType = RuntimeException
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
      , issueData =
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
