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
  acknowledgeIssue,
  isInCooldown,
  setAckState,
  setArchiveState,
  autoArchiveStaleDiscoveryIssues,
  selectIssueByHash,
  selectLatestIssueByHash,
  reopenIssue,
  bumpIssueUpdatedAt,
  rateChangeCooldownHours,

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
import Data.Time (UTCTime)
import Data.Time.LocalTime (ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.UUID.V5 qualified as UUID5
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.Anomalies (PayloadChange)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), idToText, rawSql, selectFrom)
import Relude hiding (id)
import Servant (FromHttpApiData (..), ServerError, err500, errBody)
import System.Types (DB)
import Utils (formatUTC)


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
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField, ToSchema) via WrappedEnumSC "" IssueType


-- | Hash prefix used in otel_logs_and_spans hashes column
hashPrefix :: IssueType -> Maybe Text
hashPrefix LogPattern = Just "pat:"
hashPrefix LogPatternRateChange = Just "pat:"
hashPrefix RuntimeException = Just "err:"
hashPrefix ApiChange = Just "" -- endpoint hash is stored unprefixed on span hashes
hashPrefix _ = Nothing


defaultRecommendedAction :: Text
defaultRecommendedAction = "Review the changes and update your integration accordingly."


parseIssueType :: Text -> Maybe IssueType
parseIssueType = rightToMaybe . parseUrlPiece


data IssueSummary = IssueSummary
  { id :: IssueId
  , title :: Text
  , critical :: Bool
  , severity :: Text
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
isNewEndpointOnly issue
  | issue.issueType /= ApiChange = False
  | otherwise = case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: APIChangeData) -> V.null d.newFields && V.null d.deletedFields && V.null d.modifiedFields
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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] APIChangeData


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] RuntimeExceptionData


-- | Query Alert issue data
data QueryAlertData = QueryAlertData
  { queryId :: Text
  , queryName :: Text
  , queryExpression :: Text
  , thresholdValue :: Double
  , actualValue :: Double
  , thresholdType :: Text -- "above" or "below"
  , triggeredAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson QueryAlertData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryAlertData


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
  , severity :: Text -- "critical", "warning", "info"
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
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, HI.EncodeRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "issues", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Issue)


-- | Issue with aggregated event data (for list views).
-- Columns 1-28 must match Issue's field declaration order (Generic DecodeRow).
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
insertIssue (i :: Issue) = do
  -- DuplicateRecordFields: bind before TH quasi-quote which can't disambiguate
  let (iId, iCreated, iUpdated, iPid, iType, iTgt, iPHash, iFrame, iEHash) =
        (i.id, i.createdAt, i.updatedAt, i.projectId, i.issueType, i.targetHash, i.parentHash, i.isFramework, i.endpointHash)
      (iAckAt, iAckBy, iArchAt, iTitle, iSvc, iEnv, iCrit, iSev) =
        (i.acknowledgedAt, i.acknowledgedBy, i.archivedAt, i.title, i.service, i.environment, i.critical, i.severity)
      (iAction, iComplex, iData, iReqP, iResP, iLlmAt, iLlmVer, iSeq) =
        (i.recommendedAction, i.migrationComplexity, i.issueData, i.requestPayloads, i.responsePayloads, i.llmEnhancedAt, i.llmEnhancementVersion, i.seqNum)
      (iAffReq, iAffCli) = (max 1 i.affectedRequests, max 1 i.affectedClients)
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
) VALUES (#{iId}, #{iCreated}, #{iUpdated}, #{iPid}, #{iType}::apis.issue_type, #{iTgt}, #{iPHash}, #{iFrame}, #{iEHash},
  #{iAckAt}, #{iAckBy}, #{iArchAt}, #{iTitle}, #{iSvc}, #{iEnv}, #{iCrit}, #{iSev},
  #{iAction}, #{iComplex}, #{iData}, #{iReqP}, #{iResP}, #{iLlmAt}, #{iLlmVer}, #{iSeq},
  #{iAffReq}, #{iAffCli})
ON CONFLICT (project_id, target_hash, issue_type)
  WHERE acknowledged_at IS NULL AND archived_at IS NULL
DO UPDATE SET
  updated_at = EXCLUDED.updated_at,
  affected_requests = apis.issues.affected_requests + EXCLUDED.affected_requests,
  affected_clients = apis.issues.affected_clients + EXCLUDED.affected_clients,
  issue_data = EXCLUDED.issue_data
    || CASE WHEN jsonb_exists(apis.issues.issue_data, 'occurrence_count')
       THEN jsonb_build_object('occurrence_count', (apis.issues.issue_data->>'occurrence_count')::int + COALESCE((EXCLUDED.issue_data->>'occurrence_count')::int, 1))
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
selectIssueById iid = Hasql.interpOne [HI.sql| SELECT * FROM apis.issues WHERE id = #{iid} |]


selectIssueByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
selectIssueByHash pid tgtHash = Hasql.interpOne [HI.sql| SELECT * FROM apis.issues WHERE project_id = #{pid} AND target_hash = #{tgtHash} |]


-- | Find most recent RuntimeException issue for a given hash (including acknowledged/archived)
selectLatestIssueByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
selectLatestIssueByHash pid tgtHash =
  Hasql.interpOne [HI.sql| SELECT * FROM apis.issues WHERE project_id = #{pid} AND target_hash = #{tgtHash} AND issue_type = #{RuntimeException}::apis.issue_type ORDER BY created_at DESC LIMIT 1 |]


-- | Reopen a previously acknowledged/archived issue (clear ack/archive, bump occurrence count)
reopenIssue :: (DB es, Time :> es) => IssueId -> Eff es ()
reopenIssue issueId = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| UPDATE apis.issues SET
            acknowledged_at = NULL, acknowledged_by = NULL, archived_at = NULL, updated_at = #{now},
            issue_data = issue_data || jsonb_build_object('occurrence_count',
              COALESCE((issue_data->>'occurrence_count')::int, 1) + 1)
          WHERE id = #{issueId} |]


-- | Bump updated_at and occurrence count without clearing ack/archive (for already-open issues)
bumpIssueUpdatedAt :: (DB es, Time :> es) => IssueId -> Eff es ()
bumpIssueUpdatedAt issueId = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| UPDATE apis.issues SET updated_at = #{now},
            issue_data = issue_data || jsonb_build_object('occurrence_count',
              COALESCE((issue_data->>'occurrence_count')::int, 1) + 1)
          WHERE id = #{issueId} |]


-- | Select issues with filters, returns issues and total count for pagination
-- period: "24h" = 24 hourly buckets, "7d" = 7 daily buckets (default)
selectIssues :: DB es => Projects.ProjectId -> Maybe IssueType -> Maybe Bool -> Maybe Bool -> Int -> Int -> Maybe (UTCTime, UTCTime) -> Maybe Text -> Text -> [Text] -> [Text] -> Eff es ([IssueL], Int)
selectIssues pid _typeM isAcknowledged isArchived limit offset timeRangeM sortM period serviceFilters typeFilters = do
  issues <- Hasql.interp $ rawSql q <> [HI.sql| LIMIT #{limit} OFFSET #{offset}|]
  countResult <- fromMaybe 0 <$> Hasql.interpOne (rawSql countQ)
  pure (issues, countResult)
  where
    -- Inbox tab (unacked + unarchived) hides severity='low' so demoted silent
    -- drops don't clutter the incident view. Other tabs leave them visible.
    isInbox = isAcknowledged == Just False && isArchived == Just False
    mkFilters pfx =
      let timeF = maybe "" (\(st, end) -> " AND " <> pfx <> "created_at >= '" <> formatUTC st <> "' AND " <> pfx <> "created_at <= '" <> formatUTC end <> "'") timeRangeM
          ackF' = maybe "" (bool (" AND " <> pfx <> "acknowledged_at IS NULL") (" AND " <> pfx <> "acknowledged_at IS NOT NULL")) isAcknowledged
          archF' = maybe "" (bool (" AND " <> pfx <> "archived_at IS NULL") (" AND " <> pfx <> "archived_at IS NOT NULL")) isArchived
          sevF = if isInbox then " AND (" <> pfx <> "severity IS NULL OR " <> pfx <> "severity != 'low')" else ""
          sqlArr col vals = if null vals then "" else " AND " <> pfx <> col <> " = ANY(ARRAY[" <> T.intercalate "," (map (\v -> "'" <> T.replace "'" "''" v <> "'") vals) <> "]::text[])"
       in timeF <> ackF' <> archF' <> sevF <> sqlArr "service" serviceFilters <> sqlArr "issue_type::text" typeFilters
    timefilter = mkFilters "i."
    ackF = ""
    archF = ""
    svcF = ""
    typF = ""
    cTimefilter = mkFilters ""
    cAckF = ""
    cArchF = ""
    cSvcF = ""
    cTypF = ""
    orderBy = case sortM of
      Just "-created_at" -> "ORDER BY i.created_at DESC"
      Just "+created_at" -> "ORDER BY i.created_at ASC"
      Just "-updated_at" -> "ORDER BY i.updated_at DESC"
      Just "+updated_at" -> "ORDER BY i.updated_at ASC"
      Just "-title" -> "ORDER BY i.title DESC"
      Just "+title" -> "ORDER BY i.title ASC"
      _ -> "ORDER BY i.critical DESC, i.created_at DESC"
    -- period controls bucket granularity: "24h" = hourly, "7d" = daily
    (seriesStart, seriesStep, bucketSize) = case period of
      "24h" -> ("NOW() - INTERVAL '23 hours'" :: Text, "'1 hour'" :: Text, "INTERVAL '1 hour'" :: Text)
      _ -> ("CURRENT_DATE - INTERVAL '6 days'", "'1 day'", "INTERVAL '1 day'")
    pidTxt = pid.toText
    q =
      [text|
      SELECT i.id, i.created_at, i.updated_at, i.project_id, i.issue_type::text,
        i.endpoint_hash, i.acknowledged_at, i.acknowledged_by, i.archived_at, i.title, i.service, i.critical,
        -- Columns 1-28 must match Issue's field declaration order so that Generic
        -- FromRow/DecodeRow for Issue decodes the base row correctly.
        -- Prefer the stored severity (e.g. 'low' for silent drops); fall back to critical flag.
        COALESCE(NULLIF(i.severity, ''), CASE WHEN i.critical THEN 'critical' ELSE 'info' END),
        i.affected_requests, i.affected_clients, NULL::double precision,
        i.recommended_action, i.migration_complexity, i.issue_data, i.request_payloads, i.response_payloads,
        NULL::timestamp with time zone, NULL::int,
        i.target_hash, NULL::text, i.seq_num, i.parent_hash, i.is_framework,
        CASE
          WHEN i.issue_type = 'runtime_exception' THEN COALESCE(err_ev.cnt, 0)
          WHEN i.issue_type IN ('log_pattern', 'log_pattern_rate_change') THEN COALESCE(lp_ev.cnt, 0)
          ELSE i.affected_requests
        END::bigint,
        i.updated_at, lat.event,
        CASE
          WHEN i.issue_type = 'runtime_exception' THEN COALESCE(err_ev.buckets, '{}'::int[])
          WHEN i.issue_type IN ('log_pattern', 'log_pattern_rate_change') THEN COALESCE(lp_ev.buckets, '{}'::int[])
          ELSE '{}'::int[]
        END
      FROM apis.issues i
      LEFT JOIN LATERAL (
        SELECT SUM(day_cnt)::bigint AS cnt, array_agg(day_cnt ORDER BY day) AS buckets FROM (
          SELECT d AS day, COALESCE(SUM(ehs.event_count), 0)::int AS day_cnt
          FROM generate_series($seriesStart, NOW(), $seriesStep) d
          LEFT JOIN (apis.error_hourly_stats ehs
            JOIN apis.error_patterns ep ON ep.id = ehs.error_id AND ep.project_id = ehs.project_id
              AND ep.project_id = i.project_id
              AND ((i.is_framework AND ep.parent_hash = i.target_hash)
                OR (NOT i.is_framework AND ep.hash = i.target_hash))
          ) ON ehs.hour_bucket >= d AND ehs.hour_bucket < d + $bucketSize
          GROUP BY d
        ) sub
      ) err_ev ON i.issue_type = 'runtime_exception'
      LEFT JOIN LATERAL (
        SELECT SUM(day_cnt)::bigint AS cnt, array_agg(day_cnt ORDER BY day) AS buckets FROM (
          SELECT d AS day, COALESCE(SUM(lhs.event_count), 0)::int AS day_cnt
          FROM generate_series($seriesStart, NOW(), $seriesStep) d
          LEFT JOIN apis.log_pattern_hourly_stats lhs
            ON lhs.pattern_hash = i.target_hash AND lhs.project_id = i.project_id
            AND lhs.hour_bucket >= d AND lhs.hour_bucket < d + $bucketSize
          GROUP BY d
        ) sub
      ) lp_ev ON i.issue_type IN ('log_pattern', 'log_pattern_rate_change')
      LEFT JOIN LATERAL (
        SELECT a.event FROM apis.issue_activity_log a
        WHERE a.issue_id = i.id AND a.event IN ('resolved', 'auto_resolved', 'reopened', 'regressed', 'escalated')
        ORDER BY a.created_at DESC LIMIT 1
      ) lat ON TRUE
      WHERE i.project_id = |]
        <> "'"
        <> pidTxt
        <> "'::uuid "
        <> timefilter
        <> " "
        <> ackF
        <> " "
        <> archF
        <> " "
        <> svcF
        <> " "
        <> typF
        <> " "
        <> orderBy
    countQ = "SELECT COUNT(*)::INT FROM apis.issues WHERE project_id = '" <> pidTxt <> "'::uuid " <> cTimefilter <> " " <> cAckF <> " " <> cArchF <> " " <> cSvcF <> " " <> cTypF


-- | Find open issue for endpoint
findOpenIssueForEndpoint :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
findOpenIssueForEndpoint pid tgtHash =
  Hasql.interpOne [HI.sql| SELECT * FROM apis.issues WHERE project_id = #{pid} AND issue_type = #{ApiChange}::apis.issue_type AND target_hash = #{tgtHash} AND acknowledged_at IS NULL AND archived_at IS NULL LIMIT 1 |]


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
updateIssueCriticality :: DB es => IssueId -> Bool -> Text -> Eff es ()
updateIssueCriticality issueId isCritical sev =
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.issues SET critical = #{isCritical}, severity = #{sev} WHERE id = #{issueId} |]


-- How long an acknowledgment suppresses re-firing for rate-change issues.
-- Kept short so genuinely persistent regressions still resurface, but long
-- enough that a ack during an incident is not immediately undone.
rateChangeCooldownHours :: Int
rateChangeCooldownHours = 24


-- | Acknowledge issue. For rate-change issues the ack also opens a 24h cooldown
-- that the detector consults before firing again for the same (project,
-- target_hash) — prevents the "ack'd issue re-fires on the next detection run"
-- pattern that was making the Inbox unusable.
acknowledgeIssue :: (DB es, Time :> es) => IssueId -> Projects.UserId -> Eff es ()
acknowledgeIssue issueId userId = do
  now <- Time.currentTime
  let hrs = rateChangeCooldownHours
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.issues SET
        acknowledged_at = #{now},
        acknowledged_by = #{userId},
        cooldown_until = CASE WHEN issue_type = 'log_pattern_rate_change'
                              THEN #{now}::timestamptz + (INTERVAL '1 hour' * #{hrs})
                              ELSE cooldown_until END
      WHERE id = #{issueId} |]


-- | Set ack/unack on a batch of issues. Pass @Just now@ to ack (records @acknowledged_by@);
-- pass @Nothing@ to unack (clears both timestamp and actor). Rate-change acks
-- also set a 24h cooldown; unacks clear it.
setAckState :: DB es => Projects.ProjectId -> V.Vector IssueId -> Maybe UTCTime -> Maybe Projects.UserId -> Eff es Int64
setAckState pid iids mTs mUid
  | V.null iids = pure 0
  | otherwise =
      let hrs = rateChangeCooldownHours
       in Hasql.interpExecute
            [HI.sql|
              UPDATE apis.issues
              SET acknowledged_at = #{mTs},
                  acknowledged_by = #{mUid},
                  updated_at = COALESCE(#{mTs}, updated_at),
                  cooldown_until = CASE
                    WHEN #{mTs}::timestamptz IS NULL THEN NULL
                    WHEN issue_type = 'log_pattern_rate_change'
                      THEN #{mTs}::timestamptz + (INTERVAL '1 hour' * #{hrs})
                    ELSE cooldown_until END
              WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]


-- | True if an acknowledged issue for this (project, target, type) is still
-- within its cooldown window. The detector uses this to suppress re-firing.
isInCooldown :: DB es => Projects.ProjectId -> Text -> IssueType -> UTCTime -> Eff es Bool
isInCooldown pid tgt ty now =
  isJust <$> (Hasql.interpOne q :: DB es => Eff es (Maybe Int64))
  where
    q =
      [HI.sql|
        SELECT 1 FROM apis.issues
        WHERE project_id = #{pid}
          AND target_hash = #{tgt}
          AND issue_type = #{ty}::apis.issue_type
          AND cooldown_until IS NOT NULL
          AND cooldown_until > #{now}
        LIMIT 1 |]


-- | Set archive state on a batch of issues. @Just now@ archives, @Nothing@ unarchives.
setArchiveState :: DB es => Projects.ProjectId -> V.Vector IssueId -> Maybe UTCTime -> Eff es Int64
setArchiveState pid iids mTs
  | V.null iids = pure 0
  | otherwise =
      Hasql.interpExecute
        [HI.sql|
          UPDATE apis.issues
          SET archived_at = #{mTs}, updated_at = COALESCE(#{mTs}, updated_at)
          WHERE project_id = #{pid} AND id = ANY(#{iids}::uuid[]) |]


-- | Archive open @log_pattern@ / @log_pattern_rate_change@ issues whose
-- @updated_at@ is older than @days@. @insertIssue@ bumps @updated_at@ on
-- conflict, so an actively-firing pattern never ages out — only dead signal
-- does. The predicate runs against OLD @updated_at@ before the
-- @set_updated_at@ trigger fires, so the cutoff is evaluated on the
-- pre-archive value.
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
  let anyAck = isNothing isAck
      ackFlag = fromMaybe False isAck
      anyArch = isNothing isArch
      archFlag = fromMaybe False isArch
      anyType = maybe True T.null tyM
      ty = fromMaybe "" tyM
      anySvc = maybe True T.null svcM
      svc = fromMaybe "" svcM
      whereSql =
        [HI.sql|
          WHERE project_id = #{pid}
            AND (#{anyAck} OR (#{ackFlag} AND acknowledged_at IS NOT NULL) OR (NOT #{ackFlag} AND acknowledged_at IS NULL))
            AND (#{anyArch} OR (#{archFlag} AND archived_at IS NOT NULL) OR (NOT #{archFlag} AND archived_at IS NULL))
            AND (#{anyType} OR issue_type::text = #{ty})
            AND (#{anySvc} OR service = #{svc})
        |]
  rows <- Hasql.interp (selectFrom @Issue <> whereSql <> [HI.sql| ORDER BY updated_at DESC LIMIT #{limit} OFFSET #{offset} |])
  total <- fromMaybe 0 <$> Hasql.interpOne ([HI.sql| SELECT COUNT(*)::int FROM apis.issues |] <> whereSql)
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
      , severity = if isCritical then "critical" else "warning"
      , title =
          if V.any ((== Anomalies.ATEndpoint) . (.anomalyType)) anomalies
            then "New endpoint detected: " <> apiChangeData.endpointMethod <> " " <> apiChangeData.endpointPath <> " on " <> apiChangeData.endpointHost
            else "API structure has changed"
      , recommendedAction = "Review the API changes and update your integration accordingly."
      , migrationComplexity = if breakingChanges > 5 then "high" else if breakingChanges > 0 then "medium" else "low"
      , issueData = apiChangeData
      , timestamp = Just firstAnomaly.createdAt
      }


-- | Create Query Alert issue
createQueryAlertIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> Text -> Text -> Double -> Double -> Text -> Eff es Issue
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
      , severity = "warning"
      , title = queryName <> " threshold " <> thresholdType <> " " <> show threshold
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
  deriving (Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "CT" ConversationType


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


-- | AI Chat message
data AIChatMessage = AIChatMessage
  { id :: UUIDId "ai_chat"
  , projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"
  , role :: Text -- "user", "assistant", or "system"
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
  result <-
    Hasql.interp
      [HI.sql| INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, context)
              VALUES (#{pid}, #{convId}, #{convType}, #{ctxJ}) ON CONFLICT (project_id, conversation_id) DO UPDATE SET updated_at = #{now}
              RETURNING id, project_id, conversation_id, conversation_type, context, created_at, updated_at |]
  maybe (throwError err500{errBody = "getOrCreateConversation: RETURNING clause must return a row"}) pure $ listToMaybe result


-- | Insert a new chat message
insertChatMessage :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Text -> Text -> Maybe AE.Value -> Maybe AE.Value -> Eff es ()
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


-- | Try to acquire an advisory lock for chat migration to prevent race conditions.
-- Returns True if lock was acquired, False if already locked (another request is migrating).
-- Uses PostgreSQL advisory locks which are automatically released on connection close.
chatMigrationLockKey :: UUIDId "conversation" -> Int64
chatMigrationLockKey convId = fromIntegral @Int @Int64 $ abs $ hash $ show convId.unwrap


tryAcquireChatMigrationLock :: DB es => UUIDId "conversation" -> Eff es Bool
tryAcquireChatMigrationLock convId = do
  let lockKey = chatMigrationLockKey convId
  result :: [Bool] <- Hasql.interp [HI.sql| SELECT pg_try_advisory_lock(#{lockKey}) |]
  pure $ fromMaybe False $ viaNonEmpty head result


-- | Release a chat migration advisory lock so that a failed migration can be retried
-- on a subsequent event instead of being silently blocked for the connection's lifetime.
releaseChatMigrationLock :: DB es => UUIDId "conversation" -> Eff es ()
releaseChatMigrationLock convId = do
  let lockKey = chatMigrationLockKey convId
  Hasql.interpExecute_ [HI.sql| SELECT pg_advisory_unlock(#{lockKey}) |]


-- | Create an issue for a log pattern rate change
createLogPatternRateChangeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> LogPatterns.LogPatternWithRate -> SpikeResult -> Eff es Issue
createLogPatternRateChangeIssue projectId lp sr = do
  now <- Time.currentTime
  let changePercentVal = if sr.mean > 1 then min 9999 $ abs ((sr.currentRate / sr.mean) - 1) * 100 else 0
      dir = display sr.direction
      zonedNow = utcToZonedTime utc now
      lvl = T.toLower $ fromMaybe "" lp.logLevel
      -- Silent drops on unknown/empty services are almost always deploy/pod-restart
      -- noise, not incidents. Demote them so the Inbox filter hides them by default.
      svcLabel = fromMaybe "unknown" lp.serviceName
      silentDrop = sr.direction == Drop && sr.currentRate == 0 && svcLabel `elem` ["", "unknown"]
      severity
        | silentDrop = "low"
        | otherwise = case (sr.direction, lvl) of
            (Spike, "error") -> "critical"
            (Spike, _) -> "warning"
            (Drop, _) -> "info"
      patternSnippet = T.take 40 lp.logPattern
      title =
        svcLabel
          <> " · "
          <> patternSnippet
          <> " · "
          <> dir
          <> " "
          <> showPct changePercentVal
          <> " ("
          <> showRate sr.currentRate
          <> "/hr vs "
          <> showRate sr.mean
          <> "/hr)"
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
      , timestamp = Just zonedNow
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
  let replacements :: [(Text, Text)]
      replacements =
        [ (";neutral⇒", " ")
        , (";badge-error⇒", " ")
        , (";badge-warning⇒", " ")
        , (";badge-info⇒", " ")
        , (";badge-success⇒", " ")
        , ("{integer}", "")
        , ("{uuid}", "")
        , ("{float}", "")
        , ("{*}", "")
        , ("{hex}", "")
        ]
      stripped = unwords $ words $ foldl' (\t (a, b) -> T.replace a b t) raw replacements
      printableRatio txt
        | T.null txt = 0
        | otherwise = fromIntegral (T.length (T.filter (\c -> isPrint c && isAscii c) txt)) / fromIntegral (T.length txt) :: Double
      usable = not (T.null stripped) && printableRatio stripped > 0.7
      fallback = case (serviceM, sampleM) of
        (Just svc, Just s) -> svc <> ": " <> T.take 80 s
        (_, Just s) -> s
        (Just svc, _) -> svc
        _ -> "log event"
   in T.take 100 $ if usable then stripped else fallback


-- | Create an issue for a new log pattern
createLogPatternIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> LogPatterns.LogPattern -> Eff es Issue
createLogPatternIssue projectId lp = do
  let logPatternData =
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
      lvl = T.toLower $ fromMaybe "" lp.logLevel
      severity = case lvl of
        "error" -> "critical"
        "warning" -> "warning"
        "warn" -> "warning"
        _ -> "info"
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
      , issueData = logPatternData
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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LogPatternData


data RateChangeDirection = Spike | Drop
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display) via WrappedEnumSC "" RateChangeDirection


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LogPatternRateChangeData


data MkIssueOpts a = MkIssueOpts
  { projectId :: Projects.ProjectId
  , issueType :: IssueType
  , targetHash :: Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , service :: Maybe Text
  , critical :: Bool
  , severity :: Text
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
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField, ToSchema) via WrappedEnumSC "IE" IssueEvent


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
  deriving anyclass (FromRow, HI.DecodeRow, HI.EncodeRow, NFData, ToRow)
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
addReport (r :: Report) = do
  let (rId, rCreated, rUpdated, rPid, rType, rJson, rStart, rEnd) =
        (r.id, r.createdAt, r.updatedAt, r.projectId, r.reportType, r.reportJson, r.startTime, r.endTime)
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.reports (id, created_at, updated_at, project_id, report_type, report_json, start_time, end_time)
      VALUES (#{rId}, #{rCreated}, #{rUpdated}, #{rPid}, #{rType}, #{rJson}, #{rStart}, #{rEnd}) |]


getReportById :: DB es => ReportId -> Eff es (Maybe Report)
getReportById rid = Hasql.interpOne [HI.sql| SELECT * FROM apis.reports WHERE id = #{rid} |]


reportHistoryByProject :: DB es => Projects.ProjectId -> Int -> Eff es [ReportListItem]
reportHistoryByProject pid page = do
  let off = page * 20
  Hasql.interp [HI.sql| SELECT id, created_at, project_id, report_type FROM apis.reports WHERE project_id = #{pid} ORDER BY created_at DESC LIMIT 20 OFFSET #{off} |]


getLatestReportByType :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Report)
getLatestReportByType pid rType = Hasql.interpOne [HI.sql| SELECT * FROM apis.reports WHERE project_id = #{pid} AND report_type = #{rType} ORDER BY created_at DESC LIMIT 1 |]


createErrorSpikeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> ErrorPatterns.ErrorPatternWithCurrentRate -> Double -> Double -> Double -> Eff es Issue
createErrorSpikeIssue projectId errRate currentRate baselineMean zScore =
  let increasePercent = if baselineMean > 0 then ((currentRate / baselineMean) - 1) * 100 else 0
      -- Inherit classification from the underlying pattern so a spike on a framework
      -- error dedupes against the rolled-up framework issue via the same target_hash.
      useFramework = errRate.isFramework && isJust errRate.parentHash
      tgt = if useFramework then fromMaybe errRate.hash errRate.parentHash else errRate.hash
   in mkErrorIssue
        projectId
        tgt
        errRate.parentHash
        useFramework
        errRate.service
        errRate.errorType
        errRate.message
        errRate.stacktrace
        (round currentRate)
        ("Error Spike: " <> errRate.errorType <> " (" <> show (round increasePercent :: Int) <> "% increase)")
        ("Error rate has spiked " <> show (round zScore :: Int) <> " standard deviations above baseline. Current: " <> show (round currentRate :: Int) <> "/hr, Baseline: " <> show (round baselineMean :: Int) <> "/hr. Investigate recent deployments or changes.")


-- | Create a new issue for an error pattern. Framework/transport errors are keyed on
-- the *parent* (broad) hash so that subsequent per-route variants hit the ON CONFLICT
-- path and do not fragment into many issues. App errors keep per-route identity and
-- store the parent hash for UI rollup.
createNewErrorIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> ErrorPatterns.ErrorPattern -> Eff es Issue
createNewErrorIssue projectId err =
  -- Framework errors key the issue on parentHash so per-route variants collapse into one
  -- issue via the (project_id, target_hash, issue_type) ON CONFLICT index. If the broad
  -- hash is somehow missing we fall back to the narrow hash (behaves like an app error).
  let useFramework = err.isFramework && isJust err.parentHash
      tgt = if useFramework then fromMaybe err.hash err.parentHash else err.hash
      title =
        (if useFramework then "Framework Error: " else "New Error: ")
          <> err.errorType
          <> " - "
          <> T.take 80 err.message
   in mkErrorIssue
        projectId
        tgt
        err.parentHash
        useFramework
        err.service
        err.errorType
        err.message
        err.stacktrace
        1
        title
        "Investigate the new error and implement a fix."


mkErrorIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> Maybe Text -> Bool -> Maybe Text -> Text -> Text -> Text -> Int -> Text -> Text -> Eff es Issue
mkErrorIssue projectId targetHash parentHash isFramework service errType errMsg stack occurrences title recommendedAction = do
  now <- Time.currentTime
  mkIssue
    MkIssueOpts
      { projectId
      , issueType = RuntimeException
      , targetHash
      , parentHash
      , isFramework
      , service
      , critical = True
      , severity = "critical"
      , title
      , recommendedAction
      , migrationComplexity = "n/a"
      , timestamp = Nothing
      , issueData =
          RuntimeExceptionData
            { errorType = errType
            , errorMessage = errMsg
            , stackTrace = stack
            , requestPath = Nothing
            , requestMethod = Nothing
            , occurrenceCount = occurrences
            , firstSeen = now
            , lastSeen = now
            }
      }
