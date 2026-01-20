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
  LogPatternData (..),
  ErrorEscalatingData (..),
  ErrorRegressedData (..),
  LogPatternRateChangeData (..),
  EndpointLatencyDegradationData (..),
  EndpointErrorRateSpikeData (..),
  EndpointVolumeRateChangeData (..),
  NewEndpointData (..),
  NewShapeData (..),
  FieldChangeData (..),

  -- * Database Operations
  insertIssue,
  selectIssueById,
  selectIssues,
  findOpenIssueForEndpoint,
  updateIssueWithNewAnomaly,
  updateIssueEnhancement,
  updateIssueCriticality,
  acknowledgeIssue,
  selectIssueByHash,
  updateIssueData,

  -- * Conversion Functions
  createQueryAlertIssue,
  createNewErrorIssue,
  createErrorSpikeIssue,
  createErrorEscalatingIssue,
  createErrorRegressedIssue,
  createEndpointLatencyDegradationIssue,
  createEndpointErrorRateSpikeIssue,
  createEndpointVolumeRateChangeIssue,
  createNewEndpointIssue,
  createNewShapeIssue,
  createFieldChangeIssue,
  findOpenShapeIssueForStatusCode,

  -- * Utilities
  issueIdText,
  parseIssueType,
  issueTypeToText,

  -- * AI Conversations
  AIConversation (..),
  AIChatMessage (..),
  ConversationType (..),
  getOrCreateConversation,
  insertChatMessage,
  selectChatHistory,

  -- * Thread ID Helpers
  slackThreadToConversationId,
  discordThreadToConversationId,
) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Default (Default, def)
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime)
import Data.UUID.V4 qualified as UUID4
import Data.UUID.V5 qualified as UUID5
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_selectWhere)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.Anomalies (PayloadChange)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Errors qualified as Errors

-- import Models.Apis.LogPatterns qualified as LogPatterns  -- Removed for errors-feature branch
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pkg.DBUtils (WrappedEnumSC (..))
import Pkg.DeriveUtils (UUIDId (..), idToText)
import Relude hiding (id)
import System.Types (DB)
import Utils (formatUTC)


type IssueId = UUIDId "issue"


issueIdText :: IssueId -> Text
issueIdText = idToText


-- | Issue types
data IssueType
  = NewEndpoint
  | NewShape
  | FieldChange
  | RuntimeException
  | QueryAlert
  | LogPattern
  | ErrorEscalating
  | ErrorRegressed
  | LogPatternRateChange
  | EndpointLatencyDegradation
  | EndpointErrorRateSpike
  | EndpointVolumeRateChange
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.CamelToSnake]] IssueType


issueTypeToText :: IssueType -> Text
issueTypeToText NewEndpoint = "new_endpoint"
issueTypeToText NewShape = "new_shape"
issueTypeToText FieldChange = "field_change"
issueTypeToText RuntimeException = "runtime_exception"
issueTypeToText QueryAlert = "query_alert"
issueTypeToText LogPattern = "log_pattern"
issueTypeToText ErrorEscalating = "error_escalating"
issueTypeToText ErrorRegressed = "error_regressed"
issueTypeToText LogPatternRateChange = "log_pattern_rate_change"
issueTypeToText EndpointLatencyDegradation = "endpoint_latency_degradation"
issueTypeToText EndpointErrorRateSpike = "endpoint_error_rate_spike"
issueTypeToText EndpointVolumeRateChange = "endpoint_volume_rate_change"


parseIssueType :: Text -> Maybe IssueType
parseIssueType "new_endpoint" = Just NewEndpoint
parseIssueType "new_shape" = Just NewShape
parseIssueType "field_change" = Just FieldChange
parseIssueType "shape" = Just NewShape -- Handle DB anomaly_type
parseIssueType "endpoint" = Just NewEndpoint -- Handle DB anomaly_type
parseIssueType "field" = Just FieldChange -- Handle DB anomaly_type
parseIssueType "runtime_exception" = Just RuntimeException
parseIssueType "query_alert" = Just QueryAlert
parseIssueType "log_pattern" = Just LogPattern
parseIssueType "error_escalating" = Just ErrorEscalating
parseIssueType "error_regressed" = Just ErrorRegressed
parseIssueType "log_pattern_rate_change" = Just LogPatternRateChange
parseIssueType "endpoint_latency_degradation" = Just EndpointLatencyDegradation
parseIssueType "endpoint_error_rate_spike" = Just EndpointErrorRateSpike
parseIssueType "endpoint_volume_rate_change" = Just EndpointVolumeRateChange
parseIssueType _ = Nothing


instance ToField IssueType where
  toField = Escape . encodeUtf8 . issueTypeToText


instance FromField IssueType where
  fromField f mdata = case mdata of
    Nothing -> returnError UnexpectedNull f ""
    Just bs -> case parseIssueType (decodeUtf8 bs) of
      Just t -> pure t
      Nothing -> returnError ConversionFailed f $ "Unknown issue type: " <> decodeUtf8 bs


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


-- | Log Pattern issue data (new pattern detected)
data LogPatternData = LogPatternData
  { patternHash :: Text
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , firstSeenAt :: UTCTime
  , occurrenceCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson LogPatternData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LogPatternData


-- | Error Escalating issue data (error rate increasing over time)
data ErrorEscalatingData = ErrorEscalatingData
  { errorHash :: Text
  , errorType :: Text
  , errorMessage :: Text
  , serviceName :: Maybe Text
  , currentState :: Text -- "escalating"
  , previousState :: Text -- "new" or "ongoing"
  , occurrences1h :: Int
  , occurrences24h :: Int
  , escalationRate :: Double -- rate of increase (e.g., 2.5x)
  , escalationWindow :: Text -- "1h", "6h", "24h"
  , firstSeenAt :: UTCTime
  , lastSeenAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson ErrorEscalatingData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ErrorEscalatingData


-- | Error Regressed issue data (previously resolved error returned)
data ErrorRegressedData = ErrorRegressedData
  { errorHash :: Text
  , errorType :: Text
  , errorMessage :: Text
  , serviceName :: Maybe Text
  , resolvedAt :: UTCTime -- when it was previously resolved
  , regressedAt :: UTCTime -- when it came back
  , quietPeriodMinutes :: Int -- how long it was quiet
  , previousOccurrences :: Int -- count before resolution
  , newOccurrences :: Int -- count since regression
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson ErrorRegressedData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ErrorRegressedData


-- | Log Pattern Rate Change issue data (volume spike/drop)
data LogPatternRateChangeData = LogPatternRateChangeData
  { patternHash :: Text
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , currentRatePerHour :: Double
  , baselineMean :: Double
  , baselineStddev :: Double
  , zScore :: Double -- standard deviations from baseline
  , changePercent :: Double -- percentage change from baseline
  , changeDirection :: Text -- "spike" or "drop"
  , detectedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson LogPatternRateChangeData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LogPatternRateChangeData


-- | Endpoint Latency Degradation issue data
data EndpointLatencyDegradationData = EndpointLatencyDegradationData
  { endpointHash :: Text
  , endpointMethod :: Text
  , endpointPath :: Text
  , serviceName :: Maybe Text
  , currentLatencyMs :: Double -- current p50/p95/p99
  , baselineLatencyMs :: Double -- baseline p50/p95/p99
  , baselineStddev :: Double
  , zScore :: Double
  , degradationPercent :: Double -- percentage increase
  , percentile :: Text -- "p50", "p95", "p99"
  , sampleTraceIds :: V.Vector Text -- example traces for investigation
  , detectedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson EndpointLatencyDegradationData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] EndpointLatencyDegradationData


-- | Endpoint Error Rate Spike issue data
data EndpointErrorRateSpikeData = EndpointErrorRateSpikeData
  { endpointHash :: Text
  , endpointMethod :: Text
  , endpointPath :: Text
  , serviceName :: Maybe Text
  , currentErrorRate :: Double -- current error rate (0.0 - 1.0)
  , baselineErrorRate :: Double -- baseline error rate
  , baselineStddev :: Double
  , zScore :: Double
  , spikePercent :: Double -- percentage increase
  , errorCount :: Int -- number of errors in window
  , totalRequests :: Int -- total requests in window
  , topErrorTypes :: V.Vector Text -- most common error types
  , detectedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson EndpointErrorRateSpikeData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] EndpointErrorRateSpikeData


-- | Endpoint Volume Rate Change issue data (traffic spike or drop)
data EndpointVolumeRateChangeData = EndpointVolumeRateChangeData
  { endpointHash :: Text
  , endpointMethod :: Text
  , endpointPath :: Text
  , serviceName :: Maybe Text
  , currentRatePerHour :: Double -- current requests/hour
  , baselineRatePerHour :: Double -- baseline requests/hour
  , baselineStddev :: Double
  , zScore :: Double
  , changePercent :: Double -- percentage change
  , changeDirection :: Text -- "spike" or "drop"
  , detectedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson EndpointVolumeRateChangeData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] EndpointVolumeRateChangeData


-- | New Endpoint issue data
data NewEndpointData = NewEndpointData
  { endpointHash :: Text
  , endpointMethod :: Text
  , endpointPath :: Text
  , endpointHost :: Text
  , firstSeenAt :: UTCTime
  , initialShapes :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson NewEndpointData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewEndpointData


-- | New Shape issue data
data NewShapeData = NewShapeData
  { shapeHash :: Text
  , endpointHash :: Text
  , endpointMethod :: Text
  , endpointPath :: Text
  , statusCode :: Int
  , exampleRequestPayload :: AE.Value
  , exampleResponsePayload :: AE.Value
  , newFields :: V.Vector Text
  , deletedFields :: V.Vector Text
  , modifiedFields :: V.Vector Text
  , fieldHashes :: V.Vector Text
  , firstSeenAt :: UTCTime
  , newShapesAfterIssue :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson NewShapeData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewShapeData


-- | Field Change issue data (for individual field changes)
data FieldChangeData = FieldChangeData
  { fieldHash :: Text
  , endpointHash :: Text
  , endpointMethod :: Text
  , endpointPath :: Text
  , keyPath :: Text
  , fieldCategory :: Text
  , previousType :: Maybe Text
  , newType :: Text
  , changeType :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson FieldChangeData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FieldChangeData


-- | Main Issue type
data Issue = Issue
  { id :: IssueId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , issueType :: IssueType
  , sourceType :: Text
  , targetHash :: Text
  , endpointHash :: Text
  , acknowledgedAt :: Maybe ZonedTime
  , acknowledgedBy :: Maybe Users.UserId
  , archivedAt :: Maybe ZonedTime
  , title :: Text
  , service :: Maybe Text
  , environment :: Maybe Text
  , critical :: Bool
  , severity :: Text -- "critical", "warning", "info"
  , recommendedAction :: Text
  , migrationComplexity :: Text -- "low", "medium", "high", "n/a"
  , issueData :: Aeson AE.Value
  , requestPayloads :: Aeson [PayloadChange]
  , responsePayloads :: Aeson [PayloadChange]
  , llmEnhancedAt :: Maybe UTCTime
  , llmEnhancementVersion :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "issues", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Issue)


instance Default Issue where
  def =
    Issue
      { id = def
      , createdAt = error "createdAt must be set"
      , updatedAt = error "updatedAt must be set"
      , projectId = def
      , issueType = error "issueType must be set"
      , sourceType = ""
      , targetHash = ""
      , endpointHash = ""
      , acknowledgedAt = Nothing
      , acknowledgedBy = Nothing
      , archivedAt = Nothing
      , title = ""
      , service = Nothing
      , environment = Nothing
      , critical = False
      , severity = "info"
      , recommendedAction = ""
      , migrationComplexity = "low"
      , issueData = Aeson AE.Null
      , requestPayloads = Aeson []
      , responsePayloads = Aeson []
      , llmEnhancedAt = Nothing
      , llmEnhancementVersion = Nothing
      }


-- | Issue with aggregated event data (for list views)
data IssueL = IssueL
  { id :: IssueId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , issueType :: IssueType -- Will be converted from anomaly_type in query
  , endpointHash :: Text
  , acknowledgedAt :: Maybe ZonedTime
  , acknowledgedBy :: Maybe Users.UserId
  , archivedAt :: Maybe ZonedTime
  , title :: Text
  , service :: Maybe Text
  , critical :: Bool
  , severity :: Text -- Computed in query
  , affectedRequests :: Int -- Will be converted from affected_payloads in query
  , affectedClients :: Int
  , errorRate :: Maybe Double -- Not in DB, will be NULL
  , recommendedAction :: Text
  , migrationComplexity :: Text
  , issueData :: Aeson AE.Value
  , -- Payload changes tracking (for API changes)
    requestPayloads :: Aeson [PayloadChange]
  , responsePayloads :: Aeson [PayloadChange]
  , llmEnhancedAt :: Maybe UTCTime -- Not in DB, will be NULL
  , llmEnhancementVersion :: Maybe Int -- Not in DB, will be NULL
  -- Aggregated data
  , eventCount :: Int
  , lastSeen :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData)


-- | Insert a single issue
-- Note: ON CONFLICT only applies to api_change issues that are open (not acknowledged/archived)
-- Other issue types will fail on duplicate inserts as intended
insertIssue :: DB es => Issue -> Eff es ()
insertIssue issue = void $ PG.execute q issue
  where
    q =
      [sql|
INSERT INTO apis.issues (
  id, created_at, updated_at, project_id, issue_type, source_type, target_hash, endpoint_hash,
  acknowledged_at, acknowledged_by, archived_at,
  title, service, environment, critical, severity,
  recommended_action, migration_complexity,
  issue_data, request_payloads, response_payloads,
  llm_enhanced_at, llm_enhancement_version
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT (project_id, target_hash, issue_type)
  WHERE acknowledged_at IS NULL AND archived_at IS NULL
DO UPDATE SET
  updated_at = EXCLUDED.updated_at,
  issue_data = EXCLUDED.issue_data
    |]


-- | Select issue by ID
selectIssueById :: DB es => IssueId -> Eff es (Maybe Issue)
selectIssueById iid = listToMaybe <$> PG.query (_selectWhere @Issue [[field| id |]]) (Only iid)


selectIssueByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
selectIssueByHash pid hash = listToMaybe <$> PG.query (_selectWhere @Issue [[field| project_id |], [field| endpoint_hash |]]) (pid, hash)


-- | Select issues with filters, returns issues and total count for pagination
selectIssues :: DB es => Projects.ProjectId -> Maybe IssueType -> Maybe Bool -> Maybe Bool -> Int -> Int -> Maybe (UTCTime, UTCTime) -> Maybe Text -> Eff es ([IssueL], Int)
selectIssues pid _typeM isAcknowledged isArchived limit offset timeRangeM sortM = do
  issues <- PG.query (Query $ encodeUtf8 q) (pid, limit, offset)
  countResult <- coerce @(Maybe (Only Int)) @(Maybe Int) . listToMaybe <$> PG.query (Query $ encodeUtf8 countQ) (Only pid)
  pure (issues, fromMaybe 0 countResult)
  where
    timefilter = maybe "" (\(st, end) -> " AND created_at >= '" <> formatUTC st <> "' AND created_at <= '" <> formatUTC end <> "'") timeRangeM
    ackF = maybe "" (\ack -> if ack then " AND acknowledged_at IS NOT NULL" else " AND acknowledged_at IS NULL") isAcknowledged
    archF = maybe "" (\arch -> if arch then " AND archived_at IS NOT NULL" else " AND archived_at IS NULL") isArchived
    orderBy = case sortM of
      Just "-created_at" -> "ORDER BY created_at DESC"
      Just "+created_at" -> "ORDER BY created_at ASC"
      Just "-updated_at" -> "ORDER BY updated_at DESC"
      Just "+updated_at" -> "ORDER BY updated_at ASC"
      Just "-title" -> "ORDER BY title DESC"
      Just "+title" -> "ORDER BY title ASC"
      _ -> "ORDER BY critical DESC, created_at DESC"
    q =
      [text|
      SELECT id, created_at, updated_at, project_id, issue_type::text, endpoint_hash, acknowledged_at, acknowledged_by, archived_at, title, service, critical,
        CASE WHEN critical THEN 'critical' ELSE 'info' END, 0::int, 0::int, NULL::double precision,
        recommended_action, migration_complexity, issue_data, request_payloads, response_payloads, NULL::timestamp with time zone, NULL::int, 0::bigint, updated_at
      FROM apis.issues WHERE project_id = ? $timefilter $ackF $archF $orderBy LIMIT ? OFFSET ?
    |]
    countQ = [text|SELECT COUNT(*) FROM apis.issues WHERE project_id = ? $timefilter $ackF $archF|]


-- | Find open issue for endpoint
findOpenIssueForEndpoint :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
findOpenIssueForEndpoint pid endpointHash = listToMaybe <$> PG.query q (pid, "new-endpoint" :: Text, endpointHash)
  where
    q =
      [sql|
      SELECT * FROM apis.issues
      WHERE project_id = ? 
        AND issue_type = ?
        AND endpoint_hash = ?
        AND acknowledged_at IS NULL
        AND archived_at IS NULL
      LIMIT 1
    |]


findOpenShapeIssueForStatusCode :: DB es => Projects.ProjectId -> Text -> Int -> Eff es (Maybe Issue)
findOpenShapeIssueForStatusCode pid endpointHash statusCode = listToMaybe <$> PG.query q (pid, "new-shape" :: Text, endpointHash, statusCode)
  where
    q =
      [sql|
      SELECT * FROM apis.issues
      WHERE project_id = ? 
        AND issue_type = ?
        AND endpoint_hash = ?
        AND (issue_data->>'status_code')::int = ?
        AND acknowledged_at IS NULL
        AND archived_at IS NULL
      LIMIT 1
    |]


-- | Update issue with new anomaly data
updateIssueWithNewAnomaly :: DB es => IssueId -> APIChangeData -> Eff es ()
updateIssueWithNewAnomaly issueId newData = void $ PG.execute q (Aeson newData, issueId)
  where
    q =
      [sql|
      UPDATE apis.issues
      SET
        issue_data = issue_data || ?::jsonb,
        updated_at = NOW()
      WHERE id = ?
    |]


-- | Update issue enhancement
updateIssueEnhancement :: DB es => IssueId -> Text -> Text -> Text -> Eff es ()
updateIssueEnhancement issueId title action complexity = void $ PG.execute q params
  where
    q =
      [sql|
      UPDATE apis.issues
      SET 
        title = ?,
        recommended_action = ?,
        migration_complexity = ?,
        updated_at = NOW()
      WHERE id = ?
    |]
    params = (title, action, complexity, issueId)


-- | Update issue criticality and severity
updateIssueCriticality :: DB es => IssueId -> Bool -> Text -> Eff es ()
updateIssueCriticality issueId isCritical severity = void $ PG.execute q params
  where
    q =
      [sql|
      UPDATE apis.issues
      SET 
        critical = ?,
        severity = ?
      WHERE id = ?
    |]
    params = (isCritical, severity, issueId)


-- | Acknowledge issue
acknowledgeIssue :: DB es => IssueId -> Users.UserId -> Eff es ()
acknowledgeIssue issueId userId = void $ PG.execute q (userId, issueId)
  where
    q =
      [sql|
      UPDATE apis.issues
      SET acknowledged_at = NOW(), acknowledged_by = ?
      WHERE id = ?
    |]


-- | Derive source type from issue type
issueTypeToSourceType :: IssueType -> Text
issueTypeToSourceType NewEndpoint = "endpoint"
issueTypeToSourceType NewShape = "shape"
issueTypeToSourceType FieldChange = "shape"
issueTypeToSourceType RuntimeException = "error"
issueTypeToSourceType QueryAlert = "query"
issueTypeToSourceType LogPattern = "log_pattern"
issueTypeToSourceType ErrorEscalating = "error"
issueTypeToSourceType ErrorRegressed = "error"
issueTypeToSourceType LogPatternRateChange = "log_pattern"
issueTypeToSourceType EndpointLatencyDegradation = "endpoint"
issueTypeToSourceType EndpointErrorRateSpike = "endpoint"
issueTypeToSourceType EndpointVolumeRateChange = "endpoint"


-- | Helper to create an issue with common defaults
mkIssue :: AE.ToJSON a => Projects.ProjectId -> IssueType -> Text -> Text -> Maybe Text -> Bool -> Text -> Text -> Text -> Text -> a -> IO Issue
mkIssue projectId issueType targetHash endpointHash service critical severity title recommendedAction migrationComplexity issueData = do
  issueId <- UUIDId <$> UUID4.nextRandom
  now <- getCurrentTime
  zonedNow <- utcToLocalZonedTime now
  pure
    Issue
      { id = issueId
      , createdAt = zonedNow
      , updatedAt = zonedNow
      , projectId = projectId
      , issueType = issueType
      , sourceType = issueTypeToSourceType issueType
      , targetHash = targetHash
      , endpointHash = endpointHash
      , acknowledgedAt = Nothing
      , acknowledgedBy = Nothing
      , archivedAt = Nothing
      , title = title
      , service = service
      , environment = Nothing
      , critical = critical
      , severity = severity
      , recommendedAction = recommendedAction
      , migrationComplexity = migrationComplexity
      , issueData = Aeson $ AE.toJSON issueData
      , requestPayloads = Aeson []
      , responsePayloads = Aeson []
      , llmEnhancedAt = Nothing
      , llmEnhancementVersion = Nothing
      }


-- | Create Query Alert issue
createQueryAlertIssue :: Projects.ProjectId -> Text -> Text -> Text -> Double -> Double -> Text -> IO Issue
createQueryAlertIssue projectId queryId queryName queryExpr threshold actual thresholdType = do
  now <- getCurrentTime
  let alertData =
        QueryAlertData
          { queryId = queryId
          , queryName = queryName
          , queryExpression = queryExpr
          , thresholdValue = threshold
          , actualValue = actual
          , thresholdType = thresholdType
          , triggeredAt = now
          }
  mkIssue
    projectId
    QueryAlert
    ""
    ""
    (Just "Monitoring")
    True
    "warning"
    (queryName <> " threshold " <> thresholdType <> " " <> show threshold)
    "Review the query results and take appropriate action."
    "n/a"
    alertData


-- | Create issue for a new error
createNewErrorIssue :: Projects.ProjectId -> Errors.Error -> IO Issue
createNewErrorIssue projectId err = do
  now <- getCurrentTime
  let exceptionData =
        RuntimeExceptionData
          { errorType = err.errorType
          , errorMessage = err.message
          , stackTrace = err.stacktrace
          , requestPath = Nothing
          , requestMethod = Nothing
          , occurrenceCount = 1
          , firstSeen = now
          , lastSeen = now
          }
  mkIssue
    projectId
    RuntimeException
    err.hash
    err.hash
    err.service
    True
    "critical"
    ("New Error: " <> err.errorType <> " - " <> T.take 80 err.message)
    "Investigate the new error and implement a fix."
    "n/a"
    exceptionData


-- | Create issue for an error spike
createErrorSpikeIssue :: Projects.ProjectId -> Errors.Error -> Double -> Double -> Double -> IO Issue
createErrorSpikeIssue projectId err currentRate baselineMean baselineStddev = do
  now <- getCurrentTime
  let zScore = if baselineStddev > 0 then (currentRate - baselineMean) / baselineStddev else 0
      increasePercent = if baselineMean > 0 then ((currentRate / baselineMean) - 1) * 100 else 0
      exceptionData =
        RuntimeExceptionData
          { errorType = err.errorType
          , errorMessage = err.message
          , stackTrace = err.stacktrace
          , requestPath = Nothing
          , requestMethod = Nothing
          , occurrenceCount = round currentRate
          , firstSeen = now
          , lastSeen = now
          }
  mkIssue
    projectId
    RuntimeException
    err.hash
    err.hash
    err.service
    True
    "critical"
    ("Error Spike: " <> err.errorType <> " (" <> T.pack (show (round increasePercent :: Int)) <> "% increase)")
    ("Error rate has spiked " <> T.pack (show (round zScore :: Int)) <> " standard deviations above baseline. Current: " <> T.pack (show (round currentRate :: Int)) <> "/hr, Baseline: " <> T.pack (show (round baselineMean :: Int)) <> "/hr. Investigate recent deployments or changes.")
    "n/a"
    exceptionData


-- | Create an issue for an escalating error
createErrorEscalatingIssue :: Projects.ProjectId -> Errors.Error -> Text -> Double -> Text -> IO Issue
createErrorEscalatingIssue projectId err prevState escalationRate escalationWindow = do
  now <- getCurrentTime
  let escalatingData =
        ErrorEscalatingData
          { errorHash = err.hash
          , errorType = err.errorType
          , errorMessage = err.message
          , serviceName = err.service
          , currentState = "escalating"
          , previousState = prevState
          , occurrences1h = err.occurrences1h
          , occurrences24h = err.occurrences24h
          , escalationRate = escalationRate
          , escalationWindow = escalationWindow
          , firstSeenAt = now
          , lastSeenAt = now
          }
  mkIssue
    projectId
    ErrorEscalating
    err.hash
    err.hash
    err.service
    True
    "critical"
    ("Error Escalating: " <> err.errorType <> " (" <> T.pack (show (round (escalationRate * 100) :: Int)) <> "% increase)")
    ("Error rate is escalating (" <> T.pack (show escalationRate) <> "x over " <> escalationWindow <> "). Investigate immediately.")
    "n/a"
    escalatingData


-- | Create an issue for a regressed error
createErrorRegressedIssue :: Projects.ProjectId -> Errors.Error -> UTCTime -> Int -> Int -> IO Issue
createErrorRegressedIssue projectId err resolvedAtTime quietMins prevOccurrences = do
  now <- getCurrentTime
  let regressedData =
        ErrorRegressedData
          { errorHash = err.hash
          , errorType = err.errorType
          , errorMessage = err.message
          , serviceName = err.service
          , resolvedAt = resolvedAtTime
          , regressedAt = now
          , quietPeriodMinutes = quietMins
          , previousOccurrences = prevOccurrences
          , newOccurrences = 1
          }
  mkIssue
    projectId
    ErrorRegressed
    err.hash
    err.hash
    err.service
    True
    "critical"
    ("Error Regressed: " <> err.errorType <> " (after " <> T.pack (show quietMins) <> " min quiet)")
    ("Previously resolved error has returned after " <> T.pack (show quietMins) <> " minutes. The original fix may be incomplete.")
    "n/a"
    regressedData


-- | Create an issue for endpoint latency degradation
createEndpointLatencyDegradationIssue :: Projects.ProjectId -> Text -> Text -> Text -> Maybe Text -> Double -> Double -> Double -> Text -> V.Vector Text -> IO Issue
createEndpointLatencyDegradationIssue projectId epHash method path serviceName currentLatency baselineLatency baselineStddev percentile traceIds = do
  now <- getCurrentTime
  let degradationPct = if baselineLatency > 0 then ((currentLatency / baselineLatency) - 1) * 100 else 0
      zScoreVal = if baselineStddev > 0 then (currentLatency - baselineLatency) / baselineStddev else 0
      latencyData =
        EndpointLatencyDegradationData
          { endpointHash = epHash
          , endpointMethod = method
          , endpointPath = path
          , serviceName = serviceName
          , currentLatencyMs = currentLatency
          , baselineLatencyMs = baselineLatency
          , baselineStddev = baselineStddev
          , zScore = zScoreVal
          , degradationPercent = degradationPct
          , percentile = percentile
          , sampleTraceIds = traceIds
          , detectedAt = now
          }
      severity = if degradationPct > 100 then "critical" else if degradationPct > 50 then "warning" else "info"
  mkIssue
    projectId
    EndpointLatencyDegradation
    epHash
    epHash
    serviceName
    (degradationPct > 100)
    severity
    ("Latency Degradation: " <> method <> " " <> path <> " (" <> percentile <> " +" <> T.pack (show (round degradationPct :: Int)) <> "%)")
    ("Endpoint " <> percentile <> " latency increased from " <> T.pack (show (round baselineLatency :: Int)) <> "ms to " <> T.pack (show (round currentLatency :: Int)) <> "ms. Check recent deployments and dependencies.")
    "n/a"
    latencyData


-- | Create an issue for endpoint error rate spike
createEndpointErrorRateSpikeIssue :: Projects.ProjectId -> Text -> Text -> Text -> Maybe Text -> Double -> Double -> Double -> Int -> Int -> V.Vector Text -> IO Issue
createEndpointErrorRateSpikeIssue projectId epHash method path serviceName currentRate baselineRate baselineStddev errorCount totalReqs topErrors = do
  now <- getCurrentTime
  let zScoreVal = if baselineStddev > 0 then (currentRate - baselineRate) / baselineStddev else 0
      spikePct = if baselineRate > 0 then ((currentRate / baselineRate) - 1) * 100 else currentRate * 100
      errorRateData =
        EndpointErrorRateSpikeData
          { endpointHash = epHash
          , endpointMethod = method
          , endpointPath = path
          , serviceName = serviceName
          , currentErrorRate = currentRate
          , baselineErrorRate = baselineRate
          , baselineStddev = baselineStddev
          , zScore = zScoreVal
          , spikePercent = spikePct
          , errorCount = errorCount
          , totalRequests = totalReqs
          , topErrorTypes = topErrors
          , detectedAt = now
          }
      severity = if currentRate > 0.1 then "critical" else if currentRate > 0.05 then "warning" else "info"
  mkIssue
    projectId
    EndpointErrorRateSpike
    epHash
    epHash
    serviceName
    (currentRate > 0.1)
    severity
    ("Error Rate Spike: " <> method <> " " <> path <> " (" <> T.pack (show (round (currentRate * 100) :: Int)) <> "% errors)")
    ("Error rate spiked from " <> T.pack (show (round (baselineRate * 100) :: Int)) <> "% to " <> T.pack (show (round (currentRate * 100) :: Int)) <> "% (" <> T.pack (show errorCount) <> "/" <> T.pack (show totalReqs) <> " requests failed).")
    "n/a"
    errorRateData


-- | Create an issue for endpoint volume rate change
createEndpointVolumeRateChangeIssue :: Projects.ProjectId -> Text -> Text -> Text -> Maybe Text -> Double -> Double -> Double -> Text -> IO Issue
createEndpointVolumeRateChangeIssue projectId epHash method path serviceName currentRate baselineRate baselineStddev direction = do
  now <- getCurrentTime
  let zScoreVal = if baselineStddev > 0 then abs (currentRate - baselineRate) / baselineStddev else 0
      changePct = if baselineRate > 0 then abs ((currentRate / baselineRate) - 1) * 100 else 0
      volumeData =
        EndpointVolumeRateChangeData
          { endpointHash = epHash
          , endpointMethod = method
          , endpointPath = path
          , serviceName = serviceName
          , currentRatePerHour = currentRate
          , baselineRatePerHour = baselineRate
          , baselineStddev = baselineStddev
          , zScore = zScoreVal
          , changePercent = changePct
          , changeDirection = direction
          , detectedAt = now
          }
      severity = case (direction, changePct) of
        ("drop", pct) | pct > 80 -> "critical"
        ("drop", pct) | pct > 50 -> "warning"
        ("spike", pct) | pct > 200 -> "warning"
        _ -> "info"
  mkIssue
    projectId
    EndpointVolumeRateChange
    epHash
    epHash
    serviceName
    (direction == "drop" && changePct > 80)
    severity
    ("Traffic " <> T.toTitle direction <> ": " <> method <> " " <> path <> " (" <> T.pack (show (round changePct :: Int)) <> "%)")
    ("Endpoint traffic " <> direction <> " detected. Current: " <> T.pack (show (round currentRate :: Int)) <> " req/hr, Baseline: " <> T.pack (show (round baselineRate :: Int)) <> " req/hr.")
    "n/a"
    volumeData


-- | Create an issue for a new endpoint
createNewEndpointIssue :: Projects.ProjectId -> Text -> Text -> Text -> Text -> IO Issue
createNewEndpointIssue projectId epHash method path host = do
  now <- getCurrentTime
  let endpointData =
        NewEndpointData
          { endpointHash = epHash
          , endpointMethod = method
          , endpointPath = path
          , endpointHost = host
          , firstSeenAt = now
          , initialShapes = V.empty
          }
  mkIssue
    projectId
    NewEndpoint
    epHash
    epHash
    (Just host)
    False
    "info"
    "New Endpoint detected"
    "A new API endpoint has been detected. Review to ensure it matches your API specification."
    "n/a"
    endpointData


-- | Create an issue for a new shape
createNewShapeIssue :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Int -> AE.Value -> AE.Value -> V.Vector Text -> V.Vector Text -> V.Vector Text -> V.Vector Text -> IO Issue
createNewShapeIssue projectId shHash epHash method path statusCode reqPayload respPayload newFlds deletedFlds modifiedFlds fldHashes = do
  now <- getCurrentTime
  let shapeData =
        NewShapeData
          { shapeHash = shHash
          , endpointHash = epHash
          , endpointMethod = method
          , endpointPath = path
          , statusCode = statusCode
          , exampleRequestPayload = reqPayload
          , exampleResponsePayload = respPayload
          , newFields = newFlds
          , deletedFields = deletedFlds
          , modifiedFields = modifiedFlds
          , fieldHashes = fldHashes
          , firstSeenAt = now
          , newShapesAfterIssue = V.empty
          }
      hasBreakingChanges = not (V.null deletedFlds) || not (V.null modifiedFlds)
      changeCount = V.length newFlds + V.length deletedFlds + V.length modifiedFlds
      severity = if hasBreakingChanges then "critical" else "warning"
      complexity = if V.length deletedFlds > 5 then "high" else if hasBreakingChanges then "medium" else "low"
      action =
        if hasBreakingChanges
          then "Breaking API changes detected: " <> T.pack (show (V.length deletedFlds)) <> " deleted, " <> T.pack (show (V.length modifiedFlds)) <> " modified fields. Update clients immediately."
          else "New API shape detected with " <> T.pack (show (V.length newFlds)) <> " new fields. Review for compatibility."
  mkIssue
    projectId
    NewShape
    shHash
    epHash
    Nothing
    hasBreakingChanges
    severity
    ("New Shape: " <> method <> " " <> path <> " (" <> T.pack (show statusCode) <> ") - " <> T.pack (show changeCount) <> " field changes")
    action
    complexity
    shapeData


-- | Create an issue for a field change
createFieldChangeIssue :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> Text -> Text -> IO Issue
createFieldChangeIssue projectId fldHash epHash method path keyPath category prevType newType changeType = do
  let fieldData =
        FieldChangeData
          { fieldHash = fldHash
          , endpointHash = epHash
          , endpointMethod = method
          , endpointPath = path
          , keyPath = keyPath
          , fieldCategory = category
          , previousType = prevType
          , newType = newType
          , changeType = changeType
          }
      isBreaking = changeType `elem` ["removed", "type_changed"]
      titlePrefix = case changeType of
        "added" -> "New Field"
        "removed" -> "Removed Field"
        "type_changed" -> "Field Type Changed"
        "format_changed" -> "Field Format Changed"
        _ -> "Field Changed"
  mkIssue
    projectId
    FieldChange
    fldHash
    epHash
    Nothing
    isBreaking
    (if isBreaking then "critical" else "info")
    (titlePrefix <> ": " <> keyPath <> " in " <> method <> " " <> path)
    ""
    (if isBreaking then "medium" else "low")
    fieldData


updateIssueData :: DB es => IssueId -> AE.Value -> Eff es ()
updateIssueData issueId newData = void $ PG.execute q (Aeson newData, issueId)
  where
    q =
      [sql|
      UPDATE apis.issues
      SET issue_data = ?
      WHERE  id = ?
    |]


-- | Conversation type for AI chats
data ConversationType = CTAnomaly | CTTrace | CTLogExplorer | CTDashboard | CTSlackThread | CTDiscordThread
  deriving stock (Eq, Generic, Read, Show)
  deriving (Display, FromField, ToField) via WrappedEnumSC "CT" ConversationType


instance Default ConversationType where
  def = CTAnomaly


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
  deriving anyclass (Default, FromRow, ToRow)


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
  deriving anyclass (Default, FromRow, ToRow)


-- | Get or create a conversation (race-condition safe via ON CONFLICT + RETURNING)
getOrCreateConversation :: DB es => Projects.ProjectId -> UUIDId "conversation" -> ConversationType -> AE.Value -> Eff es AIConversation
getOrCreateConversation pid convId convType ctx =
  fromMaybe (error "getOrCreateConversation: RETURNING clause must return a row") . listToMaybe <$> PG.query q (pid, convId, convType, Aeson ctx)
  where
    q =
      [sql| INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, context)
              VALUES (?, ?, ?, ?) ON CONFLICT (project_id, conversation_id) DO UPDATE SET updated_at = NOW()
              RETURNING id, project_id, conversation_id, conversation_type, context, created_at, updated_at |]


-- | Insert a new chat message
insertChatMessage :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Text -> Text -> Maybe AE.Value -> Maybe AE.Value -> Eff es ()
insertChatMessage pid convId role content widgetsM metadataM = void $ PG.execute q params
  where
    q =
      [sql|
      INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, widgets, metadata)
      VALUES (?, ?, ?, ?, ?, ?)
    |]
    params = (pid, convId, role, content, Aeson <$> widgetsM, Aeson <$> metadataM)


-- | Select chat history for a conversation (oldest first, limited to 20)
selectChatHistory :: DB es => UUIDId "conversation" -> Eff es [AIChatMessage]
selectChatHistory convId = PG.query q (Only convId)
  where
    q =
      [sql|
      SELECT id, project_id, conversation_id, role, content, widgets, metadata, created_at
      FROM apis.ai_chat_messages
      WHERE conversation_id = ?
      ORDER BY created_at ASC
      LIMIT 20
    |]


-- | Generate deterministic UUID v5 from text (uses OID namespace)
textToConversationId :: Text -> UUIDId "conversation"
textToConversationId = UUIDId . UUID5.generateNamed UUID5.namespaceOID . BS.unpack . encodeUtf8


slackThreadToConversationId :: Text -> Text -> UUIDId "conversation"
slackThreadToConversationId cid ts = textToConversationId (cid <> ":" <> ts)


discordThreadToConversationId :: Text -> UUIDId "conversation"
discordThreadToConversationId = textToConversationId
