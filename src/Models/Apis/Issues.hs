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

  -- * Conversion Functions
  createAPIChangeIssue,
  createRuntimeExceptionIssue,
  createQueryAlertIssue,

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
) where

import Data.Aeson qualified as AE
import Data.Default (Default, def)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime)
import Data.UUID.V4 qualified as UUID
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
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pkg.DeriveUtils (UUIDId (..), idToText)
import Relude hiding (id)
import System.Types (DB)
import Utils (formatUTC)


type IssueId = UUIDId "issue"


issueIdText :: IssueId -> Text
issueIdText = idToText


-- | Issue types
data IssueType
  = APIChange
  | RuntimeException
  | QueryAlert
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.CamelToSnake]] IssueType


instance Default IssueType where
  def = APIChange


issueTypeToText :: IssueType -> Text
issueTypeToText APIChange = "api_change" -- Maps to anomaly_type 'shape' in DB
issueTypeToText RuntimeException = "runtime_exception"
issueTypeToText QueryAlert = "query_alert"


parseIssueType :: Text -> Maybe IssueType
parseIssueType "api_change" = Just APIChange
parseIssueType "shape" = Just APIChange -- Handle DB anomaly_type
parseIssueType "runtime_exception" = Just RuntimeException
parseIssueType "query_alert" = Just QueryAlert
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


-- | Main Issue type
data Issue = Issue
  { id :: IssueId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , issueType :: IssueType
  , endpointHash :: Text -- For API changes, empty for others
  -- Status fields
  , acknowledgedAt :: Maybe ZonedTime
  , acknowledgedBy :: Maybe Users.UserId
  , archivedAt :: Maybe ZonedTime
  , -- Issue details
    title :: Text
  , service :: Text
  , critical :: Bool
  , severity :: Text -- "critical", "warning", "info"
  -- Impact metrics
  , affectedRequests :: Int
  , affectedClients :: Int
  , errorRate :: Maybe Double
  , -- Actions
    recommendedAction :: Text
  , migrationComplexity :: Text -- "low", "medium", "high", "n/a"
  -- Data payload (polymorphic based on issueType)
  , issueData :: Aeson AE.Value
  , -- Payload changes tracking (for API changes)
    requestPayloads :: Aeson [PayloadChange]
  , responsePayloads :: Aeson [PayloadChange]
  , -- LLM enhancement tracking
    llmEnhancedAt :: Maybe UTCTime
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
      , issueType = def
      , endpointHash = ""
      , acknowledgedAt = Nothing
      , acknowledgedBy = Nothing
      , archivedAt = Nothing
      , title = ""
      , service = ""
      , critical = False
      , severity = "info"
      , affectedRequests = 0
      , affectedClients = 0
      , errorRate = Nothing
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
  , service :: Text
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
  id, created_at, updated_at, project_id, issue_type, endpoint_hash,
  acknowledged_at, acknowledged_by, archived_at,
  title, service, critical, severity,
  affected_requests, affected_clients, error_rate,
  recommended_action, migration_complexity,
  issue_data, request_payloads, response_payloads,
  llm_enhanced_at, llm_enhancement_version
) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT (project_id, endpoint_hash)
  WHERE issue_type = 'api_change'
    AND acknowledged_at IS NULL
    AND archived_at IS NULL
    AND endpoint_hash != ''
DO UPDATE SET
  updated_at = EXCLUDED.updated_at,
  affected_requests = issues.affected_requests + EXCLUDED.affected_requests,
  affected_clients = GREATEST(issues.affected_clients, EXCLUDED.affected_clients),
  issue_data = issues.issue_data || EXCLUDED.issue_data
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
        CASE WHEN critical THEN 'critical' ELSE 'info' END, affected_requests, affected_clients, NULL::double precision,
        recommended_action, migration_complexity, issue_data, request_payloads, response_payloads, NULL::timestamp with time zone, NULL::int, 0::bigint, updated_at
      FROM apis.issues WHERE project_id = ? $timefilter $ackF $archF $orderBy LIMIT ? OFFSET ?
    |]
    countQ = [text|SELECT COUNT(*) FROM apis.issues WHERE project_id = ? $timefilter $ackF $archF|]


-- | Find open issue for endpoint
findOpenIssueForEndpoint :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Issue)
findOpenIssueForEndpoint pid endpointHash = listToMaybe <$> PG.query q (pid, "api_change" :: Text, endpointHash)
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


-- | Update issue with new anomaly data
updateIssueWithNewAnomaly :: DB es => IssueId -> APIChangeData -> Eff es ()
updateIssueWithNewAnomaly issueId newData = void $ PG.execute q (Aeson newData, issueId)
  where
    q =
      [sql|
      UPDATE apis.issues
      SET 
        issue_data = issue_data || ?::jsonb,
        affected_requests = affected_requests + 1,
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


-- | Create API Change issue from anomalies
createAPIChangeIssue :: Projects.ProjectId -> Text -> V.Vector Anomalies.AnomalyVM -> IO Issue
createAPIChangeIssue projectId endpointHash anomalies = do
  issueId <- UUIDId <$> UUID.nextRandom
  now <- getCurrentTime

  let firstAnomaly = V.head anomalies
      apiChangeData =
        APIChangeData
          { endpointMethod = fromMaybe "UNKNOWN" firstAnomaly.endpointMethod
          , endpointPath = fromMaybe "/" firstAnomaly.endpointUrlPath
          , endpointHost = "Unknown"
          , anomalyHashes = V.map (.targetHash) anomalies
          , shapeChanges = V.empty -- Simplified for now
          , formatChanges = V.empty -- Simplified for now
          , newFields = V.concatMap (.shapeNewUniqueFields) anomalies
          , deletedFields = V.concatMap (.shapeDeletedFields) anomalies
          , modifiedFields = V.concatMap (.shapeUpdatedFieldFormats) anomalies
          }

      breakingChanges = V.length apiChangeData.deletedFields + V.length apiChangeData.modifiedFields
      isCritical = breakingChanges > 0

  pure
    Issue
      { id = issueId
      , createdAt = firstAnomaly.createdAt
      , updatedAt = firstAnomaly.updatedAt
      , projectId = projectId
      , issueType = APIChange
      , endpointHash = endpointHash
      , acknowledgedAt = Nothing
      , acknowledgedBy = Nothing
      , archivedAt = Nothing
      , title = "API structure has changed"
      , service = Anomalies.detectService Nothing firstAnomaly.endpointUrlPath
      , critical = isCritical
      , severity = if isCritical then "critical" else "warning"
      , affectedRequests = 0
      , affectedClients = 0
      , errorRate = Nothing
      , recommendedAction = "Review the API changes and update your integration accordingly."
      , migrationComplexity = if breakingChanges > 5 then "high" else if breakingChanges > 0 then "medium" else "low"
      , issueData = Aeson $ AE.toJSON apiChangeData
      , requestPayloads = Aeson [] -- Will be populated during enhancement
      , responsePayloads = Aeson [] -- Will be populated during enhancement
      , llmEnhancedAt = Nothing
      , llmEnhancementVersion = Nothing
      }


-- | Create Runtime Exception issue
createRuntimeExceptionIssue :: Projects.ProjectId -> RequestDumps.ATError -> IO Issue
createRuntimeExceptionIssue projectId atError = do
  issueId <- UUIDId <$> UUID.nextRandom
  errorZonedTime <- utcToLocalZonedTime atError.when

  let exceptionData =
        RuntimeExceptionData
          { errorType = atError.errorType
          , errorMessage = atError.message
          , stackTrace = atError.stackTrace
          , requestPath = atError.requestPath
          , requestMethod = atError.requestMethod
          , occurrenceCount = 1
          , firstSeen = atError.when
          , lastSeen = atError.when
          }

  pure
    Issue
      { id = issueId
      , createdAt = errorZonedTime
      , updatedAt = errorZonedTime
      , projectId = projectId
      , issueType = RuntimeException
      , endpointHash = fromMaybe "" atError.hash
      , acknowledgedAt = Nothing
      , acknowledgedBy = Nothing
      , archivedAt = Nothing
      , title = atError.rootErrorType <> ": " <> T.take 100 atError.message
      , service = Anomalies.detectService Nothing atError.requestPath
      , critical = True
      , severity = "critical"
      , affectedRequests = 1
      , affectedClients = 0
      , errorRate = Nothing
      , recommendedAction = "Investigate the error and implement a fix."
      , migrationComplexity = "n/a"
      , issueData = Aeson $ AE.toJSON exceptionData
      , requestPayloads = Aeson []
      , responsePayloads = Aeson []
      , llmEnhancedAt = Nothing
      , llmEnhancementVersion = Nothing
      }


-- | Create Query Alert issue
createQueryAlertIssue :: Projects.ProjectId -> Text -> Text -> Text -> Double -> Double -> Text -> IO Issue
createQueryAlertIssue projectId queryId queryName queryExpr threshold actual thresholdType = do
  issueId <- UUIDId <$> UUID.nextRandom
  now <- getCurrentTime
  zonedNow <- utcToLocalZonedTime now

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

  pure
    Issue
      { id = issueId
      , createdAt = zonedNow
      , updatedAt = zonedNow
      , projectId = projectId
      , issueType = QueryAlert
      , endpointHash = ""
      , acknowledgedAt = Nothing
      , acknowledgedBy = Nothing
      , archivedAt = Nothing
      , title = queryName <> " threshold " <> thresholdType <> " " <> show threshold
      , service = "Monitoring"
      , critical = True
      , severity = "warning"
      , affectedRequests = 0
      , affectedClients = 0
      , errorRate = Nothing
      , recommendedAction = "Review the query results and take appropriate action."
      , migrationComplexity = "n/a"
      , issueData = Aeson $ AE.toJSON alertData
      , requestPayloads = Aeson []
      , responsePayloads = Aeson []
      , llmEnhancedAt = Nothing
      , llmEnhancementVersion = Nothing
      }


-- | Conversation type for AI chats
data ConversationType = CTAnomaly | CTTrace | CTLogExplorer | CTDashboard
  deriving stock (Generic, Show, Eq)

instance Default ConversationType where
  def = CTAnomaly

instance ToField ConversationType where
  toField CTAnomaly = Escape "anomaly"
  toField CTTrace = Escape "trace"
  toField CTLogExplorer = Escape "log_explorer"
  toField CTDashboard = Escape "dashboard"

instance FromField ConversationType where
  fromField f mdata = do
    txt <- fromField @Text f mdata
    case txt of
      "anomaly" -> pure CTAnomaly
      "trace" -> pure CTTrace
      "log_explorer" -> pure CTLogExplorer
      "dashboard" -> pure CTDashboard
      _ -> returnError ConversionFailed f "Invalid conversation type"


-- | AI Conversation metadata
data AIConversation = AIConversation
  { id :: UUIDId "ai_conversation"
  , projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"  -- The contextual ID (issue_id, trace_id, etc.)
  , conversationType :: ConversationType
  , context :: Maybe (Aeson AE.Value)  -- Initial context for the AI
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
  , role :: Text  -- "user", "assistant", or "system"
  , content :: Text
  , widgets :: Maybe (Aeson AE.Value)  -- Array of widget configs
  , metadata :: Maybe (Aeson AE.Value)  -- Additional metadata
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, ToRow)


-- | Get or create a conversation for a given context
getOrCreateConversation :: DB es => Projects.ProjectId -> UUIDId "conversation" -> ConversationType -> AE.Value -> Eff es AIConversation
getOrCreateConversation pid convId convType ctx = do
  existing <- listToMaybe <$> PG.query selectQ (pid, convId)
  case existing of
    Just conv -> pure conv
    Nothing -> do
      void $ PG.execute insertQ (pid, convId, convType, Aeson ctx)
      conv <- listToMaybe <$> PG.query selectQ (pid, convId)
      pure $ fromMaybe (error "Failed to create conversation") conv
  where
    selectQ = [sql|
      SELECT id, project_id, conversation_id, conversation_type, context, created_at, updated_at
      FROM apis.ai_conversations
      WHERE project_id = ? AND conversation_id = ?
      ORDER BY created_at DESC
      LIMIT 1
    |]
    insertQ = [sql|
      INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, context)
      VALUES (?, ?, ?, ?)
    |]


-- | Insert a new chat message
insertChatMessage :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Text -> Text -> Maybe AE.Value -> Maybe AE.Value -> Eff es ()
insertChatMessage pid convId role content widgetsM metadataM = void $ PG.execute q params
  where
    q = [sql|
      INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, widgets, metadata)
      VALUES (?, ?, ?, ?, ?, ?)
    |]
    params = (pid, convId, role, content, Aeson <$> widgetsM, Aeson <$> metadataM)


-- | Select chat history for a conversation (oldest first, limited to 20)
selectChatHistory :: DB es => UUIDId "conversation" -> Eff es [AIChatMessage]
selectChatHistory convId = PG.query q (Only convId)
  where
    q = [sql|
      SELECT id, project_id, conversation_id, role, content, widgets, metadata, created_at
      FROM apis.ai_chat_messages
      WHERE conversation_id = ?
      ORDER BY created_at ASC
      LIMIT 20
    |]
