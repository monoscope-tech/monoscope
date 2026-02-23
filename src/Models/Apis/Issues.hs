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
  createLogPatternIssue,
  createLogPatternRateChangeIssue,

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
  tryAcquireChatMigrationLock,

  -- * Thread ID Helpers
  slackThreadToConversationId,
  discordThreadToConversationId,

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
import Data.Default (Default, def)
import Data.Effectful.UUID (UUIDEff, genUUID)
import Data.Hashable (hash)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Time (UTCTime)
import Data.Time.LocalTime (ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.UUID.V5 qualified as UUID5
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_insert, _selectWhere)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL qualified as PG
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Models.Apis.Anomalies (PayloadChange)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Users
import NeatInterpolation (text)
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), idToText)
import Relude hiding (id)
import Servant (ServerError, err500, errBody)
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
  | LogPattern
  | LogPatternRateChange
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.CamelToSnake]] IssueType


issueTypeToText :: IssueType -> Text
issueTypeToText APIChange = "api_change" -- Maps to anomaly_type 'shape' in DB
issueTypeToText RuntimeException = "runtime_exception"
issueTypeToText QueryAlert = "query_alert"
issueTypeToText LogPattern = "log_pattern"
issueTypeToText LogPatternRateChange = "log_pattern_rate_change"


parseIssueType :: Text -> Maybe IssueType
parseIssueType "api_change" = Just APIChange
parseIssueType "shape" = Just APIChange -- Handle DB anomaly_type
parseIssueType "runtime_exception" = Just RuntimeException
parseIssueType "query_alert" = Just QueryAlert
parseIssueType "log_pattern" = Just LogPattern
parseIssueType "log_pattern_rate_change" = Just LogPatternRateChange
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
  , sourceType :: Maybe Text
  , targetHash :: Text -- Must stay non-null: used in ON CONFLICT unique index for dedup
  , endpointHash :: Text -- Legacy: always set to targetHash by mkIssue
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
      , sourceType = Nothing
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
selectIssueByHash pid endpointHash = listToMaybe <$> PG.query (_selectWhere @Issue [[field| project_id |], [field| endpoint_hash |]]) (pid, endpointHash)


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
createAPIChangeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> Text -> V.Vector Anomalies.AnomalyVM -> Eff es Issue
createAPIChangeIssue projectId endpointHash anomalies = do
  let firstAnomaly = V.head anomalies
      apiChangeData =
        APIChangeData
          { endpointMethod = fromMaybe "UNKNOWN" firstAnomaly.endpointMethod
          , endpointPath = fromMaybe "/" firstAnomaly.endpointUrlPath
          , endpointHost = "Unknown"
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
      , issueType = APIChange
      , targetHash = endpointHash
      , service = Just $ Anomalies.detectService Nothing firstAnomaly.endpointUrlPath
      , critical = isCritical
      , severity = if isCritical then "critical" else "warning"
      , title = "API structure has changed"
      , recommendedAction = "Review the API changes and update your integration accordingly."
      , migrationComplexity = if breakingChanges > 5 then "high" else if breakingChanges > 0 then "medium" else "low"
      , issueData = apiChangeData
      , timestamp = Just firstAnomaly.createdAt
      }


-- | Create Runtime Exception issue
createRuntimeExceptionIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> RequestDumps.ATError -> Eff es Issue
createRuntimeExceptionIssue projectId atError =
  mkIssue
    MkIssueOpts
      { projectId
      , issueType = RuntimeException
      , targetHash = fromMaybe "" atError.hash
      , service = atError.serviceName
      , critical = True
      , severity = "critical"
      , title = atError.rootErrorType <> ": " <> T.take 100 atError.message
      , recommendedAction = "Investigate the error and implement a fix."
      , migrationComplexity = "n/a"
      , issueData =
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
      , timestamp = Just (utcToZonedTime utc atError.when)
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
  deriving anyclass (Default)
  deriving (Display, FromField, ToField) via WrappedEnumSC "CT" ConversationType


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
getOrCreateConversation :: (DB es, Error ServerError :> es) => Projects.ProjectId -> UUIDId "conversation" -> ConversationType -> AE.Value -> Eff es AIConversation
getOrCreateConversation pid convId convType ctx = do
  result <- PG.query q (pid, convId, convType, Aeson ctx)
  maybe (throwError err500{errBody = "getOrCreateConversation: RETURNING clause must return a row"}) pure $ listToMaybe result
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
      [sql| INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, widgets, metadata)
            VALUES (?, ?, ?, ?, ?, ?) |]
    params = (pid, convId, role, content, Aeson <$> widgetsM, Aeson <$> metadataM)


-- | Select chat history for a conversation (oldest first)
selectChatHistory :: DB es => UUIDId "conversation" -> Eff es [AIChatMessage]
selectChatHistory convId = PG.query q (Only convId)
  where
    q =
      [sql| SELECT id, project_id, conversation_id, role, content, widgets, metadata, created_at
            FROM apis.ai_chat_messages
            WHERE conversation_id = ?
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
tryAcquireChatMigrationLock :: DB es => UUIDId "conversation" -> Eff es Bool
tryAcquireChatMigrationLock convId = do
  -- Use conversation ID hash as lock key (pg_try_advisory_lock takes bigint)
  let lockKey = fromIntegral @Int @Int64 $ abs $ hash $ show convId.unwrap
  -- Try to acquire lock, returns immediately with True/False
  result <- PG.query [sql| SELECT pg_try_advisory_lock(?) |] (Only lockKey)
  pure $ fromMaybe False $ viaNonEmpty head result >>= \(Only acquired) -> Just acquired


-- | Create an issue for a log pattern rate change
createLogPatternRateChangeIssue :: (Time :> es, UUIDEff :> es) => Projects.ProjectId -> LogPatterns.LogPatternWithRate -> Double -> Double -> Double -> RateChangeDirection -> Eff es Issue
createLogPatternRateChangeIssue projectId lp currentRate baselineMean baselineMad direction = do
  now <- Time.currentTime
  let zScoreVal = if baselineMad > 0 then abs (currentRate - baselineMean) / baselineMad else 0
      changePercentVal = if baselineMean > 0 then abs ((currentRate / baselineMean) - 1) * 100 else 0
      rateChangeData =
        LogPatternRateChangeData
          { patternHash = lp.patternHash
          , logPattern = lp.logPattern
          , sampleMessage = lp.sampleMessage
          , logLevel = lp.logLevel
          , serviceName = lp.serviceName
          , sourceField = Just lp.sourceField
          , currentRatePerHour = currentRate
          , baselineMean = baselineMean
          , baselineMad = baselineMad
          , zScore = zScoreVal
          , changePercent = changePercentVal
          , changeDirection = direction
          , detectedAt = now
          }
      severity = case (direction, lp.logLevel) of
        (Spike, Just "error") -> "critical"
        (Spike, _) -> "warning"
        (Drop, _) -> "info"
  let dir = display direction
  mkIssue
    MkIssueOpts
      { projectId
      , issueType = LogPatternRateChange
      , targetHash = lp.patternHash
      , service = lp.serviceName
      , critical = direction == Spike && lp.logLevel == Just "error"
      , severity
      , title = "Log Pattern " <> T.toTitle dir <> ": " <> T.take 60 lp.logPattern <> " (" <> show (round changePercentVal :: Int) <> "%)"
      , recommendedAction = "Log pattern volume " <> dir <> " detected. Current: " <> show (round currentRate :: Int) <> "/hr, Baseline: " <> show (round baselineMean :: Int) <> "/hr (" <> show (round zScoreVal :: Int) <> " std devs)."
      , migrationComplexity = "n/a"
      , issueData = rateChangeData
      , timestamp = Nothing
      }


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
          , sourceField = Just lp.sourceField
          , firstSeenAt = zonedTimeToUTC lp.firstSeenAt
          , occurrenceCount = fromIntegral lp.occurrenceCount
          }
      severity = case lp.logLevel of
        Just "error" -> "critical"
        Just "warning" -> "warning"
        _ -> "info"
  mkIssue
    MkIssueOpts
      { projectId
      , issueType = LogPattern
      , targetHash = lp.patternHash
      , service = lp.serviceName
      , critical = lp.logLevel == Just "error"
      , severity
      , title = "New Log Pattern: " <> T.take 100 lp.logPattern
      , recommendedAction = "A new log pattern has been detected. Review to ensure it's expected behavior."
      , migrationComplexity = "n/a"
      , issueData = logPatternData
      , timestamp = Nothing
      }


-- | Log Pattern issue data (new pattern detected)
data LogPatternData = LogPatternData
  { patternHash :: Text
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , sourceField :: Maybe Text
  , firstSeenAt :: UTCTime
  , occurrenceCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson LogPatternData
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LogPatternData


data RateChangeDirection = Spike | Drop
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.CamelToSnake]] RateChangeDirection
  deriving (Display) via WrappedEnumSC "" RateChangeDirection


-- | Log Pattern Rate Change issue data (volume spike/drop)
data LogPatternRateChangeData = LogPatternRateChangeData
  { patternHash :: Text
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , sourceField :: Maybe Text
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
      , sourceType = Just $ issueTypeToText opts.issueType
      , targetHash = opts.targetHash
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
      , issueData = Aeson $ AE.toJSON opts.issueData
      , requestPayloads = Aeson []
      , responsePayloads = Aeson []
      , llmEnhancedAt = Nothing
      , llmEnhancementVersion = Nothing
      }


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
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Report)


data ReportListItem = ReportListItem
  { id :: ReportId
  , createdAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , reportType :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "reports", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ReportListItem)


addReport :: DB es => Report -> Eff es ()
addReport report = void $ PG.execute (_insert @Report) report


getReportById :: DB es => ReportId -> Eff es (Maybe Report)
getReportById id' = listToMaybe <$> PG.query (_selectWhere @Report [[field| id |]]) (Only id')


reportHistoryByProject :: DB es => Projects.ProjectId -> Int -> Eff es [ReportListItem]
reportHistoryByProject pid page = PG.query q (pid, offset)
  where
    offset = page * 20
    q = [sql| SELECT id, created_at, project_id, report_type FROM apis.reports WHERE project_id = ? ORDER BY created_at DESC LIMIT 20 OFFSET ?; |]


getLatestReportByType :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Report)
getLatestReportByType pid reportType = listToMaybe <$> PG.query q (pid, reportType)
  where
    q = [sql| SELECT id, created_at, updated_at, project_id, report_type, report_json, start_time, end_time FROM apis.reports WHERE project_id = ? AND report_type = ? ORDER BY created_at DESC LIMIT 1 |]
