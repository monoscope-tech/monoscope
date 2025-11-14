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
  IssueId (..),
  IssueType (..),
  Issue (..),
  IssueL (..),

  -- * Issue Data Types
  APIChangeData (..),
  RuntimeExceptionData (..),
  QueryAlertData (..),

  -- * Database Operations
  insertIssue,
  insertIssues,
  selectIssueById,
  selectIssues,
  findOpenIssueForEndpoint,
  updateIssueWithNewAnomaly,
  updateIssueEnhancement,
  updateIssueCriticality,
  acknowledgeIssue,
  archiveIssue,

  -- * Conversion Functions
  createAPIChangeIssue,
  createRuntimeExceptionIssue,
  createQueryAlertIssue,

  -- * Utilities
  issueIdText,
  parseIssueType,
  issueTypeToText,
) where

import Data.Aeson qualified as AE
import Data.Default (Default, def)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (execute, query, queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (..), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as DAE
import Models.Apis.Anomalies (PayloadChange)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Relude hiding (id)
import Servant (FromHttpApiData (..))
import Utils (formatUTC)


-- | Issue ID type
newtype IssueId = IssueId {unIssueId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)
  deriving newtype (Default, Eq, FromField, FromHttpApiData, Ord, ToField)


issueIdText :: IssueId -> Text
issueIdText = UUID.toText . unIssueId


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
insertIssue :: Issue -> DBT IO ()
insertIssue issue = void $ execute q issue
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
ON CONFLICT ON CONSTRAINT unique_open_api_change_per_endpoint
DO UPDATE SET
  updated_at = EXCLUDED.updated_at,
  affected_requests = issues.affected_requests + EXCLUDED.affected_requests,
  affected_clients = GREATEST(issues.affected_clients, EXCLUDED.affected_clients),
  issue_data = issues.issue_data || EXCLUDED.issue_data
    |]


-- | Insert multiple issues
insertIssues :: V.Vector Issue -> DBT IO Int64
insertIssues issues = executeMany q (V.toList issues)
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
    |]


-- | Select issue by ID
selectIssueById :: IssueId -> DBT IO (Maybe Issue)
selectIssueById = selectOneByField [field| id |] . Only


-- | Select issues with filters
selectIssues :: Projects.ProjectId -> Maybe IssueType -> Maybe Bool -> Maybe Bool -> Int -> Int -> Maybe (UTCTime, UTCTime) -> DBT IO (V.Vector IssueL)
selectIssues pid typeM isAcknowledged isArchived limit offset timeRangeM = query (Query $ encodeUtf8 q) params
  where
    -- Query must return columns in the exact order of IssueL fields

    timefilter = case timeRangeM of
      Just (st, end) -> " AND created_at >= '" <> formatUTC st <> "' AND created_at <= '" <> formatUTC end <> "'"
      _ -> ""
    ackF = ""
    q =
      [text|
      SELECT 
        id,                                               -- 1. id
        created_at,                                       -- 2. createdAt
        updated_at,                                       -- 3. updatedAt
        project_id,                                       -- 4. projectId
        issue_type::text,                              -- 5. issueType (converted)
        endpoint_hash,                                    -- 6. endpointHash
        acknowledged_at,                                  -- 7. acknowledgedAt
        acknowledged_by,                                  -- 8. acknowledgedBy
        archived_at,                                      -- 9. archivedAt
        title,                                           -- 10. title
        service,                                         -- 11. service
        critical,                                        -- 12. critical
        CASE WHEN critical THEN 'critical' ELSE 'info' END, -- 13. severity
        affected_requests,                               -- 14. affectedRequests
        affected_clients,                                -- 15. affectedClients
        NULL::double precision,                          -- 16. errorRate
        recommended_action,                              -- 17. recommendedAction
        migration_complexity,                            -- 18. migrationComplexity
        issue_data,                                      -- 19. issueData
        request_payloads,                                -- 20. requestPayloads
        response_payloads,                               -- 21. responsePayloads
        NULL::timestamp with time zone,                  -- 22. llmEnhancedAt
        NULL::int,                                       -- 23. llmEnhancementVersion
        0::bigint,                                       -- 24. eventCount
        updated_at                                       -- 25. lastSeen
      FROM apis.issues
      WHERE project_id = ?  $timefilter $ackF
      ORDER BY critical DESC, created_at DESC
      LIMIT ? OFFSET ?
    |]
    params = (pid, limit, offset)


-- | Find open issue for endpoint
findOpenIssueForEndpoint :: Projects.ProjectId -> Text -> DBT IO (Maybe Issue)
findOpenIssueForEndpoint pid endpointHash = queryOne q (pid, "api_change" :: Text, endpointHash)
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
updateIssueWithNewAnomaly :: IssueId -> APIChangeData -> DBT IO ()
updateIssueWithNewAnomaly issueId newData = void $ execute q (Aeson newData, issueId)
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
updateIssueEnhancement :: IssueId -> Text -> Text -> Text -> DBT IO ()
updateIssueEnhancement issueId title action complexity = void $ execute q params
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
updateIssueCriticality :: IssueId -> Bool -> Text -> DBT IO ()
updateIssueCriticality issueId isCritical severity = void $ execute q params
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
acknowledgeIssue :: IssueId -> Users.UserId -> DBT IO ()
acknowledgeIssue issueId userId = void $ execute q (userId, issueId)
  where
    q =
      [sql|
      UPDATE apis.issues
      SET acknowledged_at = NOW(), acknowledged_by = ?
      WHERE id = ?
    |]


-- | Archive issue
archiveIssue :: IssueId -> DBT IO ()
archiveIssue issueId = void $ execute q (Only issueId)
  where
    q = [sql| UPDATE apis.issues SET archived_at = NOW() WHERE id = ? |]


-- | Create API Change issue from anomalies
createAPIChangeIssue :: Projects.ProjectId -> Text -> V.Vector Anomalies.AnomalyVM -> IO Issue
createAPIChangeIssue projectId endpointHash anomalies = do
  issueId <- IssueId <$> UUID.nextRandom
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
  issueId <- IssueId <$> UUID.nextRandom
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
  issueId <- IssueId <$> UUID.nextRandom
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
