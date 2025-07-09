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
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (execute, query, queryOne)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, returnError, ResultError (..))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Relude hiding (id)
import Servant (FromHttpApiData (..))
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC, utcToLocalZonedTime)

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
issueTypeToText APIChange = "api_change"
issueTypeToText RuntimeException = "runtime_exception"
issueTypeToText QueryAlert = "query_alert"

parseIssueType :: Text -> Maybe IssueType
parseIssueType "api_change" = Just APIChange
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
  , shapeChanges :: V.Vector Anomalies.AnomalyVM
  , formatChanges :: V.Vector Anomalies.AnomalyVM
  , newFields :: V.Vector Text
  , deletedFields :: V.Vector Text
  , modifiedFields :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] APIChangeData
  deriving (FromField, ToField) via Aeson APIChangeData

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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] RuntimeExceptionData
  deriving (FromField, ToField) via Aeson RuntimeExceptionData

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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryAlertData
  deriving (FromField, ToField) via Aeson QueryAlertData

-- | Main Issue type
data Issue = Issue
  { id :: IssueId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , issueType :: IssueType
  , endpointHash :: Text -- For API changes, empty for others
  , -- Status fields
  , acknowledgedAt :: Maybe ZonedTime
  , acknowledgedBy :: Maybe Users.UserId
  , archivedAt :: Maybe ZonedTime
  , -- Issue details
  , title :: Text
  , service :: Text
  , critical :: Bool
  , severity :: Text -- "critical", "warning", "info"
  , -- Impact metrics
  , affectedRequests :: Int
  , affectedClients :: Int
  , errorRate :: Maybe Double
  , -- Actions
  , recommendedAction :: Text
  , migrationComplexity :: Text -- "low", "medium", "high", "n/a"
  , -- Data payload (polymorphic based on issueType)
  , issueData :: Aeson AE.Value
  , -- LLM enhancement tracking
  , llmEnhancedAt :: Maybe UTCTime
  , llmEnhancementVersion :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "issues", PrimaryKey "id", FieldModifiers '[DAE.CamelToSnake]] Issue)

-- | Issue with aggregated event data (for list views)
data IssueL = IssueL
  { id :: IssueId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , issueType :: IssueType
  , endpointHash :: Text
  , acknowledgedAt :: Maybe ZonedTime
  , acknowledgedBy :: Maybe Users.UserId
  , archivedAt :: Maybe ZonedTime
  , title :: Text
  , service :: Text
  , critical :: Bool
  , severity :: Text
  , affectedRequests :: Int
  , affectedClients :: Int
  , errorRate :: Maybe Double
  , recommendedAction :: Text
  , migrationComplexity :: Text
  , issueData :: Aeson AE.Value
  , llmEnhancedAt :: Maybe UTCTime
  , llmEnhancementVersion :: Maybe Int
  , -- Aggregated data
  , eventCount :: Int
  , lastSeen :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData)

-- | Insert a single issue
insertIssue :: Issue -> DBT IO ()
insertIssue issue = void $ execute q issue
  where
    q = [sql|
      INSERT INTO apis.issues (
        id, created_at, updated_at, project_id, issue_type, endpoint_hash,
        acknowledged_at, acknowledged_by, archived_at,
        title, service, critical, severity,
        affected_requests, affected_clients, error_rate,
        recommended_action, migration_complexity,
        issue_data, llm_enhanced_at, llm_enhancement_version
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT (project_id, issue_type, endpoint_hash) 
      WHERE acknowledged_at IS NULL AND archived_at IS NULL
      DO UPDATE SET
        updated_at = EXCLUDED.updated_at,
        affected_requests = issues.affected_requests + EXCLUDED.affected_requests,
        affected_clients = MAX(issues.affected_clients, EXCLUDED.affected_clients),
        issue_data = issues.issue_data || EXCLUDED.issue_data
    |]

-- | Insert multiple issues
insertIssues :: V.Vector Issue -> DBT IO Int64
insertIssues = execute q . V.toList
  where
    q = [sql|
      INSERT INTO apis.issues (
        id, created_at, updated_at, project_id, issue_type, endpoint_hash,
        acknowledged_at, acknowledged_by, archived_at,
        title, service, critical, severity,
        affected_requests, affected_clients, error_rate,
        recommended_action, migration_complexity,
        issue_data, llm_enhanced_at, llm_enhancement_version
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |]

-- | Select issue by ID
selectIssueById :: IssueId -> DBT IO (Maybe Issue)
selectIssueById = selectOneByField [field| id |] . Only

-- | Select issues with filters
selectIssues :: Projects.ProjectId -> Maybe IssueType -> Maybe Bool -> Maybe Bool -> Int -> Int -> DBT IO (V.Vector IssueL)
selectIssues pid typeM isAcknowledged isArchived limit offset = query q params
  where
    q = [sql|
      SELECT 
        i.id, i.created_at, i.updated_at, i.project_id, i.issue_type, i.endpoint_hash,
        i.acknowledged_at, i.acknowledged_by, i.archived_at,
        i.title, i.service, i.critical, i.severity,
        i.affected_requests, i.affected_clients, i.error_rate,
        i.recommended_action, i.migration_complexity,
        i.issue_data, i.llm_enhanced_at, i.llm_enhancement_version,
        COALESCE(stats.event_count, 0),
        COALESCE(stats.last_seen, i.updated_at)
      FROM apis.issues i
      LEFT JOIN LATERAL (
        SELECT COUNT(*) as event_count, MAX(created_at) as last_seen
        FROM apis.request_dumps rd
        WHERE rd.project_id = i.project_id
          AND rd.created_at > CURRENT_TIMESTAMP - INTERVAL '14 days'
          AND (
            (i.issue_type = 'api_change' AND rd.endpoint_hash = i.endpoint_hash)
            OR (i.issue_type = 'runtime_exception' AND rd.errors @> i.issue_data->'error_hashes')
          )
      ) stats ON true
      WHERE i.project_id = ?
        AND ($2::text IS NULL OR i.issue_type = $2::apis.issue_type)
        AND ($3::boolean IS NULL OR (i.acknowledged_at IS NOT NULL) = $3)
        AND ($4::boolean IS NULL OR (i.archived_at IS NOT NULL) = $4)
      ORDER BY i.critical DESC, i.created_at DESC
      LIMIT ? OFFSET ?
    |]
    params = (pid, issueTypeToText <$> typeM, isAcknowledged, isArchived, limit, offset)

-- | Find open issue for endpoint
findOpenIssueForEndpoint :: Projects.ProjectId -> Text -> DBT IO (Maybe Issue)
findOpenIssueForEndpoint pid endpointHash = queryOne q (pid, APIChange, endpointHash)
  where
    q = [sql|
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
    q = [sql|
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
    q = [sql|
      UPDATE apis.issues
      SET 
        title = ?,
        recommended_action = ?,
        migration_complexity = ?,
        llm_enhanced_at = NOW(),
        llm_enhancement_version = 1
      WHERE id = ?
    |]
    params = (title, action, complexity, issueId)

-- | Acknowledge issue
acknowledgeIssue :: IssueId -> Users.UserId -> DBT IO ()
acknowledgeIssue issueId userId = void $ execute q (userId, issueId)
  where
    q = [sql|
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
      apiChangeData = APIChangeData
        { endpointMethod = fromMaybe "UNKNOWN" firstAnomaly.endpointMethod
        , endpointPath = fromMaybe "/" firstAnomaly.endpointUrlPath
        , endpointHost = "Unknown"
        , anomalyHashes = V.map (.targetHash) anomalies
        , shapeChanges = V.filter (\a -> a.anomalyType == Anomalies.ATShape) anomalies
        , formatChanges = V.filter (\a -> a.anomalyType == Anomalies.ATFormat) anomalies
        , newFields = V.concat $ V.map (.shapeNewUniqueFields) anomalies
        , deletedFields = V.concat $ V.map (.shapeDeletedFields) anomalies
        , modifiedFields = V.concat $ V.map (.shapeUpdatedFieldFormats) anomalies
        }
      
      breakingChanges = V.length apiChangeData.deletedFields + V.length apiChangeData.modifiedFields
      isCritical = breakingChanges > 0
  
  pure Issue
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
    , service = Anomalies.detectService Nothing (fromMaybe "/" firstAnomaly.endpointUrlPath)
    , critical = isCritical
    , severity = if isCritical then "critical" else "warning"
    , affectedRequests = 0
    , affectedClients = 0
    , errorRate = Nothing
    , recommendedAction = "Review the API changes and update your integration accordingly."
    , migrationComplexity = if breakingChanges > 5 then "high" else if breakingChanges > 0 then "medium" else "low"
    , issueData = Aeson $ AE.toJSON apiChangeData
    , llmEnhancedAt = Nothing
    , llmEnhancementVersion = Nothing
    }

-- | Create Runtime Exception issue
createRuntimeExceptionIssue :: Projects.ProjectId -> RequestDumps.ATError -> IO Issue
createRuntimeExceptionIssue projectId error = do
  issueId <- IssueId <$> UUID.nextRandom
  
  let exceptionData = RuntimeExceptionData
        { errorType = error.errorType
        , errorMessage = error.message
        , stackTrace = error.stackTrace
        , requestPath = error.requestPath
        , requestMethod = error.requestMethod
        , occurrenceCount = 1
        , firstSeen = zonedTimeToUTC error.when
        , lastSeen = zonedTimeToUTC error.when
        }
  
  pure Issue
    { id = issueId
    , createdAt = error.when
    , updatedAt = error.when
    , projectId = projectId
    , issueType = RuntimeException
    , endpointHash = ""
    , acknowledgedAt = Nothing
    , acknowledgedBy = Nothing
    , archivedAt = Nothing
    , title = error.rootErrorType <> ": " <> T.take 100 error.message
    , service = Anomalies.detectService Nothing (fromMaybe "/" error.requestPath)
    , critical = True
    , severity = "critical"
    , affectedRequests = 1
    , affectedClients = 0
    , errorRate = Nothing
    , recommendedAction = "Investigate the error and implement a fix."
    , migrationComplexity = "n/a"
    , issueData = Aeson $ AE.toJSON exceptionData
    , llmEnhancedAt = Nothing
    , llmEnhancementVersion = Nothing
    }

-- | Create Query Alert issue
createQueryAlertIssue :: Projects.ProjectId -> Text -> Text -> Text -> Double -> Double -> Text -> IO Issue
createQueryAlertIssue projectId queryId queryName queryExpr threshold actual thresholdType = do
  issueId <- IssueId <$> UUID.nextRandom
  now <- getCurrentTime
  zonedNow <- utcToLocalZonedTime now
  
  let alertData = QueryAlertData
        { queryId = queryId
        , queryName = queryName
        , queryExpression = queryExpr
        , thresholdValue = threshold
        , actualValue = actual
        , thresholdType = thresholdType
        , triggeredAt = now
        }
  
  pure Issue
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
    , llmEnhancedAt = Nothing
    , llmEnhancementVersion = Nothing
    }