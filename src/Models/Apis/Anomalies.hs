module Models.Apis.Anomalies (
  AnomalyVM (..),
  errorByHash,
  insertIssues,
  AnomalyActions (..),
  Issue (..),
  IssueL (..),
  IssueEventAgg (..),
  AnomalyTypes (..),
  AnomalyId (..),
  IssuesData (..),
  ATError (..),
  NewEndpointIssue (..),
  NewFieldIssue (..),
  NewShapeIssue (..),
  NewFormatIssue (..),
  PayloadChange (..),
  ChangeType (..),
  FieldChange (..),
  FieldChangeKind (..),
  selectIssueByHash,
  bulkInsertErrors,
  insertErrorQueryAndParams,
  selectIssues,
  parseAnomalyTypes,
  convertAnomalyToIssue,
  detectService,
  getReportAnomalies,
  parseAnomalyActions,
  getAnomaliesVM,
  anomalyIdText,
  errorsByHashes,
  countAnomalies,
  parseAnomalyRawTypes,
  acknowlegeCascade,
  acknowledgeAnomalies,
  getShapeParentAnomaliesVM,
  getFormatParentAnomalyVM,
  findOpenIssueForEndpoint,
  updateIssueWithNewAnomaly,
  -- mergeIssues, -- DEPRECATED
  -- getEndpointFromIssueData, -- DEPRECATED
  -- getShapeChangesFromIssueData, -- DEPRECATED
  -- getFieldFromIssueData, -- DEPRECATED
  -- getErrorDataFromIssueData, -- DEPRECATED
  selectIssueById,
  -- updateIssueEnhancement, -- DEPRECATED
)
where

import Data.Aeson qualified as AE
import Data.ByteString.Char8 qualified as BSC
import Data.Default (Default, def)
import Data.Text qualified as T
import Data.Time
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (execute, query, queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Time (parseUTCTime)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (
  FieldCategoryEnum,
  FieldId,
  FieldTypes,
 )
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Relude hiding (id, many, some)
import Relude.Unsafe qualified as Unsafe
import Servant (FromHttpApiData (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Utils


newtype AnomalyId = AnomalyId {unAnomalyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)
  deriving newtype (Default, Eq, FromField, FromHttpApiData, Ord, ToField)


anomalyIdText :: AnomalyId -> Text
anomalyIdText = UUID.toText . unAnomalyId


data AnomalyTypes
  = ATUnknown
  | ATField
  | ATEndpoint
  | ATShape
  | ATFormat
  | ATRuntimeException
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "FT", DAE.CamelToSnake]] AnomalyTypes


instance Default AnomalyTypes where
  def = ATUnknown


anomalyTypesToText :: AnomalyTypes -> Text
anomalyTypesToText ATUnknown = "unknown"
anomalyTypesToText ATField = "field"
anomalyTypesToText ATEndpoint = "endpoint"
anomalyTypesToText ATShape = "shape"
anomalyTypesToText ATFormat = "format"
anomalyTypesToText ATRuntimeException = "runtime_exception"


instance ToField AnomalyTypes where
  toField = Escape . encodeUtf8 <$> anomalyTypesToText


parseAnomalyTypes :: (Eq s, IsString s) => s -> Maybe AnomalyTypes
parseAnomalyTypes "unknown" = Just ATUnknown
parseAnomalyTypes "field" = Just ATField
parseAnomalyTypes "endpoint" = Just ATEndpoint
parseAnomalyTypes "shape" = Just ATShape
parseAnomalyTypes "format" = Just ATFormat
parseAnomalyTypes "runtime_exception" = Just ATRuntimeException
parseAnomalyTypes _ = Nothing


parseAnomalyRawTypes :: Text -> AnomalyTypes
parseAnomalyRawTypes "ATField" = ATField
parseAnomalyRawTypes "ATEndpoint" = ATEndpoint
parseAnomalyRawTypes "ATShape" = ATShape
parseAnomalyRawTypes "ATFormat" = ATFormat
parseAnomalyRawTypes "ATRuntimeException" = ATRuntimeException
parseAnomalyRawTypes _ = ATUnknown


instance FromField AnomalyTypes where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseAnomalyTypes bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected 'anomaly_type' enum, got " <> decodeUtf8 bs <> " instead."


data AnomalyActions
  = AAUnknown
  | AACreated
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "FT", DAE.CamelToSnake]] AnomalyActions


instance Default AnomalyActions where
  def = AAUnknown


anomalyActionsToText :: AnomalyActions -> Text
anomalyActionsToText AAUnknown = "unknown"
anomalyActionsToText AACreated = "created"


instance ToField AnomalyActions where
  toField = Escape . encodeUtf8 <$> anomalyActionsToText


parseAnomalyActions :: (Eq s, IsString s) => s -> Maybe AnomalyActions
parseAnomalyActions "unknown" = Just AAUnknown
parseAnomalyActions "created" = Just AACreated
parseAnomalyActions _ = Nothing


instance FromField AnomalyActions where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseAnomalyActions bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected 'anomaly_actions' enum, got " <> decodeUtf8 bs <> " instead."


data AnomalyVM = AnomalyVM
  { id :: AnomalyId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , acknowlegedAt :: Maybe ZonedTime
  , acknowlegedBy :: Maybe Users.UserId
  , anomalyType :: AnomalyTypes
  , action :: AnomalyActions
  , targetHash :: Text
  , --
    shapeId :: Maybe Shapes.ShapeId
  , shapeNewUniqueFields :: V.Vector Text
  , shapeDeletedFields :: V.Vector Text
  , shapeUpdatedFieldFormats :: V.Vector Text
  , --
    fieldId :: Maybe Fields.FieldId
  , fieldKey :: Maybe Text
  , fieldKeyPath :: Maybe Text
  , fieldCategory :: Maybe Fields.FieldCategoryEnum
  , fieldFormat :: Maybe Text
  , --
    formatId :: Maybe Formats.FormatId
  , formatType :: Maybe Fields.FieldTypes -- fieldFormat in the formats table
  , formatExamples :: Maybe (V.Vector Text)
  , --
    endpointId :: Maybe Endpoints.EndpointId
  , endpointMethod :: Maybe Text
  , endpointUrlPath :: Maybe Text
  , --
    archivedAt :: Maybe ZonedTime
  , eventsCount14d :: Int
  , lastSeen :: ZonedTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "anomalies_vm", PrimaryKey "id", FieldModifiers '[CamelToSnake]] AnomalyVM)


getAnomaliesVM :: Projects.ProjectId -> V.Vector Text -> DBT IO (V.Vector AnomalyVM)
getAnomaliesVM pid hash
  | V.null hash = pure V.empty
  | otherwise = query q (pid, hash)
  where
    q =
      [sql|
SELECT
    an.id,
    an.created_at,
    an.updated_at,
    an.project_id,
    an.acknowleged_at,
    an.acknowleged_by,
    an.anomaly_type,
    an.action,
    an.target_hash,
    shapes.id shape_id,
    coalesce(shapes.new_unique_fields, '{}'::TEXT[]) new_unique_fields,
    coalesce(shapes.deleted_fields, '{}'::TEXT[]) deleted_fields,
    coalesce(shapes.updated_field_formats, '{}'::TEXT[]) updated_field_formats,
    fields.id field_id,
    fields.key field_key,
    fields.key_path field_key_path,
    fields.field_category field_category,
    fields.format field_format,
    formats.id format_id,
    formats.field_type format_type,
    formats.examples format_examples,
    endpoints.id endpoint_id,
    endpoints.method endpoint_method,
    endpoints.url_path endpoint_url_path,
    an.archived_at,
    0,now()
from
    apis.anomalies an
    LEFT JOIN apis.formats on (target_hash = formats.hash AND an.project_id = formats.project_id)
    LEFT JOIN apis.fields on (
        ((fields.hash = formats.field_hash ) AND an.project_id = fields.project_id)
        OR fields.hash = formats.field_hash
    )
    LEFT JOIN apis.shapes on (target_hash = shapes.hash AND an.project_id = shapes.project_id)
    LEFT JOIN apis.endpoints ON (starts_with(an.target_hash, endpoints.hash) AND an.project_id = endpoints.project_id)
where
  ((anomaly_type = 'endpoint')
    OR (anomaly_type = 'shape' AND endpoints.project_id = an.project_id AND endpoints.created_at != an.created_at)
    OR (anomaly_type = 'format' AND fields.project_id = an.project_id AND fields.created_at != an.created_at)
    OR NOT ( anomaly_type = ANY('{"endpoint","shape","field","format"}'::apis.anomaly_type[]))
  ) AND an.project_id=? AND an.target_hash=ANY(?)
      |]


getShapeParentAnomaliesVM :: Projects.ProjectId -> V.Vector Text -> DBT IO (V.Vector Text)
getShapeParentAnomaliesVM pid hashes
  | V.null hashes = pure V.empty
  | otherwise = query q (pid, hashes)
  where
    q =
      [sql|SELECT target_hash
           FROM apis.issues iss JOIN apis.anomalies aan ON iss.id = aan.id
           WHERE iss.project_id = ? AND ANY(?) LIKE iss.target_hash ||'%' AND iss.anomaly_type='endpoint' AND aan.acknowleged_at IS NULL
      |]


getFormatParentAnomalyVM :: Projects.ProjectId -> Text -> DBT IO Int
getFormatParentAnomalyVM pid hash = do
  result <- query q (pid, hash)
  case result of
    [Only countt] -> return countt
    v -> return $ length v
  where
    q =
      [sql|
              SELECT COUNT(*)
              FROM apis.issues iss
              JOIN apis.anomalies aan ON iss.id = aan.id
              WHERE iss.project_id = ? AND ? LIKE iss.target_hash ||'%' AND iss.anomaly_type != 'format' AND aan.acknowleged_at IS NULL
      |]


selectIssues :: Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Int -> Int -> DBT IO (V.Vector IssueL)
selectIssues pid endpointM isAcknowleged isArchived sortM limitM skipM = query (Query $ encodeUtf8 q) (MkDBField pid : paramList)
  where
    boolToNullSubQ a = if a then " not " else ""
    condlist =
      catMaybes
        [ (\a -> " acknowleged_at is" <> a <> " null ") . boolToNullSubQ <$> isAcknowleged
        , (\a -> " archived_at is" <> a <> " null ") . boolToNullSubQ <$> isArchived
        , "endpoint_id=?" <$ endpointM
        ]
    cond
      | null condlist = mempty
      | otherwise = "AND " <> mconcat (intersperse " AND " condlist)
    paramList = mapMaybe (MkDBField <$>) [endpointM]
    orderBy = case sortM of
      Nothing -> "created_at desc"
      Just "first_seen" -> "created_at desc"
      Just "events" -> "events desc"
      Just "last_seen" -> "last_seen desc"
      _ -> "created_at desc"

    limit = maybe "" (\x -> "limit " <> show x) limitM
    skip = "offset " <> show skipM <> " "

    -- Exclude endpoints from anomaly list
    q =
      [text|
SELECT id, created_at, updated_at, project_id, acknowleged_at, anomaly_type, target_hash, issue_data,
    endpoint_id, acknowleged_by, archived_at,
    CASE
      WHEN anomaly_type='shape' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND shape_hash=iss.target_hash AND created_at > current_timestamp - interval '14d' )
      -- Format requires a CONTAINS query which is not covered by the regular indexes. GIN index can't have created_at compound indexes, so its a slow query
      -- WHEN anomaly_type='format' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND iss.target_hash=ANY(format_hashes) AND created_at > current_timestamp - interval '14d' )
      WHEN anomaly_type='runtime_exception' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND errors @> ('[{"hash": "' || iss.target_hash || '"}]')::jsonb AND created_at > current_timestamp - interval '14d')
      ELSE (0, NOW()::TEXT)
    END as req_count,
    title, service, critical, breaking_changes, incremental_changes,
    affected_payloads, affected_clients, estimated_requests, migration_complexity, recommended_action,
    request_payloads, response_payloads, COALESCE(anomaly_hashes, '{}'), COALESCE(endpoint_hash, '')
    FROM apis.issues iss WHERE project_id = ? $cond
    AND anomaly_type!='endpoint' 
    ORDER BY $orderBy $skip $limit |]


selectIssueByHash :: Projects.ProjectId -> Text -> DBT IO (Maybe IssueL)
selectIssueByHash pid targetHash = queryOne q (pid, targetHash)
  where
    q =
      [sql|
SELECT id, created_at, updated_at, project_id, acknowleged_at, anomaly_type, target_hash, issue_data,
    endpoint_id, acknowleged_by, archived_at,
    CASE
      WHEN anomaly_type='endpoint' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND endpoint_hash=iss.target_hash AND created_at > current_timestamp - interval '14d' )
      WHEN anomaly_type='shape' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND shape_hash=iss.target_hash AND created_at > current_timestamp - interval '14d' )
      WHEN anomaly_type='format' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND iss.target_hash=ANY(format_hashes) AND created_at > current_timestamp - interval '14d' )
      WHEN anomaly_type='runtime_exception' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND errors @> ('[{"hash": "' || iss.target_hash || '"}]')::jsonb AND created_at > current_timestamp - interval '14d')
      ELSE (0::BIGINT, NOW()::TEXT)
    END as req_count,
    title, service, critical, breaking_changes, incremental_changes,
    affected_payloads, affected_clients, estimated_requests, migration_complexity, recommended_action,
    request_payloads, response_payloads, COALESCE(anomaly_hashes, '{}'), COALESCE(endpoint_hash, '')
    FROM apis.issues iss WHERE project_id = ? and target_hash=? LIMIT 1
    |]


getReportAnomalies :: Projects.ProjectId -> Text -> DBT IO (V.Vector IssueL)
getReportAnomalies pid report_type = query (Query $ encodeUtf8 q) pid
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text|
  SELECT id, created_at, updated_at, project_id, acknowleged_at, anomaly_type, target_hash, issue_data,
    endpoint_id, acknowleged_by, archived_at,
    CASE
      WHEN anomaly_type='endpoint' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND endpoint_hash=iss.target_hash AND created_at > current_timestamp - interval '14d' )
      WHEN anomaly_type='shape' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND shape_hash=iss.target_hash AND created_at > current_timestamp - interval '14d' )
      -- Format requires a CONTAINS query which is not covered by the regular indexes. GIN index can't have created_at compound indexes, so its a slow query
      -- WHEN anomaly_type='format' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND iss.target_hash=ANY(format_hashes) AND created_at > current_timestamp - interval '14d' )
      WHEN anomaly_type='runtime_exception' THEN (select (count(*), COALESCE(max(created_at), iss.created_at)) from apis.request_dumps where project_id=iss.project_id AND errors @> ('[{"hash": "' || iss.target_hash || '"}]')::jsonb AND created_at > current_timestamp - interval '14d')
      ELSE (0::BIGINT, NOW()::TEXT)
    END as req_count,
    title, service, critical, breaking_changes, incremental_changes,
    affected_payloads, affected_clients, estimated_requests, migration_complexity, recommended_action,
    request_payloads, response_payloads, COALESCE(anomaly_hashes, '{}'), COALESCE(endpoint_hash, '')
    FROM apis.issues iss WHERE project_id = ?  and created_at > current_timestamp - interval $report_interval
    ORDER BY req_count desc limit 20
        |]


countAnomalies :: Projects.ProjectId -> Text -> DBT IO Int
countAnomalies pid report_type = do
  result <- query (Query $ encodeUtf8 q) pid
  case result of
    [Only countt] -> return countt
    v -> return $ length v
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text|
      SELECT COUNT(*) as anomaly_count
      FROM apis.issues iss WHERE project_id = ?  and created_at > current_timestamp - interval $report_interval
     |]


acknowledgeAnomalies :: Users.UserId -> V.Vector Text -> DBT IO (V.Vector Text)
acknowledgeAnomalies uid aids
  | V.null aids = pure V.empty
  | otherwise = do
      -- Get anomaly hashes from the issues being acknowledged
      anomalyHashesResult <- query qGetHashes (Only aids) :: DBT IO (V.Vector (Only (V.Vector Text)))
      let allAnomalyHashes = V.concat $ V.toList $ V.map (\(Only hashes) -> hashes) anomalyHashesResult
      -- Update issues
      _ <- query qIssues (uid, aids) :: DBT IO (V.Vector Text)
      -- Update anomalies - both directly referenced and those tracked by the issues
      _ <- query q (uid, aids) :: DBT IO (V.Vector Text)
      -- Also update anomalies referenced by the issues' anomaly_hashes arrays
      unless (V.null allAnomalyHashes) $ do
        _ <- execute qAnomaliesByHash (uid, allAnomalyHashes)
        pass
      query q (uid, aids)
  where
    qGetHashes = [sql| SELECT anomaly_hashes FROM apis.issues WHERE id=ANY(?::uuid[]) |]
    qIssues = [sql| update apis.issues set acknowleged_by=?, acknowleged_at=NOW() where id=ANY(?::uuid[]) RETURNING target_hash; |]
    q = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=ANY(?::uuid[]) RETURNING target_hash; |]
    qAnomaliesByHash = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where target_hash=ANY(?) |]


acknowlegeCascade :: Users.UserId -> V.Vector Text -> DBT IO Int64
acknowlegeCascade uid targets
  | V.null targets = pure 0
  | otherwise = do
      _ <- execute qIssues (uid, hashes)
      execute q (uid, hashes)
  where
    hashes = (<> "%") <$> targets
    qIssues = [sql| UPDATE apis.issues SET acknowleged_by = ?, acknowleged_at = NOW() WHERE target_hash=ANY (?); |]
    q = [sql| UPDATE apis.anomalies SET acknowleged_by = ?, acknowleged_at = NOW() WHERE target_hash LIKE ANY (?); |]


-------------------------------------------------------------------------------------------
-- New Issues model implementations
--

data NewShapeIssue = NewShapeIssue
  { id :: Shapes.ShapeId
  , endpointId :: Endpoints.EndpointId
  , endpointMethod :: Text
  , endpointUrlPath :: Text
  , host :: Text
  , newUniqueFields :: V.Vector Text
  , deletedFields :: V.Vector Text
  , updatedFieldFormats :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewShapeIssue


data NewFieldIssue = NewFieldIssue
  { id :: Fields.FieldId
  , endpointId :: Endpoints.EndpointId
  , endpointMethod :: Text
  , endpointUrlPath :: Text
  , host :: Text
  , key :: Text
  , keyPath :: Text
  , fieldCategory :: Fields.FieldCategoryEnum
  , format :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewFieldIssue


data NewFormatIssue = NewFormatIssue
  { id :: Formats.FormatId
  , endpointId :: Endpoints.EndpointId
  , endpointMethod :: Text
  , endpointUrlPath :: Text
  , host :: Text
  , fieldKeyPath :: Text
  , formatType :: Fields.FieldTypes
  , fieldCategory :: Fields.FieldCategoryEnum
  , examples :: Maybe (V.Vector Text)
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewFormatIssue


data NewEndpointIssue = NewEndpointIssue
  { id :: Endpoints.EndpointId
  , endpointMethod :: Text
  , endpointUrlPath :: Text
  , host :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewEndpointIssue


data IssuesData
  = IDNewShapeIssue NewShapeIssue
  | IDNewFieldIssue NewFieldIssue
  | IDNewFormatIssue NewFormatIssue
  | IDNewEndpointIssue NewEndpointIssue
  | IDNewRuntimeExceptionIssue RequestDumps.ATError
  | IDEmpty
  deriving stock (Generic, Show)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson IssuesData
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] IssuesData


instance Default IssuesData where
  def = IDEmpty


data Issue = Issue
  { id :: AnomalyId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , acknowlegedAt :: Maybe ZonedTime
  , anomalyType :: AnomalyTypes
  , targetHash :: Text
  , issueData :: IssuesData
  , endpointId :: Maybe Endpoints.EndpointId
  , acknowlegedBy :: Maybe Users.UserId
  , archivedAt :: Maybe ZonedTime
  , -- Enhanced UI fields
    title :: Text
  , service :: Text
  , critical :: Bool
  , breakingChanges :: Int
  , incrementalChanges :: Int
  , affectedPayloads :: Int
  , affectedClients :: Int
  , estimatedRequests :: Text
  , migrationComplexity :: Text
  , recommendedAction :: Text
  , requestPayloads :: Aeson [PayloadChange]
  , responsePayloads :: Aeson [PayloadChange]
  , -- New fields for anomaly grouping
    anomalyHashes :: V.Vector Text
  , endpointHash :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "issues", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Issue)


data IssueEventAgg = IssueEventAgg
  { count :: Int
  , lastSeen :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


instance FromField IssueEventAgg where
  fromField f mdata = case mdata of
    Nothing -> returnError UnexpectedNull f ""
    Just bs -> case parseMaybe parseIssueEventAgg (BSC.unpack bs) of
      Nothing -> returnError ConversionFailed f "Failed to parse IssueEventAgg"
      Just result -> return result


type Parser = Parsec Void String


parseIssueEventAgg :: Parser IssueEventAgg
parseIssueEventAgg = do
  _ <- char '('
  cnt <- L.decimal
  _ <- char ','
  _ <- space
  str <- char '"' *> manyTill L.charLiteral (char '"')
  utcTime <- case parseUTCTime (encodeUtf8 str) of
    Left err -> fail err
    Right time -> return time
  _ <- char ')'
  pure $ IssueEventAgg cnt utcTime


data IssueL = IssueL
  { id :: AnomalyId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , acknowlegedAt :: Maybe ZonedTime
  , anomalyType :: AnomalyTypes
  , targetHash :: Text
  , issueData :: IssuesData
  , endpointId :: Maybe Endpoints.EndpointId
  , acknowlegedBy :: Maybe Users.UserId
  , archivedAt :: Maybe ZonedTime
  , eventsAgg :: IssueEventAgg
  , -- New fields for enhanced UI
    title :: Text
  , service :: Text
  , critical :: Bool
  , breakingChanges :: Int
  , incrementalChanges :: Int
  , affectedPayloads :: Int
  , affectedClients :: Int
  , estimatedRequests :: Text
  , migrationComplexity :: Text
  , recommendedAction :: Text
  , -- Payload changes data
    requestPayloads :: Aeson [PayloadChange]
  , responsePayloads :: Aeson [PayloadChange]
  , -- New fields for anomaly grouping
    anomalyHashes :: V.Vector Text
  , endpointHash :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData)


-- NFData instance for Aeson wrapper (postgresql-simple doesn't provide it)
instance NFData a => NFData (Aeson a) where
  rnf (Aeson a) = rnf a


-- Default instance for Aeson wrapper
instance Default (Aeson [a]) where
  def = Aeson []


-- Payload change data structures
data PayloadChange = PayloadChange
  { method :: Maybe Text
  , statusCode :: Maybe Int
  , statusText :: Maybe Text
  , contentType :: Text
  , changeType :: ChangeType
  , description :: Text
  , changes :: [FieldChange]
  , exampleBefore :: Text
  , exampleAfter :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)


data ChangeType = Breaking | Incremental | Safe
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)


data FieldChange = FieldChange
  { fieldName :: Text
  , changeKind :: FieldChangeKind
  , breaking :: Bool
  , path :: Text
  , changeDescription :: Text
  , oldType :: Maybe Text
  , newType :: Maybe Text
  , oldValue :: Maybe Text
  , newValue :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)


data FieldChangeKind = Modified | Added | Removed
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)


-- Helper function to create IssuesData based on AnomalyType
createIssueData :: Maybe Text -> AnomalyVM -> Maybe IssuesData
createIssueData hostM anomaly = case anomaly.anomalyType of
  ATShape ->
    IDNewShapeIssue
      <$> ( NewShapeIssue
              <$> anomaly.shapeId
              <*> anomaly.endpointId
              <*> anomaly.endpointMethod
              <*> anomaly.endpointUrlPath
              <*> pure (fromMaybe "" hostM)
              <*> pure anomaly.shapeNewUniqueFields
              <*> pure anomaly.shapeDeletedFields
              <*> pure anomaly.shapeUpdatedFieldFormats
          )
  ATField ->
    IDNewFieldIssue
      <$> ( NewFieldIssue
              <$> anomaly.fieldId
              <*> anomaly.endpointId
              <*> anomaly.endpointMethod
              <*> anomaly.endpointUrlPath
              <*> pure (fromMaybe "" hostM)
              <*> anomaly.fieldKey
              <*> anomaly.fieldKeyPath
              <*> anomaly.fieldCategory
              <*> anomaly.fieldFormat
          )
  ATFormat ->
    IDNewFormatIssue
      <$> ( NewFormatIssue
              <$> anomaly.formatId
              <*> anomaly.endpointId
              <*> anomaly.endpointMethod
              <*> anomaly.endpointUrlPath
              <*> pure (fromMaybe "" hostM)
              <*> anomaly.fieldKeyPath
              <*> anomaly.formatType
              <*> anomaly.fieldCategory
              <*> pure anomaly.formatExamples
          )
  ATEndpoint ->
    IDNewEndpointIssue
      <$> ( NewEndpointIssue
              <$> anomaly.endpointId
              <*> anomaly.endpointMethod
              <*> anomaly.endpointUrlPath
              <*> pure (fromMaybe "" hostM)
          )
  _ -> Nothing


-- Helper function to generate issue title based on anomaly type
generateIssueTitle :: AnomalyVM -> Text
generateIssueTitle anomaly = case anomaly.anomalyType of
  ATShape -> case (V.length anomaly.shapeNewUniqueFields, V.length anomaly.shapeDeletedFields, V.length anomaly.shapeUpdatedFieldFormats) of
    (n, 0, 0) -> "New fields detected in " <> fromMaybe "endpoint" anomaly.endpointUrlPath
    (0, d, 0) -> "Fields removed from " <> fromMaybe "endpoint" anomaly.endpointUrlPath
    (0, 0, u) -> "Field formats updated in " <> fromMaybe "endpoint" anomaly.endpointUrlPath
    _ -> "Schema changes detected in " <> fromMaybe "endpoint" anomaly.endpointUrlPath
  ATField -> "New field detected: " <> fromMaybe "" anomaly.fieldKey
  ATFormat -> "Field format changed: " <> fromMaybe "" anomaly.fieldKeyPath
  ATEndpoint -> "New endpoint detected: " <> fromMaybe "" anomaly.endpointMethod <> " " <> fromMaybe "" anomaly.endpointUrlPath
  ATRuntimeException -> "Runtime exception detected"
  _ -> "Unknown anomaly detected"


-- Helper function to detect service from endpoint path or host
detectService :: Maybe Text -> Maybe Text -> Text
detectService hostM endpointPathM =
  let pathSegments = maybe [] (T.splitOn "/" . T.dropWhile (== '/')) endpointPathM
      -- Try to detect service from common patterns like /api/v1/auth/login -> auth-service
      serviceFromPath = case pathSegments of
        ("api" : _ : service : _) -> service <> "-service"
        (service : _) | service /= "" -> service <> "-service"
        _ -> ""
   in if T.null serviceFromPath
        then fromMaybe "api-service" hostM
        else serviceFromPath


-- Helper function to assess if changes are critical
assessCriticality :: AnomalyVM -> Bool
assessCriticality anomaly = case anomaly.anomalyType of
  ATShape -> V.length anomaly.shapeDeletedFields > 0 || V.length anomaly.shapeUpdatedFieldFormats > 0
  ATFormat -> True -- Format changes are often breaking
  ATEndpoint -> False -- New endpoints are not critical
  ATRuntimeException -> True -- Runtime exceptions are critical
  _ -> False


-- Helper function to count breaking changes
countBreakingChanges :: AnomalyVM -> Int
countBreakingChanges anomaly = case anomaly.anomalyType of
  ATShape -> V.length anomaly.shapeDeletedFields + V.length anomaly.shapeUpdatedFieldFormats
  ATFormat -> 1
  _ -> 0


-- Helper function to count incremental changes
countIncrementalChanges :: AnomalyVM -> Int
countIncrementalChanges anomaly = case anomaly.anomalyType of
  ATShape -> V.length anomaly.shapeNewUniqueFields
  ATField -> 1
  ATEndpoint -> 1
  _ -> 0


-- Helper function to assess migration complexity
assessMigrationComplexity :: AnomalyVM -> Text
assessMigrationComplexity anomaly =
  let breakingCount = countBreakingChanges anomaly
   in if breakingCount == 0
        then "low"
        else
          if breakingCount <= 2
            then "medium"
            else "high"


-- Helper function to generate recommended action
generateRecommendedAction :: AnomalyVM -> Text
generateRecommendedAction anomaly = case anomaly.anomalyType of
  ATShape ->
    let hasBreaking = V.length anomaly.shapeDeletedFields > 0 || V.length anomaly.shapeUpdatedFieldFormats > 0
     in if hasBreaking
          then "Review breaking changes and implement backward compatibility or schedule client migration"
          else "Update documentation and notify clients of new optional fields"
  ATFormat -> "Validate data format compatibility and update validation rules"
  ATEndpoint -> "Document new endpoint and update API specifications"
  ATRuntimeException -> "Investigate error cause and implement fix"
  _ -> "Review changes and assess impact"


-- Helper function to generate example payloads (placeholder for now)
generateExamplePayloads :: AnomalyVM -> [PayloadChange]
generateExamplePayloads anomaly = case anomaly.anomalyType of
  ATShape ->
    let changes = generateFieldChanges anomaly
        hasBreaking = countBreakingChanges anomaly > 0
        description = generatePayloadChangeDescription anomaly
     in if not (null changes)
          then
            [ PayloadChange
                { method = anomaly.endpointMethod
                , statusCode = Nothing
                , statusText = Nothing
                , contentType = "application/json"
                , changeType = if hasBreaking then Breaking else Incremental
                , description = description
                , changes = changes
                , exampleBefore = "{}" -- TODO: Generate from actual data
                , exampleAfter = "{}" -- TODO: Generate from actual data
                }
            ]
          else []
  _ -> []


-- Helper function to generate descriptive text for payload changes
generatePayloadChangeDescription :: AnomalyVM -> Text
generatePayloadChangeDescription anomaly =
  let newCount = V.length anomaly.shapeNewUniqueFields
      deletedCount = V.length anomaly.shapeDeletedFields
      updatedCount = V.length anomaly.shapeUpdatedFieldFormats
      parts =
        catMaybes
          [ if newCount > 0 then Just (show newCount <> " new field" <> if newCount > 1 then "s" else "") else Nothing
          , if deletedCount > 0 then Just (show deletedCount <> " removed field" <> if deletedCount > 1 then "s" else "") else Nothing
          , if updatedCount > 0 then Just (show updatedCount <> " modified field" <> if updatedCount > 1 then "s" else "") else Nothing
          ]
   in if null parts
        then "No changes detected"
        else "Schema updated: " <> T.intercalate ", " parts


-- Helper function to generate field changes from anomaly
generateFieldChanges :: AnomalyVM -> [FieldChange]
generateFieldChanges anomaly =
  let newFields =
        V.toList anomaly.shapeNewUniqueFields <&> \fieldName ->
          FieldChange
            { fieldName = fieldName
            , changeKind = Added
            , breaking = False
            , path = fieldName
            , changeDescription = "New field added"
            , oldType = Nothing
            , newType = Just "unknown"
            , oldValue = Nothing
            , newValue = Nothing
            }
      deletedFields =
        V.toList anomaly.shapeDeletedFields <&> \fieldName ->
          FieldChange
            { fieldName = fieldName
            , changeKind = Removed
            , breaking = True
            , path = fieldName
            , changeDescription = "Field removed"
            , oldType = Just "unknown"
            , newType = Nothing
            , oldValue = Nothing
            , newValue = Nothing
            }
      updatedFields =
        V.toList anomaly.shapeUpdatedFieldFormats <&> \fieldName ->
          FieldChange
            { fieldName = fieldName
            , changeKind = Modified
            , breaking = True
            , path = fieldName
            , changeDescription = "Field format changed"
            , oldType = Just "unknown"
            , newType = Just "unknown"
            , oldValue = Nothing
            , newValue = Nothing
            }
   in newFields ++ deletedFields ++ updatedFields


-- Main conversion function
convertAnomalyToIssue :: Maybe Text -> AnomalyVM -> Maybe Issue
convertAnomalyToIssue hostM anomaly = do
  issueData <- createIssueData hostM anomaly
  let endpointHashValue = case anomaly.anomalyType of
        ATShape -> T.take 8 anomaly.targetHash
        ATFormat -> T.take 8 anomaly.targetHash
        ATEndpoint -> anomaly.targetHash
        _ -> ""
  return
    Issue
      { id = anomaly.id
      , createdAt = anomaly.createdAt
      , updatedAt = anomaly.updatedAt
      , projectId = anomaly.projectId
      , acknowlegedAt = anomaly.acknowlegedAt
      , anomalyType = anomaly.anomalyType
      , targetHash = anomaly.targetHash
      , issueData = issueData
      , endpointId = anomaly.endpointId
      , acknowlegedBy = anomaly.acknowlegedBy
      , archivedAt = anomaly.archivedAt
      , -- Enhanced fields
        title = generateIssueTitle anomaly
      , service = detectService hostM anomaly.endpointUrlPath
      , critical = assessCriticality anomaly
      , breakingChanges = countBreakingChanges anomaly
      , incrementalChanges = countIncrementalChanges anomaly
      , affectedPayloads = 0 -- TODO: Calculate from actual request data
      , affectedClients = 0 -- TODO: Calculate from actual client data
      , estimatedRequests = "N/A" -- TODO: Calculate from metrics
      , migrationComplexity = assessMigrationComplexity anomaly
      , recommendedAction = generateRecommendedAction anomaly
      , requestPayloads = Aeson $ generateExamplePayloads anomaly
      , responsePayloads = Aeson [] -- TODO: Generate response payloads
      -- New fields for anomaly grouping
      , anomalyHashes = V.singleton anomaly.targetHash
      , endpointHash = endpointHashValue
      }


newtype ErrorId = ErrorId {unErrorId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, NFData, Ord, ToField)


data ATError = ATError
  { id :: ErrorId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , hash :: Text
  , errorType :: Text
  , message :: Text
  , errorData :: RequestDumps.ATError
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "errors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ATError)
  deriving (FromField, ToField) via Aeson ATError
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError


errorByHash :: Text -> DBT IO (Maybe ATError)
errorByHash hash = selectOneByField [field| hash |] (Only hash)


errorsByHashes :: Projects.ProjectId -> V.Vector Text -> DBT IO (V.Vector ATError)
errorsByHashes pid hashes
  | V.null hashes = pure V.empty
  | otherwise = query q (pid, hashes)
  where
    q =
      [sql| SELECT id, created_at, updated_at, project_id, hash, error_type, message, error_data
            FROM apis.errors WHERE project_id=? AND hash=ANY(?); |]


insertErrorQueryAndParams :: Projects.ProjectId -> RequestDumps.ATError -> (Query, [DBField])
insertErrorQueryAndParams pid err = (q, params)
  where
    q =
      [sql| insert into apis.errors (project_id,created_at, hash, error_type, message, error_data) VALUES (?,?,?,?,?,?)
            ON CONFLICT (project_id, hash) DO NOTHING; |]
    params =
      [ MkDBField pid
      , MkDBField err.when
      , MkDBField $ Unsafe.fromJust err.hash -- Illegal should not happen
      , MkDBField err.errorType
      , MkDBField err.message
      , MkDBField err
      ]


bulkInsertErrors :: DB :> es => V.Vector RequestDumps.ATError -> Eff es ()
bulkInsertErrors errors = void $ dbtToEff $ executeMany q (V.toList rowsToInsert)
  where
    q =
      [sql| INSERT into apis.errors  (project_id,created_at, hash, error_type, message, error_data) 
            VALUES (?,?,?,?,?,?) ON CONFLICT DO NOTHING; |]
    rowsToInsert =
      errors <&> \err ->
        ( Unsafe.fromJust err.projectId
        , err.when
        , Unsafe.fromJust err.hash -- Nothing state should never happen
        , err.errorType
        , err.message
        , err
        )


insertIssues :: V.Vector Issue -> DBT IO Int64
insertIssues issues = executeMany q (V.toList issues)
  where
    q =
      [sql|insert into apis.issues (id, created_at, updated_at, project_id, acknowleged_at, anomaly_type, target_hash,
                      issue_data, endpoint_id, acknowleged_by, archived_at, title, service, critical, breaking_changes,
                      incremental_changes, affected_payloads, affected_clients, estimated_requests, migration_complexity,
                      recommended_action, request_payloads, response_payloads, anomaly_hashes, endpoint_hash) 
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                      ON CONFLICT (project_id, target_hash) DO NOTHING;
                      |]


-- Find an existing open issue for an endpoint
findOpenIssueForEndpoint :: Projects.ProjectId -> Text -> DBT IO (Maybe Issue)
findOpenIssueForEndpoint pid endpointHash = queryOne q (pid, endpointHash)
  where
    q =
      [sql|
      SELECT id, created_at, updated_at, project_id, acknowleged_at, anomaly_type, target_hash, issue_data,
        endpoint_id, acknowleged_by, archived_at, title, service, critical, breaking_changes, incremental_changes,
        affected_payloads, affected_clients, estimated_requests, migration_complexity, recommended_action,
        request_payloads, response_payloads, COALESCE(anomaly_hashes, '{}'), COALESCE(endpoint_hash, '')
      FROM apis.issues 
      WHERE project_id = ? 
        AND endpoint_hash = ? 
        AND acknowleged_at IS NULL 
        AND archived_at IS NULL
      LIMIT 1
    |]


-- Update existing issue with new anomaly data
updateIssueWithNewAnomaly :: Issue -> V.Vector Text -> [PayloadChange] -> [PayloadChange] -> DBT IO Int64
updateIssueWithNewAnomaly existingIssue newAnomalyHashes newRequestPayloads newResponsePayloads =
  execute q params
  where
    q =
      [sql|
      UPDATE apis.issues 
      SET 
        anomaly_hashes = array_cat(anomaly_hashes, ?),
        breaking_changes = breaking_changes + ?,
        incremental_changes = incremental_changes + ?,
        request_payloads = COALESCE(request_payloads, '[]'::jsonb) || ?::jsonb,
        response_payloads = COALESCE(response_payloads, '[]'::jsonb) || ?::jsonb,
        updated_at = NOW()
      WHERE id = ?
    |]
    newBreaking = length $ filter (\p -> p.changeType == Breaking) (newRequestPayloads ++ newResponsePayloads)
    newIncremental = length $ filter (\p -> p.changeType == Incremental || p.changeType == Safe) (newRequestPayloads ++ newResponsePayloads)
    params =
      ( newAnomalyHashes
      , newBreaking
      , newIncremental
      , Aeson newRequestPayloads
      , Aeson newResponsePayloads
      , existingIssue.id
      )


-- Merge multiple issues into one (for grouping logic)
-- DEPRECATED: These functions are for the old Issue model
-- mergeIssues :: V.Vector Issue -> Issue
-- mergeIssues issues = case V.uncons issues of
--   Nothing -> error "Cannot merge empty issues vector"
--   Just (firstIssue, rest) -> V.foldl' mergeTwo firstIssue rest
--   where
--     mergeTwo :: Issue -> Issue -> Issue
--     mergeTwo base new = base
--       { anomalyHashes = base.anomalyHashes <> new.anomalyHashes
--       , breakingChanges = base.breakingChanges + new.breakingChanges
--       , incrementalChanges = base.incrementalChanges + new.incrementalChanges
--       , affectedPayloads = base.affectedPayloads + new.affectedPayloads
--       , affectedClients = max base.affectedClients new.affectedClients
--       , requestPayloads = mergePayloadLists base.requestPayloads new.requestPayloads
--       , responsePayloads = mergePayloadLists base.responsePayloads new.responsePayloads
--       , updatedAt = if zonedTimeToUTC base.updatedAt > zonedTimeToUTC new.updatedAt then base.updatedAt else new.updatedAt
--       , critical = base.critical || new.critical
--       }
--
--     mergePayloadLists :: Aeson [PayloadChange] -> Aeson [PayloadChange] -> Aeson [PayloadChange]
--     mergePayloadLists (Aeson pl1) (Aeson pl2) = Aeson (pl1 ++ pl2)

-- DEPRECATED: Helper functions for extracting data from IssuesData (old model)
-- getEndpointFromIssueData :: IssuesData -> Maybe Text
-- getEndpointFromIssueData (IDNewEndpointIssue issue) = Just $ issue.endpointMethod <> " " <> issue.endpointUrlPath
-- getEndpointFromIssueData _ = Nothing

-- getShapeChangesFromIssueData :: IssuesData -> ([Text], [Text], [Text])
-- getShapeChangesFromIssueData (IDNewShapeIssue issue) =
--   (V.toList issue.newUniqueFields, V.toList issue.deletedFields, V.toList issue.updatedFieldFormats)
-- getShapeChangesFromIssueData _ = ([], [], [])

-- getFieldFromIssueData :: IssuesData -> Maybe Text
-- getFieldFromIssueData (IDNewFieldIssue issue) = Just issue.key
-- getFieldFromIssueData (IDNewFormatIssue issue) = Just issue.fieldKeyPath
-- getFieldFromIssueData _ = Nothing

-- getErrorDataFromIssueData :: IssuesData -> Maybe RequestDumps.ATError
-- getErrorDataFromIssueData (IDNewRuntimeExceptionIssue err) = Just err
-- getErrorDataFromIssueData _ = Nothing

-- Select issue by ID
selectIssueById :: AnomalyId -> DBT IO (Maybe Issue)
selectIssueById aid = queryOne q (Only aid)
  where
    q =
      [sql|
      SELECT id, created_at, updated_at, project_id, acknowleged_at, anomaly_type, target_hash, issue_data,
        endpoint_id, acknowleged_by, archived_at, title, service, critical, breaking_changes, incremental_changes,
        affected_payloads, affected_clients, estimated_requests, migration_complexity, recommended_action,
        request_payloads, response_payloads, COALESCE(anomaly_hashes, '{}'), COALESCE(endpoint_hash, '')
      FROM apis.issues 
      WHERE id = ?
      LIMIT 1
    |]

-- DEPRECATED: Update issue with LLM enhancement (old model)
-- updateIssueEnhancement :: AnomalyId -> Text -> Text -> Text -> DBT IO Int64
-- updateIssueEnhancement issueId title recommendedAction migrationComplexity =
--   execute q (title, recommendedAction, migrationComplexity, issueId)
--   where
--     q = [sql|
--       UPDATE apis.issues
--       SET title = ?,
--           recommended_action = ?,
--           migration_complexity = ?,
--           llm_enhanced_at = NOW(),
--           llm_enhancement_version = 1,
--           updated_at = NOW()
--       WHERE id = ?
--     |]
