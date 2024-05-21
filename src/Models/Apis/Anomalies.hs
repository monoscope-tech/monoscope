{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models.Apis.Anomalies (
  AnomalyVM (..),
  errorByHash,
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
  insertErrorQueryAndParams,
  selectIssues,
  parseAnomalyTypes,
  convertAnomalyToIssue,
  getReportAnomalies,
  parseAnomalyActions,
  getAnomalyVM,
  anomalyIdText,
  countAnomalies,
  parseAnomalyRawTypes,
  acknowlegeCascade,
  acknowledgeAnomalies,
  getShapeParentAnomalyVM,
  getFormatParentAnomalyVM,
)
where

import Data.Aeson qualified as AE
import Data.ByteString.Char8 qualified as BSC
import Data.Default (Default, def)
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName, field)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Time (parseUTCTime)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields ()
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
  deriving newtype (NFData, AE.FromJSON, AE.ToJSON)
  deriving
    (Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID


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
    (AE.ToJSON, AE.FromJSON)
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving
    (AE.ToJSON, AE.FromJSON)
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
  , shapeNewUniqueFields :: Vector Text
  , shapeDeletedFields :: Vector Text
  , shapeUpdatedFieldFormats :: Vector Text
  , --
    fieldId :: Maybe Fields.FieldId
  , fieldKey :: Maybe Text
  , fieldKeyPath :: Maybe Text
  , fieldCategory :: Maybe Fields.FieldCategoryEnum
  , fieldFormat :: Maybe Text
  , --
    formatId :: Maybe Formats.FormatId
  , formatType :: Maybe Fields.FieldTypes -- fieldFormat in the formats table
  , formatExamples :: Maybe (Vector Text)
  , --
    endpointId :: Maybe Endpoints.EndpointId
  , endpointMethod :: Maybe Text
  , endpointUrlPath :: Maybe Text
  , --
    archivedAt :: Maybe ZonedTime
  , eventsCount14d :: Int
  , lastSeen :: ZonedTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, Default, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "anomalies_vm", PrimaryKey "id", FieldModifiers '[CamelToSnake]] AnomalyVM)


getAnomalyVM :: Projects.ProjectId -> Text -> DBT IO (Maybe AnomalyVM)
getAnomalyVM pid hash = queryOne Select q (pid, hash)
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
  ) AND an.project_id=? AND an.target_hash=?
      |]


getShapeParentAnomalyVM :: Projects.ProjectId -> Text -> DBT IO Int
getShapeParentAnomalyVM pid hash = do
  result <- query Select q (pid, hash)
  case result of
    [Only countt] -> return countt
    v -> return $ length v
  where
    q =
      [sql|SELECT COUNT(*) 
           FROM apis.issues iss JOIN apis.anomalies aan ON iss.id = aan.id
           WHERE iss.project_id = ? AND ? LIKE iss.target_hash ||'%' AND iss.anomaly_type='endpoint' AND aan.acknowleged_at IS NULL
      |]


getFormatParentAnomalyVM :: Projects.ProjectId -> Text -> DBT IO Int
getFormatParentAnomalyVM pid hash = do
  result <- query Select q (pid, hash)
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


selectIssues :: Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Int -> Int -> DBT IO (Vector IssueL)
selectIssues pid endpointM isAcknowleged isArchived sortM limitM skipM = query Select (Query $ encodeUtf8 q) (MkDBField pid : paramList)
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
      ELSE (0, NOW()::TEXT)
    END as req_count
    FROM apis.issues iss WHERE project_id = ? $cond
    ORDER BY $orderBy $skip $limit |]


getReportAnomalies :: Projects.ProjectId -> Text -> DBT IO (Vector AnomalyVM)
getReportAnomalies pid report_type = query Select (Query $ encodeUtf8 q) pid
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text|
  SELECT avm.id, avm.created_at, avm.updated_at, avm.project_id, aan.acknowleged_at, 
         aan.acknowleged_by, avm.anomaly_type, avm.action, avm.target_hash,
         avm.shape_id, avm.new_unique_fields, avm.deleted_fields, avm.updated_field_formats, 
         avm.field_id, avm.field_key, avm.field_key_path, avm.field_category, avm.field_format, 
         avm.format_id, avm.format_type, avm.format_examples, 
         avm.endpoint_id, avm.endpoint_method, avm.endpoint_url_path, aan.archived_at,
         count(rd.id) events, max(rd.created_at) last_seen
      FROM apis.anomalies_vm avm
      JOIN apis.anomalies aan ON avm.id = aan.id
      JOIN apis.request_dumps rd ON avm.project_id=rd.project_id 
          AND (avm.target_hash=ANY(rd.format_hashes) AND avm.anomaly_type='format')
          OR  (avm.target_hash=rd.shape_hash AND avm.anomaly_type='shape')
          OR  (avm.target_hash=rd.endpoint_hash AND avm.anomaly_type='endpoint')
      WHERE
          avm.project_id = ? 
          AND avm.anomaly_type != 'field'
          AND aan.created_at > NOW() - interval $report_interval
          AND aan.acknowleged_at IS NULL
          AND aan.archived_at IS NULL
      GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25
      HAVING count(rd.id) > 5 limit 11;
        |]


countAnomalies :: Projects.ProjectId -> Text -> DBT IO Int
countAnomalies pid report_type = do
  result <- query Select (Query $ encodeUtf8 q) pid
  case result of
    [Only countt] -> return countt
    v -> return $ length v
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text|
      SELECT COUNT(*) as anomaly_count
      FROM apis.anomalies_vm avm
      JOIN apis.anomalies aan ON avm.id = aan.id
      JOIN apis.request_dumps rd ON avm.project_id = rd.project_id 
          AND (avm.target_hash = ANY(rd.format_hashes) AND avm.anomaly_type = 'format')
          OR (avm.target_hash = rd.shape_hash AND avm.anomaly_type = 'shape')
          OR (avm.target_hash = rd.endpoint_hash AND avm.anomaly_type = 'endpoint')
      WHERE avm.project_id = ? 
          AND avm.anomaly_type != 'field'
          AND rd.created_at > NOW() - interval $report_interval
          AND aan.acknowleged_at IS NULL
          AND aan.archived_at IS NULL
      GROUP BY avm.id -- Include the columns that define an anomaly
      HAVING COUNT(rd.id) > 5;
     |]


acknowledgeAnomalies :: Users.UserId -> Vector Text -> DBT IO (Vector Text)
acknowledgeAnomalies uid aids = do
  _ <- query Update qIssues (uid, aids) :: DBT IO (Vector Text)
  query Update q (uid, aids)
  where
    qIssues = [sql| update apis.issues set acknowleged_by=?, acknowleged_at=NOW() where id=ANY(?::uuid[]) RETURNING target_hash; |]
    q = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=ANY(?::uuid[]) RETURNING target_hash; |]


acknowlegeCascade :: Users.UserId -> Vector Text -> DBT IO Int64
acknowlegeCascade uid targets = do
  _ <- execute Update qIssues (uid, hashes)
  execute Update q (uid, hashes)
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
  , newUniqueFields :: Vector Text
  , deletedFields :: Vector Text
  , updatedFieldFormats :: Vector Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving
    (AE.ToJSON, AE.FromJSON)
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
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving
    (AE.ToJSON, AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewFieldIssue


data NewFormatIssue = NewFormatIssue
  { id :: Formats.FormatId
  , endpointId :: Endpoints.EndpointId
  , endpointMethod :: Text
  , endpointUrlPath :: Text
  , host :: Text
  , fieldKeyPath :: Text
  , formatType :: Maybe Fields.FieldTypes
  , examples :: Maybe (Vector Text)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving
    (AE.ToJSON, AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewFormatIssue


data NewEndpointIssue = NewEndpointIssue
  { id :: Endpoints.EndpointId
  , endpointMethod :: Text
  , endpointUrlPath :: Text
  , host :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving
    (AE.ToJSON, AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] NewEndpointIssue


data IssuesData
  = IDNewShapeIssue NewShapeIssue
  | IDNewFieldIssue NewFieldIssue
  | IDNewFormatIssue NewFormatIssue
  | IDNewEndpointIssue NewEndpointIssue
  | IDNewRuntimeExceptionIssue RequestDumps.ATError
  | IDEmpty
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson IssuesData
  deriving
    (AE.ToJSON, AE.FromJSON)
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "issues", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Issue)


data IssueEventAgg = IssueEventAgg
  { count :: Int
  , lastSeen :: UTCTime
  }
  deriving stock (Show, Generic)
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, NFData)


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
              <*> pure (anomaly.shapeNewUniqueFields)
              <*> pure (anomaly.shapeDeletedFields)
              <*> pure (anomaly.shapeUpdatedFieldFormats)
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
              <*> pure anomaly.formatType
              <*> pure (anomaly.formatExamples)
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


-- Main conversion function
convertAnomalyToIssue :: Maybe Text -> AnomalyVM -> Maybe Issue
convertAnomalyToIssue hostM anomaly = do
  issueData <- createIssueData hostM anomaly
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
      }


newtype ErrorId = ErrorId {unErrorId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, FromField, ToField, FromHttpApiData, Default, AE.FromJSON, NFData, AE.ToJSON)


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
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData, Default)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "errors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ATError)
  deriving (FromField) via Aeson ATError


errorByHash :: Text -> DBT IO (Maybe ATError)
errorByHash hash = selectOneByField [field| hash |] (Only hash)


insertErrorQueryAndParams :: Projects.ProjectId -> RequestDumps.ATError -> (Query, [DBField])
insertErrorQueryAndParams pid err = (q, params)
  where
    q =
      [sql| insert into apis.errors (project_id,created_at, hash, error_type, message, error_data) VALUES (?,?,?,?,?,?)
            ON CONFLICT DO NOTHING |]
    params =
      [ MkDBField pid
      , MkDBField err.when
      , MkDBField $ Unsafe.fromJust err.hash -- Illegal should not happen
      , MkDBField err.errorType
      , MkDBField err.message
      , MkDBField err
      ]
