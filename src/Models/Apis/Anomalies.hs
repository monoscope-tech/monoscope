module Models.Apis.Anomalies (
  AnomalyVM (..),
  AnomalyActions (..),
  Issue (..),
  IssueL (..),
  IssueEventAgg (..),
  AnomalyTypes (..),
  AnomalyId,
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
  insertErrorQueryAndParams,
  parseAnomalyTypes,
  detectService,
  parseAnomalyActions,
  getAnomaliesVM,
  errorsByHashes,
  countAnomalies,
  acknowlegeCascade,
  acknowledgeAnomalies,
  errorByHash,
)
where

import Data.Aeson qualified as AE
import Data.ByteString.Char8 qualified as BSC
import Data.Default (Default, def)
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Time (parseUTCTime)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL qualified as PG
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (
  FieldCategoryEnum,
  FieldId,
  FieldTypes,
  FormatId,
 )
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (id, many, some)
import Relude.Unsafe qualified as Unsafe
import Servant (FromHttpApiData (..))
import System.Types (DB)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Utils


type AnomalyId = UUIDId "anomaly"


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
    formatId :: Maybe Fields.FormatId
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


getAnomaliesVM :: DB es => Projects.ProjectId -> V.Vector Text -> Eff es [AnomalyVM]
getAnomaliesVM pid hash
  | V.null hash = pure []
  | otherwise = PG.query q (pid, hash)
  where
    q =
      [sql|
SELECT
    an.id,
    an.created_at,
    an.updated_at,
    an.project_id,
    an.acknowledged_at,
    an.acknowledged_by,
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
  ((an.anomaly_type = 'endpoint')
    OR (an.anomaly_type = 'shape' AND endpoints.project_id = an.project_id AND endpoints.created_at != an.created_at)
    OR (an.anomaly_type = 'format' AND fields.project_id = an.project_id AND fields.created_at != an.created_at)
    OR NOT (an.anomaly_type = ANY('{"endpoint","shape","field","format"}'::apis.anomaly_type[]))
  ) AND an.project_id=? AND an.target_hash=ANY(?)
      |]


countAnomalies :: DB es => Projects.ProjectId -> Text -> Eff es Int
countAnomalies pid report_type = do
  result <- PG.query (Query $ encodeUtf8 q) (Only pid)
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


acknowledgeAnomalies :: DB es => Users.UserId -> V.Vector Text -> Eff es [Text]
acknowledgeAnomalies uid aids
  | V.null aids = pure []
  | otherwise = do
      -- Get anomaly hashes from the issues being acknowledged
      anomalyHashesResult :: [Only (V.Vector Text)] <- PG.query qGetHashes (Only aids)
      let allAnomalyHashes = V.concat $ coerce @[Only (V.Vector Text)] @[V.Vector Text] anomalyHashesResult
      -- Update issues
      (_ :: [Only Text]) <- PG.query qIssues (uid, aids)
      -- Update anomalies - both directly referenced and those tracked by the issues
      (_ :: [Only Text]) <- PG.query q (uid, aids)
      -- Also update anomalies referenced by the issues' anomaly_hashes arrays
      unless (V.null allAnomalyHashes) $ do
        _ <- PG.execute qAnomaliesByHash (uid, allAnomalyHashes)
        pass
      coerce @[Only Text] @[Text] <$> PG.query q (uid, aids)
  where
    qGetHashes = [sql| SELECT anomaly_hashes FROM apis.issues WHERE id=ANY(?::uuid[]) |]
    qIssues = [sql| update apis.issues set acknowledged_by=?, acknowleged_at=NOW() where id=ANY(?::uuid[]) RETURNING target_hash; |]
    q = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=ANY(?::uuid[]) RETURNING target_hash; |]
    qAnomaliesByHash = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where target_hash=ANY(?) |]


acknowlegeCascade :: DB es => Users.UserId -> V.Vector Text -> Eff es Int64
acknowlegeCascade uid targets
  | V.null targets = pure 0
  | otherwise = do
      _ <- PG.execute qIssues (uid, hashes)
      PG.execute q (uid, hashes)
  where
    hashes = (<> "%") <$> targets
    qIssues = [sql| UPDATE apis.issues SET acknowledged_by = ?, acknowleged_at = NOW() WHERE target_hash=ANY (?); |]
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
  { id :: Fields.FormatId
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
  , affectedRequests :: Int
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
  , affectedRequests :: Int
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
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "errors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ATError)
  deriving (FromField, ToField) via Aeson ATError
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError


errorsByHashes :: DB es => Projects.ProjectId -> V.Vector Text -> Eff es [ATError]
errorsByHashes pid hashes
  | V.null hashes = pure []
  | otherwise = PG.query q (pid, hashes)
  where
    q =
      [sql| SELECT id, created_at, updated_at, project_id, hash, error_type, message, error_data, first_trace_id, recent_trace_id
            FROM apis.errors WHERE project_id=? AND hash=ANY(?); |]


errorByHash :: Projects.ProjectId -> Text -> DBT IO (Maybe ATError)
errorByHash pid hash = do
  results <- query q (pid, hash)
  case results of
    [err] -> return $ Just err
    _ -> return Nothing
  where
    q =
      [sql| SELECT id, created_at, updated_at, project_id, hash, error_type, message, error_data, first_trace_id, recent_trace_id
            FROM apis.errors WHERE project_id=? AND hash=?; |]


insertErrorQueryAndParams :: Projects.ProjectId -> RequestDumps.ATError -> (Query, [DBField])
insertErrorQueryAndParams pid err = (q, params)
  where
    q =
      [sql| insert into apis.errors (project_id, created_at, hash, error_type, message, error_data, first_trace_id, recent_trace_id) VALUES (?,?,?,?,?,?,?,?)
            ON CONFLICT (project_id, hash) DO UPDATE SET updated_at = EXCLUDED.created_at, recent_trace_id = EXCLUDED.recent_trace_id; |]
    params =
      [ MkDBField pid
      , MkDBField err.when
      , MkDBField $ Unsafe.fromJust err.hash -- Illegal should not happen
      , MkDBField err.errorType
      , MkDBField err.message
      , MkDBField err
      , MkDBField err.traceId
      , MkDBField err.traceId
      ]
