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
  parseAnomalyTypes,
  detectService,
  getAnomaliesVM,
  acknowlegeCascade,
  acknowledgeAnomalies,
)
where

import Data.Aeson qualified as AE
import Data.ByteString.Char8 qualified as BSC
import Data.Default (Default, def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.Time (parseUTCTime)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Fields qualified as Fields (
  FieldCategoryEnum,
  FieldId,
  FieldTypes,
  FormatId,
  ShapeId,
 )
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude hiding (id, many, some)
import Servant (FromHttpApiData (..))
import System.Types (DB)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


type AnomalyId = UUIDId "anomaly"


data AnomalyTypes
  = ATUnknown
  | ATField
  | ATEndpoint
  | ATShape
  | ATFormat
  | ATRuntimeException
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "FT", DAE.CamelToSnake]] AnomalyTypes
  deriving (Display, FromField, ToField) via WrappedEnumSC "AT" AnomalyTypes
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "AT" AnomalyTypes


parseAnomalyTypes :: (Eq s, IsString s) => s -> Maybe AnomalyTypes
parseAnomalyTypes "unknown" = Just ATUnknown
parseAnomalyTypes "field" = Just ATField
parseAnomalyTypes "endpoint" = Just ATEndpoint
parseAnomalyTypes "shape" = Just ATShape
parseAnomalyTypes "format" = Just ATFormat
parseAnomalyTypes "runtime_exception" = Just ATRuntimeException
parseAnomalyTypes _ = Nothing


data AnomalyActions
  = AAUnknown
  | AACreated
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "FT", DAE.CamelToSnake]] AnomalyActions
  deriving (Display, FromField, ToField) via WrappedEnumSC "AA" AnomalyActions
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "AA" AnomalyActions


data AnomalyVM = AnomalyVM
  { id :: AnomalyId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , acknowlegedAt :: Maybe ZonedTime
  , acknowlegedBy :: Maybe Projects.UserId
  , anomalyType :: AnomalyTypes
  , action :: AnomalyActions
  , targetHash :: Text
  , --
    shapeId :: Maybe Fields.ShapeId
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
  deriving anyclass (Default, FromRow, HI.DecodeRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "anomalies_vm", PrimaryKey "id", FieldModifiers '[CamelToSnake]] AnomalyVM)


getAnomaliesVM :: (DB es, Time :> es) => Projects.ProjectId -> V.Vector Text -> Eff es [AnomalyVM]
getAnomaliesVM pid hash
  | V.null hash = pure []
  | otherwise = do
      now <- Time.currentTime
      Hasql.interp
        [HI.sql|
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
    COALESCE(iss.affected_requests, 0),#{now}::timestamptz
from
    apis.anomalies an
    LEFT JOIN apis.issues iss ON iss.target_hash = an.target_hash AND iss.project_id = an.project_id
    LEFT JOIN apis.formats on (an.target_hash = formats.hash AND an.project_id = formats.project_id)
    LEFT JOIN apis.fields on (
        ((fields.hash = formats.field_hash ) AND an.project_id = fields.project_id)
        OR fields.hash = formats.field_hash
    )
    LEFT JOIN apis.shapes on (an.target_hash = shapes.hash AND an.project_id = shapes.project_id)
    LEFT JOIN apis.endpoints ON (starts_with(an.target_hash, endpoints.hash) AND an.project_id = endpoints.project_id)
where
  ((an.anomaly_type = 'endpoint')
    OR (an.anomaly_type = 'shape' AND endpoints.project_id = an.project_id AND endpoints.created_at != an.created_at)
    OR (an.anomaly_type = 'format' AND fields.project_id = an.project_id AND fields.created_at != an.created_at)
    OR NOT (an.anomaly_type = ANY('{"endpoint","shape","field","format"}'::apis.anomaly_type[]))
  ) AND an.project_id=#{pid} AND an.target_hash=ANY(#{hash})
      |]


acknowledgeAnomalies :: (DB es, Time :> es) => Projects.UserId -> V.Vector Text -> Eff es [Text]
acknowledgeAnomalies uid aids
  | V.null aids = pure []
  | otherwise = do
      now <- Time.currentTime
      -- Get anomaly hashes from the issues being acknowledged
      anomalyHashesResult :: [V.Vector Text] <-
        Hasql.interp
          [HI.sql| SELECT anomaly_hashes FROM apis.issues WHERE id=ANY(#{aids}::uuid[]) |]
      let allAnomalyHashes = V.concat anomalyHashesResult
      -- Update issues
      (_ :: [Text]) <-
        Hasql.interp
          [HI.sql| UPDATE apis.issues SET acknowledged_by=#{uid}, acknowledged_at=#{now} WHERE id=ANY(#{aids}::uuid[]) RETURNING target_hash |]
      -- Also update anomalies referenced by the issues' anomaly_hashes arrays
      unless (V.null allAnomalyHashes)
        $ Hasql.interpExecute_
          [HI.sql| UPDATE apis.anomalies SET acknowledged_by=#{uid}, acknowledged_at=#{now} WHERE target_hash=ANY(#{allAnomalyHashes}) |]
      -- Update anomalies - both directly referenced and those tracked by the issues
      Hasql.interp
        [HI.sql| UPDATE apis.anomalies SET acknowledged_by=#{uid}, acknowledged_at=#{now} WHERE id=ANY(#{aids}::uuid[]) RETURNING target_hash |]


acknowlegeCascade :: (DB es, Time :> es) => Projects.UserId -> V.Vector Text -> Eff es Int64
acknowlegeCascade uid targets
  | V.null targets = pure 0
  | otherwise = do
      now <- Time.currentTime
      let hashes = (<> "%") <$> targets
      _ <- Hasql.interpExecute [HI.sql| UPDATE apis.issues SET acknowledged_by = #{uid}, acknowledged_at = #{now} WHERE target_hash=ANY(#{hashes}) |]
      Hasql.interpExecute [HI.sql| UPDATE apis.anomalies SET acknowledged_by = #{uid}, acknowledged_at = #{now} WHERE target_hash LIKE ANY(#{hashes}) |]


-------------------------------------------------------------------------------------------
-- New Issues model implementations
--

data NewShapeIssue = NewShapeIssue
  { id :: Fields.ShapeId
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
  | IDNewRuntimeExceptionIssue ErrorPatterns.ATError
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
  , acknowlegedBy :: Maybe Projects.UserId
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
      Just result -> pure result


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
    Right time -> pure time
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
  , acknowlegedBy :: Maybe Projects.UserId
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
  , errorData :: ErrorPatterns.ATError
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "error_patterns", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ATError)
  deriving (FromField, ToField) via Aeson ATError
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError
