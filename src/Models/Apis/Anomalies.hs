module Models.Apis.Anomalies (
  AnomalyVM (..),
  AnomalyActions (..),
  AnomalyTypes (..),
  AnomalyId,
  PayloadChange (..),
  ChangeType (..),
  FieldChange (..),
  FieldChangeKind (..),
  parseAnomalyTypes,
  detectService,
  getAnomaliesVM,
  acknowlegeCascade,
  acknowledgeAnomalies,
  archiveAnomaliesAndIssues,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default, def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Effectful (Eff, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Pkg.SchemaLearning.Catalog qualified as Fields (
  FieldCategoryEnum,
  FieldId,
  FieldTypes,
  FormatId,
  ShapeId,
 )
import Relude hiding (id)
import System.Types (DB)


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
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC ('Just "apis.anomaly_type") "AT" AnomalyTypes


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
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC ('Just "apis.anomaly_action") "AA" AnomalyActions


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
  , endpointServiceName :: Maybe Text
  , endpointEnvironment :: Maybe Text
  , endpointHost :: Maybe Text
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


-- | Read VM rows for the anomalies UI.
--
-- target_hash conventions (set by @Pkg.SchemaLearning.Worker@):
--   * endpoint → @keyHash@                              (== endpoints.hash for HTTP)
--   * shape    → @keyHash:s:<templateHash[:8]>@
--   * field    → @keyHash:f:<xxhash(fieldPath)[:8]>@
--   * format   → @keyHash:fmt:<xxhash(fieldPath)[:8]>@
--
-- Endpoint metadata joins via @endpoints.hash = split_part(target_hash,':',1)@.
-- Per-field detail (key path, format) is fetched lazily by the caller via
-- 'getAnomalyFieldDetail' — encoding the full path into target_hash would
-- blow the unique-index size budget, and Postgres has no built-in xxhash.
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
    NULL::uuid     shape_id,
    '{}'::TEXT[]   new_unique_fields,
    '{}'::TEXT[]   deleted_fields,
    '{}'::TEXT[]   updated_field_formats,
    NULL::uuid     field_id,
    NULL::text     field_key,
    NULL::text     field_key_path,
    NULL::text     field_category,
    NULL::text     field_format,
    NULL::uuid     format_id,
    NULL::text     format_type,
    '{}'::TEXT[]   format_examples,
    endpoints.id   endpoint_id,
    -- Prefer values stored directly on the anomaly row (populated by the
    -- schema-learning writer from the catalog 'Scope'); fall back to the
    -- 'apis.endpoints' join for legacy rows written before migration 0092.
    COALESCE(an.method, endpoints.method) endpoint_method,
    COALESCE(an.url_path, endpoints.url_path) endpoint_url_path,
    endpoints.service_name endpoint_service_name,
    endpoints.environment endpoint_environment,
    COALESCE(an.host, endpoints.host) endpoint_host,
    an.archived_at,
    COALESCE(iss.affected_requests, 0),#{now}::timestamptz
from
    apis.anomalies an
    LEFT JOIN apis.issues iss ON iss.target_hash = an.target_hash AND iss.project_id = an.project_id
    LEFT JOIN apis.endpoints ON an.project_id = endpoints.project_id
                            AND endpoints.hash = split_part(an.target_hash, ':', 1)
where
  an.project_id=#{pid} AND an.target_hash=ANY(#{hash})
      |]


-- | Acknowledge issues by id and cascade to anomalies referenced via @issue_data.anomaly_hashes@.
-- Returned target_hashes are passed to 'acknowlegeCascade' by the caller for an additional
-- LIKE-prefix sweep on the legacy hash space.
acknowledgeAnomalies :: (DB es, Time :> es) => Projects.UserId -> V.Vector Text -> Eff es [Text]
acknowledgeAnomalies uid issueIds
  | V.null issueIds = pure []
  | otherwise = do
      now <- Time.currentTime
      Hasql.interp
        [HI.sql| UPDATE apis.issues SET acknowledged_by=#{uid}, acknowledged_at=#{now}
                 WHERE id=ANY(#{issueIds}::uuid[]) RETURNING target_hash |]
        <* Hasql.interpExecute_
          [HI.sql| WITH related AS (
                     SELECT jsonb_array_elements_text(COALESCE(issue_data->'anomaly_hashes','[]'::jsonb)) AS h
                     FROM apis.issues WHERE id=ANY(#{issueIds}::uuid[])
                   )
                   UPDATE apis.anomalies a SET acknowledged_by=#{uid}, acknowledged_at=#{now}
                   WHERE a.target_hash IN (SELECT h FROM related) |]


acknowlegeCascade :: (DB es, Time :> es) => Projects.UserId -> V.Vector Text -> Eff es Int64
acknowlegeCascade uid targets
  | V.null targets = pure 0
  | otherwise = do
      now <- Time.currentTime
      let hashes = (<> "%") <$> targets
      Hasql.interpExecute_ [HI.sql| UPDATE apis.issues SET acknowledged_by = #{uid}, acknowledged_at = #{now} WHERE target_hash LIKE ANY(#{hashes}) |]
      Hasql.interpExecute [HI.sql| UPDATE apis.anomalies SET acknowledged_by = #{uid}, acknowledged_at = #{now} WHERE target_hash LIKE ANY(#{hashes}) |]


-- | Archive issues by id and cascade-archive their underlying anomalies via
-- the @issue_data.anomaly_hashes@ JSONB field. Returns the row count from the
-- issues update (the user-visible action target).
archiveAnomaliesAndIssues :: (DB es, Time :> es) => V.Vector Text -> Eff es Int64
archiveAnomaliesAndIssues issueIds
  | V.null issueIds = pure 0
  | otherwise = do
      now <- Time.currentTime
      Hasql.interpExecute [HI.sql| UPDATE apis.issues SET archived_at=#{now} WHERE id=ANY(#{issueIds}::uuid[]) |]
        <* Hasql.interpExecute_
          [HI.sql| WITH related AS (
                     SELECT jsonb_array_elements_text(COALESCE(issue_data->'anomaly_hashes','[]'::jsonb)) AS h
                     FROM apis.issues WHERE id=ANY(#{issueIds}::uuid[])
                   )
                   UPDATE apis.anomalies a SET archived_at=#{now}
                   WHERE a.target_hash IN (SELECT h FROM related) |]


-------------------------------------------------------------------------------------------
-- New Issues model implementations
--

-- Orphans: postgresql-simple's Aeson wrapper provides neither.
instance NFData a => NFData (Aeson a) where
  rnf (Aeson a) = rnf a


instance Default (Aeson [a]) where
  def = Aeson []


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


-- | Derive a service name from the path (@\/api\/v1\/auth\/login@ → @auth-service@), falling back to the host.
detectService :: Maybe Text -> Maybe Text -> Text
detectService hostM endpointPathM = case maybe [] (T.splitOn "/" . T.dropWhile (== '/')) endpointPathM of
  ("api" : _ : service : _) -> service <> "-service"
  (service : _) | service /= "" -> service <> "-service"
  _ -> fromMaybe "api-service" hostM
