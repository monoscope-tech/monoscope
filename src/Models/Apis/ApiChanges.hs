-- | The API-change detector: rows in @apis.anomalies@ that the schema-learning
-- worker writes when an endpoint, shape, field, or format drifts. They are
-- evidence, not a triage object — every one of them rolls up into an
-- @api_change@ 'Models.Apis.Issues.Issue', which is what a user acts on.
--
-- Lifecycle state (acknowledged\/archived) lives on the issue alone; it used to
-- be mirrored onto these rows and kept in sync by a prefix sweep over 6.3M rows
-- that nothing ever read back.
module Models.Apis.ApiChanges (
  AnomalyVM (..),
  SchemaSubject (..),
  schemaSubject,
  spanSchemaTitle,
  FieldEvidence (..),
  FieldSnapshot (..),
  fieldSnapshot,
  AnomalyActions (..),
  AnomalyTypes (..),
  AnomalyId,
  PayloadChange (..),
  ChangeType (..),
  FieldChange (..),
  FieldChangeKind (..),
  parseAnomalyTypes,
  getAnomaliesVM,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default, def)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Text.Display (Display)
import Data.Time
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Relude hiding (id)
import System.Types (DB)
import Web.HttpApiData (FromHttpApiData (..))


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
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC ('Just "apis.anomaly_type") "AT" AnomalyTypes


-- | The stored spelling back into the enum — the exact inverse of the @WrappedEnumSC@
-- encoding above, so a new constructor cannot be added without this following it.
--
-- >>> parseAnomalyTypes "runtime_exception"
-- Just ATRuntimeException
-- >>> parseAnomalyTypes "not_a_type"
-- Nothing
parseAnomalyTypes :: Text -> Maybe AnomalyTypes
parseAnomalyTypes = rightToMaybe . parseUrlPiece


data AnomalyActions
  = AAUnknown
  | AACreated
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC ('Just "apis.anomaly_action") "AA" AnomalyActions


-- | The columns of an anomaly row the API-change rollup reads. Field drift
-- ('newFields', 'modifiedFields') is only carried by shape anomalies.
data AnomalyVM = AnomalyVM
  { createdAt :: ZonedTime
  , anomalyType :: AnomalyTypes
  , targetHash :: Text
  , endpointMethod :: Maybe Text
  , endpointUrlPath :: Maybe Text
  , endpointHost :: Maybe Text
  , serviceName :: Maybe Text
  , keyKind :: Maybe Catalog.KeyKind
  , scope :: Maybe (Aeson Catalog.Scope)
  , newFields :: V.Vector Text
  , modifiedFields :: V.Vector Text
  , fieldEvidence :: Aeson (V.Vector FieldEvidence)
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | HTTP and non-HTTP schema observations have different telemetry identities.
data SchemaSubject = EndpointSchema Catalog.Scope | SpanSchema Catalog.Scope
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)


schemaSubject :: Catalog.KeyKind -> Catalog.Scope -> SchemaSubject
schemaSubject Catalog.HttpEndpoint = EndpointSchema
schemaSubject Catalog.SpanIdentity = SpanSchema


spanSchemaTitle :: Catalog.Scope -> Text
spanSchemaTitle scope = "Span schema changed" <> foldMap (": " <>) scope.spanName


-- | Immutable observations at detection time; absence of a prior snapshot means a new field.
data FieldEvidence = FieldEvidence
  { path :: !Text
  , before :: !(Maybe FieldSnapshot)
  , after :: !FieldSnapshot
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)


data FieldSnapshot = FieldSnapshot
  { structure :: !Catalog.FieldStruct
  , examples :: !(V.Vector AE.Value)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData)


fieldSnapshot :: Text -> Catalog.CatalogEntry -> Maybe FieldSnapshot
fieldSnapshot path entry = (\structure -> FieldSnapshot structure (maybe V.empty (.values) $ HM.lookup path entry.valuesDelta)) <$> HM.lookup path entry.template.fields


-- | Read VM rows for the anomalies UI.
--
-- target_hash conventions (set by @Pkg.SchemaLearning.Worker@):
--   * endpoint → @keyHash@                              (== endpoints.hash for HTTP)
--   * shape    → @keyHash:s:<templateHash[:8]>@
--   * field    → @keyHash:f:<xxhash(fieldPath)[:8]>@
--   * format   → @keyHash:fmt:<xxhash(fieldPath)[:8]>@
--
-- Endpoint metadata joins via @endpoints.hash = split_part(target_hash,':',1)@.
getAnomaliesVM :: DB es => Projects.ProjectId -> V.Vector Text -> Eff es [AnomalyVM]
getAnomaliesVM pid hash
  | V.null hash = pure []
  | otherwise =
      Hasql.interp
        [HI.sql|
SELECT
    an.created_at,
    an.anomaly_type,
    an.target_hash,
    -- Prefer values stored directly on the anomaly row (populated by the
    -- schema-learning writer from the catalog 'Scope'); fall back to the
    -- 'apis.endpoints' join for legacy rows written before migration 0092.
    COALESCE(an.method, endpoints.method),
    COALESCE(an.url_path, endpoints.url_path),
    COALESCE(an.host, endpoints.host),
    COALESCE(NULLIF(btrim(c.scope->>'service'), ''), NULLIF(btrim(endpoints.service_name), '')),
    c.key_kind,
    c.scope,
    an.new_fields,
    an.modified_fields,
    an.field_evidence
from
    apis.anomalies an
    LEFT JOIN apis.endpoints ON an.project_id = endpoints.project_id
                            AND endpoints.hash = split_part(an.target_hash, ':', 1)
    LEFT JOIN apis.schema_catalog c ON c.project_id = an.project_id
                                  AND c.key_hash = split_part(an.target_hash, ':', 1)
where
  an.project_id=#{pid} AND an.target_hash=ANY(#{hash})
      |]


-- Orphans: postgresql-simple's Aeson wrapper provides neither.
deriving newtype instance NFData a => NFData (Aeson a)


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


-- $setup
-- >>> :set -XOverloadedStrings
