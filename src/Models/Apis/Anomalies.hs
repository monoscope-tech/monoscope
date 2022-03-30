{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Anomalies
  ( selectAnomalies,
    AnomalyVM (..),
    AnomalyActions (..),
    AnomalyTypes (..),
    AnomalyId (..),
    selectOngoingAnomaliesForEndpoint,
    acknowlegeAnomaly,
    anomalyIdText,
    unAcknowlegeAnomaly,
    selectOngoingAnomalies,
  )
where

import Data.Default (Default, def)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (Only))
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Transact (DBT)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Optics.TH
import Relude hiding (id)
import Text.RawString.QQ (r)
import Web.HttpApiData (FromHttpApiData)

newtype AnomalyId = AnomalyId {unAnomalyId :: UUID.UUID}
  deriving stock (Generic, Show)
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
  deriving stock (Eq, Generic, Show)

instance Default AnomalyTypes where
  def = ATUnknown

instance ToField AnomalyTypes where
  toField ATUnknown = Escape "unknown"
  toField ATField = Escape "field"
  toField ATEndpoint = Escape "endpoint"
  toField ATShape = Escape "shape"
  toField ATFormat = Escape "format"

parseAnomalyTypes :: (Eq s, IsString s) => s -> Maybe AnomalyTypes
parseAnomalyTypes "unknown" = Just ATUnknown
parseAnomalyTypes "field" = Just ATField
parseAnomalyTypes "endpoint" = Just ATEndpoint
parseAnomalyTypes "shape" = Just ATShape
parseAnomalyTypes "format" = Just ATFormat
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
  deriving stock (Show)

instance Default AnomalyActions where
  def = AAUnknown

instance ToField AnomalyActions where
  toField AAUnknown = Escape "unknown"
  toField AACreated = Escape "created"

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

data AnomalyVM = AnomalyCM
  { id :: AnomalyId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    acknowlegedAt :: Maybe ZonedTime,
    acknowlegedBy :: Maybe Users.UserId,
    anomalyType :: AnomalyTypes,
    action :: AnomalyActions,
    targetId :: UUID.UUID,
    --
    shapeId :: Maybe Shapes.ShapeId,
    --
    fieldId :: Maybe Fields.FieldId,
    fieldKey :: Maybe Text,
    fieldKeyPathStr :: Maybe Text,
    fieldCategory :: Maybe Fields.FieldCategoryEnum,
    fieldFormat :: Maybe Text,
    --
    formatId :: Maybe Formats.FormatId,
    formatType :: Maybe Fields.FieldTypes, -- fieldFormat in the formats table
    formatExamples :: Maybe (Vector Text),
    --
    endpointId :: Maybe Endpoints.EndpointId,
    endpointMethod :: Maybe Text,
    endpointUrlPath :: Maybe Text,
    --
    archivedAt :: Maybe ZonedTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "anomalies_vm", PrimaryKey "id", FieldModifiers '[CamelToSnake]] AnomalyVM)

makeFieldLabelsNoPrefix ''AnomalyVM

acknowlegeAnomaly :: AnomalyId -> Users.UserId -> DBT IO ()
acknowlegeAnomaly aid uid = void $ execute Update q (uid, aid)
  where
    q = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=? |]

unAcknowlegeAnomaly :: AnomalyId -> DBT IO ()
unAcknowlegeAnomaly aid = void $ execute Update q (Only aid)
  where
    q = [sql| update apis.anomalies set acknowleged_by=null, acknowleged_at=null where id=? |]

archiveAnomaly :: AnomalyId -> Users.UserId -> DBT IO ()
archiveAnomaly aid uid = void $ execute Update q (uid, aid)
  where
    q = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=? |]

unArchiveAnomaly :: AnomalyId -> DBT IO ()
unArchiveAnomaly aid = void $ execute Update q (Only aid)
  where
    q = [sql| update apis.anomalies set acknowleged_by=null, acknowleged_at=null where id=? |]

selectAnomalies :: Projects.ProjectId -> DBT IO (Vector AnomalyVM)
selectAnomalies pid = selectManyByField @AnomalyVM [field| project_id |] pid

selectOngoingAnomalies :: Projects.ProjectId -> DBT IO (Vector AnomalyVM)
selectOngoingAnomalies pid = query Select q opt
  where
    q = _selectWhere @AnomalyVM [[field| project_id |]] <> [r|   AND "acknowleged_at" is null AND "archived_at" is null |]
    opt = Only pid

selectOngoingAnomaliesForEndpoint :: Projects.ProjectId -> Endpoints.EndpointId -> DBT IO (Vector AnomalyVM)
selectOngoingAnomaliesForEndpoint pid eid = query Select q opt
  where
    q = _selectWhere @AnomalyVM [[field| project_id |], [field| endpoint_id |]] <> [r|   AND "acknowleged_at" is null AND "archived_at" is null |]
    opt = (pid, eid)

-- selectOngoingAnomalies :: Projects.ProjectId -> DBT IO (Vector AnomalyVM)
-- selectOngoingAnomalies pid = do
--   -- FIXME: when this is vaguely tested to be fine, it should be converted into a materialized view
--   -- SHould be an auto updating view using timescale db functionality.
--   let q =
--         [sql|
-- 	SELECT
--     an.id, an.created_at, an.updated_at, an.project_id, an.acknowleged_at,an.acknowleged_by, an.anomaly_type, an.action, an.target_id,
--     to_jsonb(fields.*) field,
--     to_jsonb(shapes.*) shape,
--     to_jsonb(formats.*) format,
--     to_jsonb(endpoints.*) endpoint,
--     an.archived_at,
--     endpoints.id endpoint_id
-- 	FROM apis.anomalies an
-- 	LEFT JOIN apis.formats on target_id=formats.id
-- 	LEFT JOIN apis.fields on (fields.id=target_id OR fields.id=formats.field_id)
-- 	LEFT JOIN apis.shapes on target_id=shapes.id
-- 	LEFT JOIN apis.endpoints
--       ON (endpoints.id = target_id
--           OR endpoints.id = fields.endpoint_id
--           OR endpoints.id = shapes.endpoint_id
--           )
--   where an.project_id = ? and an.acknowleged_at is null and an.archived_at is null
--       |]
--   let options = Only pid
--   query Select q options

-- selectOngoingAnomaliesForEndpoint :: Projects.ProjectId -> Endpoints.EndpointId -> DBT IO (Vector AnomalyVM)
-- selectOngoingAnomaliesForEndpoint pid eid = do
--   -- FIXME: when this is vaguely tested to be fine, it should be converted into a materialized view
--   -- SHould be an auto updating view using timescale db functionality.
--   let q =
--         [sql|
-- 	SELECT
--     an.id, an.created_at, an.updated_at, an.project_id, an.acknowleged_at,an.acknowleged_by, an.anomaly_type, an.action, an.target_id,
--     to_jsonb(fields.*) field,
--     to_jsonb(shapes.*) shape,
--     to_jsonb(formats.*) format,
--     to_jsonb(endpoints.*) endpoint,
--     an.archived_at,
--     endpoints.id endpoint_id
-- 	FROM apis.anomalies an
-- 	LEFT JOIN apis.formats on target_id=formats.id
-- 	LEFT JOIN apis.fields on (fields.id=target_id OR fields.id=formats.field_id)
-- 	LEFT JOIN apis.shapes on target_id=shapes.id
-- 	LEFT JOIN apis.endpoints
--       ON (endpoints.id = target_id
--           OR endpoints.id = fields.endpoint_id
--           OR endpoints.id = shapes.endpoint_id
--           )
--   where an.project_id = ? and endpoints.id=? and an.acknowleged_at is null and an.archived_at is null
--       |]
--   let options = (pid, eid)
--   query Select q options
