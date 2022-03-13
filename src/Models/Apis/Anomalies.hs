{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Anomalies (selectAnomalies, AnomalyVM (..)) where

import Data.Aeson qualified as AE
import Data.Default (Default, def)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
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
import Web.HttpApiData (FromHttpApiData)

newtype AnomalyId = AnomalyId {unAnomalyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

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

data Anomaly = Anomaly
  { id :: AnomalyId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    acknowlegedAt :: Maybe ZonedTime,
    acknowlegedBy :: Maybe Users.UserId,
    anomalyType :: AnomalyTypes,
    action :: AnomalyActions,
    targetId :: UUID.UUID
  }

makeFieldLabelsNoPrefix ''Anomaly

data AnomalyData
  = ADEndpoint Endpoints.Endpoint
  | ADShape Shapes.Shape
  | ADField Fields.Field
  | ADFormat Formats.Format
  | ADUnknown AE.Value
  deriving stock (Show)

data AnomalyVM' = AnomalyVM'
  { id :: AnomalyId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    acknowlegedAt :: Maybe ZonedTime,
    acknowlegedBy :: Maybe Users.UserId,
    anomalyType :: AnomalyTypes,
    action :: AnomalyActions,
    targetId :: UUID.UUID,
    dataObject :: AE.Value,
    endpointId :: Endpoints.EndpointId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)

data AnomalyVM = AnomalyVM
  { id :: AnomalyId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    acknowlegedAt :: Maybe ZonedTime,
    acknowlegedBy :: Maybe Users.UserId,
    anomalyType :: AnomalyTypes,
    action :: AnomalyActions,
    targetId :: UUID.UUID,
    dataObject :: AnomalyData,
    endpointId :: Endpoints.EndpointId
  }
  deriving stock (Show, Generic)

makeFieldLabelsNoPrefix ''AnomalyVM

selectAnomalies :: Projects.ProjectId -> DBT IO (Vector AnomalyVM)
selectAnomalies pid = do
  -- FIXME: when this is vaguely tested to be fine, it should be converted into a materialized view
  -- SHould be an auto updating view using timescale db functionality.
  let q =
        [sql| 
	SELECT an.id, an.created_at, an.updated_at, an.project_id, an.acknowleged_at,an.acknowleged_by, an.anomaly_type, an.action, an.target_id,
	(CASE WHEN anomaly_type='field' THEN to_jsonb(fields.*) 
		 WHEN anomaly_type='shape' THEN to_jsonb(shapes.*) 
		 WHEN anomaly_type='format' THEN to_jsonb(formats.*) 
		 WHEN anomaly_type='endpoint' THEN to_jsonb(endpoints.*) 
	 END
	) data_object,
	(CASE WHEN anomaly_type='field' THEN fields.endpoint_id 
		 WHEN anomaly_type='shape' THEN shapes.endpoint_id
		 WHEN anomaly_type='format' THEN (select endpoint_id from apis.fields where id=formats.field_id )
		 WHEN anomaly_type='endpoint' THEN endpoints.id
	 END
	) endpoint_id
	FROM apis.anomaly an
	LEFT JOIN apis.fields on target_id=fields.id
	LEFT JOIN apis.shapes on target_id=shapes.id
	LEFT JOIN apis.formats on target_id=formats.id
	LEFT JOIN apis.endpoints on target_id=endpoints.id

  where an.project_id = ?
	;
      |]
  let options = Only pid
  resp <- query Select q options
  pure $ Vector.map normalizeAnomaly resp

normalizeAnomaly :: AnomalyVM' -> AnomalyVM
normalizeAnomaly AnomalyVM' {..} =
  let dataObject' = aesonToAnomalyData anomalyType dataObject
   in AnomalyVM {dataObject = dataObject', ..}

aesonToAnomalyData :: AnomalyTypes -> AE.Value -> AnomalyData
aesonToAnomalyData at val = case at of
  ATUnknown -> ADUnknown val
  ATEndpoint -> case AE.fromJSON val of
    AE.Error v -> ADUnknown $ AE.toJSON v
    AE.Success enp -> ADEndpoint enp
  ATField -> case AE.fromJSON val of
    AE.Error v -> ADUnknown $ AE.toJSON v
    AE.Success field -> ADField field
  ATShape -> case AE.fromJSON val of
    AE.Error v -> ADUnknown $ AE.toJSON v
    AE.Success shape -> ADShape shape
  ATFormat -> case AE.fromJSON val of
    AE.Error v -> ADUnknown $ AE.toJSON v
    AE.Success format -> ADEndpoint format
