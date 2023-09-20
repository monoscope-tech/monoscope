{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Anomalies (
  selectAnomalies,
  AnomalyVM (..),
  AnomalyActions (..),
  AnomalyTypes (..),
  AnomalyId (..),
  parseAnomalyTypes,
  getReportAnomalies,
  parseAnomalyActions,
  getAnomalyVM,
  anomalyIdText,
  countAnomalies,
) where

import Data.Aeson qualified as AE
import Data.Default (Default, def)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (Only))
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (
  FieldCategoryEnum,
  FieldId,
  FieldTypes,
 )
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Optics.TH
import Relude hiding (id)
import Utils
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

instance ToField AnomalyTypes where
  toField = Escape . encodeUtf8 <$> anomalyTypesToText

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
  deriving stock (Show, Eq, Generic)
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
  deriving anyclass (FromRow, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "anomalies_vm", PrimaryKey "id", FieldModifiers '[CamelToSnake]] AnomalyVM)

makeFieldLabelsNoPrefix ''AnomalyVM

getAnomalyVM :: Projects.ProjectId -> Text -> DBT IO (Maybe AnomalyVM)
getAnomalyVM pid hash = queryOne Select q (pid, hash)
  where
    q = [sql| SELECT *,0,now() FROM apis.anomalies_vm WHERE project_id=? AND target_hash=?|]

selectAnomalies :: Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Int -> DBT IO (Vector AnomalyVM)
selectAnomalies pid endpointM isAcknowleged isArchived sortM limitM = query Select (Query $ encodeUtf8 q) (MkDBField pid : paramList)
  where
    boolToNullSubQ a = if a then " not " else ""
    condlist =
      catMaybes
        [ (\a -> " aan.acknowleged_at is" <> a <> " null ") <$> (boolToNullSubQ <$> isAcknowleged)
        , (\a -> " aan.archived_at is" <> a <> " null ") <$> (boolToNullSubQ <$> isArchived)
        , "endpoint_id=?" <$ endpointM
        ]
    cond
      | null condlist = mempty
      | otherwise = "AND " <> mconcat (intersperse " AND " condlist)
    paramList = mapMaybe (MkDBField <$>) [endpointM]
    orderBy = case sortM of
      Nothing -> "avm.created_at desc"
      Just "first_seen" -> "avm.created_at desc"
      Just "events" -> "events desc"
      Just "last_seen" -> "last_seen desc"
      _ -> "avm.created_at desc"

    limit = maybe "" (\x -> "limit " <> show x) limitM

    -- FIXME: optimize anomalies equation
    q =
      [text|
SELECT avm.id, avm.created_at, avm.updated_at, avm.project_id, aan.acknowleged_at, aan.acknowleged_by, avm.anomaly_type, avm.action, avm.target_hash,
       avm.shape_id, avm.new_unique_fields, avm.deleted_fields, avm.updated_field_formats, 
       avm.field_id, avm.field_key, avm.field_key_path, avm.field_category, avm.field_format, 
       avm.format_id, avm.format_type, avm.format_examples, 
       avm.endpoint_id, avm.endpoint_method, avm.endpoint_url_path, aan.archived_at,
       -- count(rd.id) events, max(rd.created_at) last_seen
       COUNT(CASE WHEN rd.created_at > NOW() - interval '14 days' THEN 1 ELSE NULL END) AS events,
       MAX(CASE WHEN rd.created_at > NOW() - interval '14 days' THEN rd.created_at ELSE NULL END) AS last_seen
    FROM
        apis.anomalies_vm avm
    JOIN apis.anomalies aan ON avm.id = aan.id
    RIGHT JOIN apis.request_dumps rd ON avm.project_id=rd.project_id 
        AND (avm.target_hash=ANY(rd.format_hashes) AND avm.anomaly_type='format')
        OR  (avm.target_hash=rd.shape_hash AND avm.anomaly_type='shape')
        OR  (avm.target_hash=rd.endpoint_hash AND avm.anomaly_type='endpoint')
    WHERE
        avm.project_id = ? 
        $cond
        AND avm.anomaly_type != 'field'
        AND rd.created_at > NOW() - interval '14 days' 
    GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25
    ORDER BY $orderBy
    $limit;
      |]

-- TODO: notice the limit? We need to support pagination on the anomalies page

getReportAnomalies :: Projects.ProjectId -> Text -> DBT IO (Vector AnomalyVM)
getReportAnomalies pid report_type = query Select (Query $ encodeUtf8 q) pid
  where
    report_interval = if report_type == "daily" then ("'24 hours'" :: Text) else "'7 days'"
    q =
      [text|
  SELECT avm.id, avm.created_at, avm.updated_at, avm.project_id, aan.acknowleged_at, aan.acknowleged_by, avm.anomaly_type, avm.action, avm.target_hash,
         avm.shape_id, avm.new_unique_fields, avm.deleted_fields, avm.updated_field_formats, 
         avm.field_id, avm.field_key, avm.field_key_path, avm.field_category, avm.field_format, 
         avm.format_id, avm.format_type, avm.format_examples, 
         avm.endpoint_id, avm.endpoint_method, avm.endpoint_url_path, aan.archived_at,
         count(rd.id) events, max(rd.created_at) last_seen
      FROM
          apis.anomalies_vm avm
      JOIN apis.anomalies aan ON avm.id = aan.id
      JOIN apis.request_dumps rd ON avm.project_id=rd.project_id 
          AND (avm.target_hash=ANY(rd.format_hashes) AND avm.anomaly_type='format')
          OR  (avm.target_hash=rd.shape_hash AND avm.anomaly_type='shape')
          OR  (avm.target_hash=rd.endpoint_hash AND avm.anomaly_type='endpoint')
      WHERE
          avm.project_id = ? 
          AND avm.anomaly_type != 'field'
          AND rd.created_at > NOW() - interval $report_interval
          AND aan.acknowleged_at IS NULL
          AND aan.archived_at IS NULL
      GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25
      HAVING count(rd.id) > 5
      limit 11;
        |]

countAnomalies :: Projects.ProjectId -> Text -> DBT IO Int
countAnomalies pid report_type = do
  result <- query Select (Query $ encodeUtf8 q) pid
  case result of
    [Only count] -> return count
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
