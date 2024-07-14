module Models.Apis.Shapes (
  Shape (..),
  ShapeWithFields (..),
  SwShape (..),
  ShapeId (..),
  getShapeFields,
  bulkInsertShapes,
  shapeIdText,
  insertShapes,
  shapesByEndpointHashes,
  shapesByEndpointHash,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (UTCTime, getZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.Fields.Types
import Models.Apis.Fields.Types qualified as Fields
import Models.Projects.Projects qualified as Projects
import Models.Projects.Projects qualified as Projescts
import Relude
import Web.HttpApiData (FromHttpApiData)


newtype ShapeId = ShapeId {unShapeId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving
    (AE.FromJSON, AE.ToJSON, Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID


shapeIdText :: ShapeId -> Text
shapeIdText = UUID.toText . unShapeId


data ShapeWithFields = ShapeWidthFields
  { status :: Int
  , sHash :: Text
  , fieldsMap :: Map FieldCategoryEnum [Fields.Field]
  , reqDescription :: Text
  , resDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


getShapeFields :: Shape -> Vector Fields.Field -> ShapeWithFields
getShapeFields shape fields =
  ShapeWidthFields
    { status = shape.statusCode
    , sHash = shape.hash
    , fieldsMap = fieldM
    , reqDescription = shape.requestDescription
    , resDescription = shape.responseDescription
    }
  where
    matchedFields = Vector.filter (\field -> field.hash `Vector.elem` shape.fieldHashes) fields
    fieldM = Fields.groupFieldsByCategory matchedFields


-- A shape is a deterministic representation of a request-response combination for a given endpoint.
-- We usually expect multiple shapes per endpoint. Eg a shape for a success request-response and another for an error response.
data Shape = Shape
  { id :: ShapeId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , approvedOn :: Maybe UTCTime
  , projectId :: Projects.ProjectId
  , endpointHash :: Text
  , queryParamsKeypaths :: Vector Text
  , requestBodyKeypaths :: Vector Text
  , responseBodyKeypaths :: Vector Text
  , requestHeadersKeypaths :: Vector Text
  , responseHeadersKeypaths :: Vector Text
  , fieldHashes :: Vector Text
  , hash :: Text
  , statusCode :: Int
  , responseDescription :: Text
  , requestDescription :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Shape
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "shapes", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)
  deriving (FromField) via Aeson Shape


bulkInsertShapes :: DB :> es => [Shape] -> Eff es ()
bulkInsertShapes shapes = void $ dbtToEff $ executeMany q rowsToInsert
  where
    q =
      [sql| 
        INSERT INTO apis.shapes
            (project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths, field_hashes, hash, status_code, request_description, response_description)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING|]
    rowsToInsert =
      shapes <&> \shape ->
        ( shape.projectId
        , shape.endpointHash
        , shape.queryParamsKeypaths
        , shape.requestBodyKeypaths
        , shape.responseBodyKeypaths
        , shape.requestHeadersKeypaths
        , shape.responseHeadersKeypaths
        , shape.fieldHashes
        , shape.hash
        , fromIntegral shape.statusCode
        , ""
        , ""
        )


shapesByEndpointHash :: Projescts.ProjectId -> Text -> PgT.DBT IO (Vector Shape)
shapesByEndpointHash pid endpointHash = query Select q (pid, endpointHash)
  where
    q =
      [sql| 
          SELECT id, created_at, updated_at, approved_on, project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, 
          response_body_keypaths, request_headers_keypaths, response_headers_keypaths, field_hashes, hash, status_code, request_description, response_description
          FROM apis.shapes WHERE project_id= ? AND endpoint_hash = ?
      |]


insertShapes :: [Shape] -> DBT IO Int64
insertShapes shapes = do
  currTime <- liftIO getZonedTime
  let insertQuery =
        [sql|
        INSERT INTO apis.shapes
        (approved_on, project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths, field_hashes, hash, status_code, request_description, response_description)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) 
        ON CONFLICT (hash)
        DO UPDATE SET request_headers_keypaths = EXCLUDED.request_headers_keypaths, approved_on = EXCLUDED.approved_on, request_description = EXCLUDED.request_description, response_description = EXCLUDED.response_description
      |]
  let params = map getShapeParams shapes
  executeMany insertQuery params


getShapeParams :: Shape -> (Maybe UTCTime, Projects.ProjectId, Text, Vector Text, Vector Text, Vector Text, Vector Text, Vector Text, Vector Text, Text, Int, Text, Text)
getShapeParams shape =
  ( shape.approvedOn
  , shape.projectId
  , shape.endpointHash
  , shape.queryParamsKeypaths
  , shape.requestBodyKeypaths
  , shape.responseBodyKeypaths
  , shape.requestHeadersKeypaths
  , shape.responseHeadersKeypaths
  , shape.fieldHashes
  , shape.hash
  , shape.statusCode
  , shape.requestDescription
  , shape.responseDescription
  )


data SwShape = SwShape
  { swEndpointHash :: Text
  , swQueryParamsKeypaths :: Vector Text
  , swRequestBodyKeypaths :: Vector Text
  , swResponseBodyKeypaths :: Vector Text
  , swRequestHeadersKeypaths :: Vector Text
  , swResponseHeadersKeypaths :: Vector Text
  , swHash :: Text
  , swStatusCode :: Int
  , swFieldHashes :: Vector Text
  , swRequestDescription :: Text
  , swResponseDescription :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default, NFData)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwShape
  deriving (FromField) via Aeson SwShape
  deriving anyclass (AE.ToJSON)


shapesByEndpointHashes :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector SwShape)
shapesByEndpointHashes pid hashes = query Select q (pid, hashes)
  where
    q =
      [sql|
      SELECT endpoint_hash sw_endpoint_hash, query_params_keypaths sw_query_params_keypaths, request_body_keypaths sw_request_body_keypaths,
             response_body_keypaths sw_response_body_keypaths, request_headers_keypaths sw_request_headers_keypaths, 
             response_headers_keypaths sw_response_headers_keypaths, hash sw_hash,status_code sw_status_code,field_hashes sw_field_hashes,
             request_description sw_request_description, response_description sw_response_description
      FROM apis.shapes sh
      INNER JOIN apis.anomalies ann ON (ann.anomaly_type = 'shape' AND  ann.target_hash = sh.hash)
      WHERE sh.project_id = ? AND endpoint_hash = ANY(?)
    |]
