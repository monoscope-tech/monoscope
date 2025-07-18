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
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (query)
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
    (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, Ord, ToField)
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


getShapeFields :: Shape -> V.Vector Fields.Field -> ShapeWithFields
getShapeFields shape fields =
  ShapeWidthFields
    { status = shape.statusCode
    , sHash = shape.hash
    , fieldsMap = fieldM
    , reqDescription = shape.requestDescription
    , resDescription = shape.responseDescription
    }
  where
    matchedFields = V.filter (\field -> field.hash `V.elem` shape.fieldHashes) fields
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
  , queryParamsKeypaths :: V.Vector Text
  , requestBodyKeypaths :: V.Vector Text
  , responseBodyKeypaths :: V.Vector Text
  , requestHeadersKeypaths :: V.Vector Text
  , responseHeadersKeypaths :: V.Vector Text
  , fieldHashes :: V.Vector Text
  , hash :: Text
  , statusCode :: Int
  , responseDescription :: Text
  , requestDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "shapes", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)
  deriving (FromField) via Aeson Shape
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Shape


bulkInsertShapes :: DB :> es => V.Vector Shape -> Eff es ()
bulkInsertShapes shapes = void $ dbtToEff $ executeMany q $ V.toList rowsToInsert
  where
    q =
      [sql| 
        INSERT INTO apis.shapes
            (project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths, field_hashes, hash, status_code, request_description, response_description)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING|]
    rowsToInsert =
      V.map
        ( \shape ->
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
        )
        shapes


shapesByEndpointHash :: Projescts.ProjectId -> Text -> PgT.DBT IO (V.Vector Shape)
shapesByEndpointHash pid endpointHash = query q (pid, endpointHash)
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


getShapeParams :: Shape -> (Maybe UTCTime, Projects.ProjectId, Text, V.Vector Text, V.Vector Text, V.Vector Text, V.Vector Text, V.Vector Text, V.Vector Text, Text, Int, Text, Text)
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
  , swQueryParamsKeypaths :: V.Vector Text
  , swRequestBodyKeypaths :: V.Vector Text
  , swResponseBodyKeypaths :: V.Vector Text
  , swRequestHeadersKeypaths :: V.Vector Text
  , swResponseHeadersKeypaths :: V.Vector Text
  , swHash :: Text
  , swStatusCode :: Int
  , swFieldHashes :: V.Vector Text
  , swRequestDescription :: Text
  , swResponseDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving anyclass (AE.ToJSON)
  deriving (FromField) via Aeson SwShape
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwShape


shapesByEndpointHashes :: Projects.ProjectId -> V.Vector Text -> PgT.DBT IO (V.Vector SwShape)
shapesByEndpointHashes pid hashes
  | V.null hashes = pure V.empty
  | otherwise = query q (pid, hashes)
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
