module Models.Apis.Shapes (
  Shape (..),
  ShapeWithFields (..),
  SwShape (..),
  ShapeId,
  ShapeForIssue (..),
  bulkInsertShapes,
  getShapeForIssue,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL qualified as PG
import Models.Apis.Fields.Types
import Models.Apis.Fields.Types qualified as Fields
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Types (DB)


type ShapeId = UUIDId "shape"


data ShapeWithFields = ShapeWidthFields
  { status :: Int
  , sHash :: Text
  , fieldsMap :: Map FieldCategoryEnum [Fields.Field]
  , reqDescription :: Text
  , resDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


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


bulkInsertShapes :: DB es => V.Vector Shape -> Eff es ()
bulkInsertShapes shapes = void $ PG.executeMany q $ V.toList rowsToInsert
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


-- | Shape data needed for creating issues
data ShapeForIssue = ShapeForIssue
  { shapeHash :: Text
  , endpointHash :: Text
  , method :: Text
  , path :: Text
  , statusCode :: Int
  , exampleRequestPayload :: AE.Value
  , exampleResponsePayload :: AE.Value
  , newFields :: V.Vector Text
  , deletedFields :: V.Vector Text
  , modifiedFields :: V.Vector Text
  , fieldHashes :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow)


getShapeForIssue :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe ShapeForIssue)
getShapeForIssue pid hash = listToMaybe <$> PG.query q (Only hash)
  where
    q =
      [sql|
        SELECT
          s.hash,
          s.endpoint_hash,
          COALESCE(e.method, 'UNKNOWN'),
          COALESCE(e.url_path, '/'),
          s.status_code,
          COALESCE(s.example_request_payload, '{}'::jsonb),
          COALESCE(s.example_response_payload, '{}'::jsonb),
          COALESCE(s.new_unique_fields, '{}'::TEXT[]),
          COALESCE(s.deleted_fields, '{}'::TEXT[]),
          COALESCE(s.updated_field_formats, '{}'::TEXT[]),
          COALESCE(s.field_hashes, '{}'::TEXT[])
        FROM apis.shapes s
        LEFT JOIN apis.endpoints e ON e.hash = s.endpoint_hash
        WHERE s.project_id = ? AND s.hash = ?
      |]
