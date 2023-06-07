{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Shapes (Shape (..), ShapeId (..), shapeIdText, insertShapeQueryAndParam, shapesByEndpointHash) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Query, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Optics.TH
import Relude
import Utils (DBField (MkDBField))
import Web.HttpApiData (FromHttpApiData)

newtype ShapeId = ShapeId {unShapeId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (AE.FromJSON, Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

shapeIdText :: ShapeId -> Text
shapeIdText = UUID.toText . unShapeId

-- A shape is a deterministic representation of a request-response combination for a given endpoint.
-- We usually expect multiple shapes per endpoint. Eg a shape for a success request-response and another for an error response.
data Shape = Shape
  { id :: ShapeId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Shape
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)
  deriving (FromField) via Aeson Shape

Optics.TH.makeFieldLabelsNoPrefix ''Shape

insertShapeQueryAndParam :: Shape -> (Query, [DBField])
insertShapeQueryAndParam shape = (q, params)
 where
  q =
    [sql| 
            INSERT INTO apis.shapes
            (project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths, field_hashes, hash, status_code)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING; 
          |]
  params =
    [ MkDBField $ shape.projectId
    , MkDBField $ shape.endpointHash
    , MkDBField $ shape.queryParamsKeypaths
    , MkDBField $ shape.requestBodyKeypaths
    , MkDBField $ shape.responseBodyKeypaths
    , MkDBField $ shape.requestHeadersKeypaths
    , MkDBField $ shape.responseHeadersKeypaths
    , MkDBField $ shape.fieldHashes
    , MkDBField $ shape.hash
    , MkDBField $ shape.statusCode
    ]

shapesByEndpointHash :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector Shape)
shapesByEndpointHash pid hashes = query Select q (pid, hashes)
 where
  q =
    [sql|
      SELECT id, created_at, updated_at, project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, 
             response_headers_keypaths, field_hashes, hash, status_code
      FROM apis.shapes
      WHERE project_id = ? AND endpoint_hash = ANY(?)
    |]