{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Shapes (insertShape, Shape (..), ShapeId (..)) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert), queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Optics.Core ((^.))
import Optics.TH
import Relude
import Web.HttpApiData (FromHttpApiData)

newtype ShapeId = ShapeId {unShapeId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (AE.FromJSON, Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

data Shape = Shape
  { id :: ShapeId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    endpointId :: Endpoints.EndpointId,
    queryParamsKeypaths :: Vector Text,
    requestBodyKeypaths :: Vector Text,
    responseBodyKeypaths :: Vector Text,
    requestHeadersKeypaths :: Vector Text,
    responseHeadersKeypaths :: Vector Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Shape
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)
  deriving (FromField) via Aeson Shape

makeFieldLabelsNoPrefix ''Shape

insertShape :: Shape -> DBT IO (Maybe (Only ShapeId))
insertShape shape = queryOne Insert q options
  where
    q =
      [sql| 
        with s as (
            INSERT INTO apis.shapes
            (project_id, endpoint_id, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths)
            VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING
            RETURNING id
          )
          SELECT * from s 
          UNION 
            SELECT id from apis.shapes where 
              project_id=? AND endpoint_id=? AND query_params_keypaths=? AND request_body_keypaths=? AND 
              response_body_keypaths=? AND request_headers_keypaths=? AND response_headers_keypaths=?;
            |]
    options =
      ( shape ^. #projectId,
        shape ^. #endpointId,
        shape ^. #queryParamsKeypaths,
        shape ^. #requestBodyKeypaths,
        shape ^. #responseBodyKeypaths,
        shape ^. #requestHeadersKeypaths,
        shape ^. #responseHeadersKeypaths,
        shape ^. #projectId,
        shape ^. #endpointId,
        shape ^. #queryParamsKeypaths,
        shape ^. #requestBodyKeypaths,
        shape ^. #responseBodyKeypaths,
        shape ^. #requestHeadersKeypaths,
        shape ^. #responseHeadersKeypaths
      )
