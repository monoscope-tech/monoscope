{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Shapes (insertShape, Shape (..), ShapeId (..)) where

import Data.Default (Default)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert), queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, Only, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Optics.TH
import Relude
import Web.HttpApiData (FromHttpApiData)

newtype ShapeId = ShapeId {unShapeId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromField, ToField, FromHttpApiData, Default)
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
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)

makeFieldLabelsNoPrefix ''Shape

-- data CreateShape = CreateShape
--   { projectId :: Projects.ProjectId,
--     endpointId :: Endpoints.EndpointId,
--     queryParamsKeypaths :: Vector Text,
--     requestBodyKeypaths :: Vector Text,
--     responseBodyKeypaths :: Vector Text,
--     requestHeadersKeypaths :: Vector Text,
--     responseHeadersKeypaths :: Vector Text
--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (FromRow, ToRow, Default)
--   deriving
--     (Entity)
--     via (GenericEntity '[Schema "apis", TableName "fields", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)

insertShape :: Shape -> DBT IO (Maybe (Only ShapeId))
insertShape shape = queryOne Insert q options
  where
    q =
      [sql| insert into shapes
           (project_id, endpoint_id, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_header_keypaths, response_header_keypaths)
                       values (?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING|]
    options = ()

-- insertShape = insert @Shape

-- insertShape :: CreateShape -> DBT IO ()
-- insertShape = insert @CreateShape

-- insertShape = queryOne Insert q
--   where q = [sql| insert into shapes
--                       (project_id, endpoint_id, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_header_keypaths, response_header_keypaths)
--                       values (?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING|]
