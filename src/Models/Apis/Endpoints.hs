{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Endpoints
  ( Endpoint (..),
    EndpointId (..),
    upsertEndpoints,
    endpointsByProject,
    endpointById,
  )
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (selectById, selectManyByField)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import GHC.Generics (Generic)
import Models.Projects.Projects qualified as Projects
import Optics.Operators ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
  ( Bool (True),
    Eq ((==)),
    IO,
    Maybe,
    Ord,
    Show,
    Text,
  )
import Web.HttpApiData (FromHttpApiData)

-- Added only for satisfying the tests
instance Eq ZonedTime where
  (==) _ _ = True

newtype EndpointId = EndpointId {unEndpointId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

-- TODO: Introduce request header hashes and response header hashes
data Endpoint = Endpoint
  { id :: EndpointId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    urlPath :: Text,
    urlParams :: AE.Value,
    method :: Text,
    hosts :: Vector.Vector Text,
    requestHashes :: Vector.Vector Text,
    responseHashes :: Vector.Vector Text,
    queryparamHashes :: Vector.Vector Text
  }
  deriving (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "endpoints", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Endpoint)

makeFieldLabelsNoPrefix ''Endpoint

upsertEndpoints :: Endpoint -> PgT.DBT IO (Maybe (EndpointId, Text, Text))
upsertEndpoints endpoint = queryOne Insert q options
  where
    q =
      [sql|  
        INSERT INTO apis.endpoints (project_id, url_path, url_params, method, hosts, request_hashes, response_hashes, queryparam_hashes)
        VALUES(?, ?, ?, ?, ?, ?, ?, ?) 
        ON CONFLICT (project_id, url_path, method) 
        DO 
           UPDATE SET 
            hosts = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.hosts, excluded.hosts) as e order by e),
            request_hashes = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.request_hashes, excluded.request_hashes) as e order by e),
            response_hashes = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.response_hashes, excluded.response_hashes) as e order by e),
            queryparam_hashes = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.queryparam_hashes, excluded.queryparam_hashes) as e order by e)
        RETURNING id, method, url_path 
      |]
    options =
      ( endpoint ^. #projectId,
        endpoint ^. #urlPath,
        endpoint ^. #urlParams,
        endpoint ^. #method,
        endpoint ^. #hosts,
        endpoint ^. #requestHashes,
        endpoint ^. #responseHashes,
        endpoint ^. #queryparamHashes
      )

endpointsByProject :: Projects.ProjectId -> PgT.DBT IO (Vector.Vector Endpoint)
endpointsByProject pid = selectManyByField @Endpoint [field| project_id |] pid

endpointById :: EndpointId -> PgT.DBT IO (Maybe Endpoint)
endpointById = selectById @Endpoint
