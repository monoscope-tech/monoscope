{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Endpoints
  ( Endpoint (..),
    EndpointId (..),
    EndpointRequestStats (..),
    endpointUrlPath,
    upsertEndpoints,
    endpointRequestStatsByProject,
    endpointRequestStatsByEndpoint,
    endpointById,
    endpointIdText,
    endpointToUrlPath,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity (selectById, selectManyByField)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Optics.Operators ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import Web.HttpApiData (FromHttpApiData)

-- Added only for satisfying the tests
instance Eq ZonedTime where
  (==) _ _ = True

newtype EndpointId = EndpointId {unEndpointId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (ToJSON, FromJSON, Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

endpointIdText :: EndpointId -> Text
endpointIdText = UUID.toText . unEndpointId

-- TODO: Introduce request header hashes and response header hashes
data Endpoint = Endpoint
  { id :: EndpointId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    urlPath :: Text,
    urlParams :: AE.Value, -- Key value map of key to the type. Needs a bit more figuring out.
    method :: Text,
    hosts :: Vector.Vector Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "endpoints", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Endpoint)
  deriving (FromField) via Aeson Endpoint

makeFieldLabelsNoPrefix ''Endpoint

-- | endpointToUrlPath builds an apitoolkit path link to the endpoint details page of that endpoint.
endpointToUrlPath :: Endpoint -> Text
endpointToUrlPath enp = endpointUrlPath (enp ^. #projectId) (enp ^. #id)

endpointUrlPath :: Projects.ProjectId -> EndpointId -> Text
endpointUrlPath pid eid = "/p/" <> Projects.projectIdText pid <> "/endpoints/" <> endpointIdText eid

upsertEndpoints :: Endpoint -> PgT.DBT IO (Maybe EndpointId)
upsertEndpoints endpoint = queryOne Insert q options
  where
    host = fromMaybe "" ((endpoint ^. #hosts) Vector.!? 0) -- Read the first item from head or default to empty string
    q =
      [sql|  
        with e as (
          INSERT INTO apis.endpoints (project_id, url_path, url_params, method, hosts)
          VALUES(?, ?, ?, ?, $$ ? => null$$) 
          ON CONFLICT (project_id, url_path, method) 
          DO 
             UPDATE SET 
              hosts[?] = null 
          RETURNING id 
        )
        SELECT id from e 
        UNION 
          SELECT id FROM apis.endpoints WHERE project_id=? AND url_path=? AND method=?;
      |]
    options =
      ( endpoint ^. #projectId,
        endpoint ^. #urlPath,
        endpoint ^. #urlParams,
        endpoint ^. #method,
        host,
        host,
        endpoint ^. #projectId,
        endpoint ^. #urlPath,
        endpoint ^. #method
      )

-- Based of a view which is generated every 5minutes.
data EndpointRequestStats = EndpointRequestStats
  { endpointId :: EndpointId,
    projectId :: Projects.ProjectId,
    urlPath :: Text,
    method :: Text,
    min :: Double,
    p50 :: Double,
    p75 :: Double,
    p90 :: Double,
    p95 :: Double,
    p99 :: Double,
    max :: Double,
    totalTime :: Double,
    totalTimeProj :: Double,
    totalRequests :: Int,
    totalRequestsProj :: Int
    -- ongoingAnomalies :: Int
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "endpoint_request_stats", PrimaryKey "endpoint_id", FieldModifiers '[CamelToSnake]] EndpointRequestStats)

-- FIXME: Include and return a boolean flag to show if fields that have annomalies.
endpointRequestStatsByProject :: Projects.ProjectId -> PgT.DBT IO (Vector EndpointRequestStats)
endpointRequestStatsByProject = selectManyByField @EndpointRequestStats [field| project_id |]

endpointRequestStatsByEndpoint :: EndpointId -> PgT.DBT IO (Maybe EndpointRequestStats)
endpointRequestStatsByEndpoint = selectById @EndpointRequestStats

endpointById :: EndpointId -> PgT.DBT IO (Maybe Endpoint)
endpointById = selectById @Endpoint
