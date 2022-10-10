{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
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
    upsertEndpointQueryAndParam,
    endpointByHash,
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
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), Query, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import Utils (DBField (MkDBField))
import Web.HttpApiData (FromHttpApiData)

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
    hosts :: Vector.Vector Text,
    hash :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint
  deriving (FromField) via Aeson Endpoint

makeFieldLabelsNoPrefix ''Endpoint

-- | endpointToUrlPath builds an apitoolkit path link to the endpoint details page of that endpoint.
endpointToUrlPath :: Endpoint -> Text
endpointToUrlPath enp = endpointUrlPath (enp.projectId) (enp.id)

endpointUrlPath :: Projects.ProjectId -> EndpointId -> Text
endpointUrlPath pid eid = "/p/" <> Projects.projectIdText pid <> "/endpoints/" <> endpointIdText eid

upsertEndpointQueryAndParam :: Endpoint -> (Query, [DBField])
upsertEndpointQueryAndParam endpoint = (q, params)
  where
    host = fromMaybe @Text "" ((endpoint.hosts) Vector.!? 0) -- Read the first item from head or default to empty string
    q =
      [sql|  
          INSERT INTO apis.endpoints (project_id, url_path, url_params, method, hosts, hash)
          VALUES(?, ?, ?, ?, $$ ? => null$$, ?) 
          ON CONFLICT (project_id, url_path, method) 
          DO 
             UPDATE SET hosts=endpoints.hosts||hstore(?, null); 
      |]
    params =
      [ MkDBField endpoint.projectId,
        MkDBField endpoint.urlPath,
        MkDBField endpoint.urlParams,
        MkDBField endpoint.method,
        MkDBField host,
        MkDBField endpoint.hash,
        MkDBField host
      ]

-- FIXME: Delete this, as this function is deprecated and no longer in use.
-- Updating hosts can be a specific functino that does just that.
upsertEndpoints :: Endpoint -> PgT.DBT IO (Maybe EndpointId)
upsertEndpoints endpoint = queryOne Insert q options
  where
    host = fromMaybe "" ((endpoint.hosts) Vector.!? 0) -- Read the first item from head or default to empty string
    q =
      [sql|  
        with e as (
          INSERT INTO apis.endpoints (project_id, url_path, url_params, method, hosts)
          VALUES(?, ?, ?, ?, $$ ? => null$$) 
          ON CONFLICT (project_id, url_path, method) 
          DO 
             UPDATE SET 
               hosts=endpoints.hosts||hstore(?, null) 
          RETURNING id 
        )
        SELECT id from e 
        UNION 
          SELECT id FROM apis.endpoints WHERE project_id=? AND url_path=? AND method=?;
      |]
    options =
      ( endpoint.projectId,
        endpoint.urlPath,
        endpoint.urlParams,
        endpoint.method,
        host,
        host,
        endpoint.projectId,
        endpoint.urlPath,
        endpoint.method
      )

-- Based of a view which is generated every 5minutes.
data EndpointRequestStats = EndpointRequestStats
  { endpointId :: EndpointId,
    endpointHash :: Text,
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
    totalRequestsProj :: Int,
    ongoingAnomalies :: Int,
    ongoingAnomaliesProj :: Int
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "endpoint_request_stats", PrimaryKey "endpoint_id", FieldModifiers '[CamelToSnake]] EndpointRequestStats)

-- FIXME: Include and return a boolean flag to show if fields that have annomalies.
-- FIXME: return endpoint_hash as well.
endpointRequestStatsByProject :: Projects.ProjectId -> PgT.DBT IO (Vector EndpointRequestStats)
endpointRequestStatsByProject pid = query Select q (pid, pid)
  where
    q =
      [sql| SELECT endpoint_id, endpoint_hash, project_id, url_path, method, min, p50, p75, p90, p95, p99, max, 
                   total_time, total_time_proj, total_requests, total_requests_proj,
                   (SELECT count(*) from apis.anomalies 
                           where project_id=? AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
                   ) ongoing_anomalies,
                  (SELECT count(*) from apis.anomalies 
                           where project_id=project_id AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
                   ) ongoing_anomalies_proj
              FROM apis.endpoint_request_stats WHERE project_id=?|]

-- FIXME: return endpoint_hash as well.
-- This would require tampering with the view.
endpointRequestStatsByEndpoint :: EndpointId -> PgT.DBT IO (Maybe EndpointRequestStats)
endpointRequestStatsByEndpoint eid = queryOne Select q (eid, eid)
  where
    q =
      [sql| SELECT endpoint_id, endpoint_hash, project_id, url_path, method, min, p50, p75, p90, p95, p99, max, 
                   total_time, total_time_proj, total_requests, total_requests_proj,
                   (SELECT count(*) from apis.anomalies 
                           where endpoint_id=? AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
                   ) ongoing_anomalies,
                  (SELECT count(*) from apis.anomalies 
                           where project_id=project_id AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
                   ) ongoing_anomalies_proj
              FROM apis.endpoint_request_stats WHERE endpoint_id=?|]

endpointById :: EndpointId -> PgT.DBT IO (Maybe Endpoint)
endpointById eid = queryOne Select q (Only eid)
  where
    q = [sql| SELECT id, created_at, updated_at, project_id, url_path, url_params, method, akeys(hosts), hash from apis.endpoints where id=? |]

endpointByHash :: Projects.ProjectId -> Text -> PgT.DBT IO (Maybe Endpoint)
endpointByHash pid hash = queryOne Select q (pid, hash)
  where
    q = [sql| SELECT id, created_at, updated_at, project_id, url_path, url_params, method, akeys(hosts), hash from apis.endpoints where project_id=? AND hash=? |]
