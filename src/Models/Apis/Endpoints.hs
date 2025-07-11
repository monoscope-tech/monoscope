{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Models.Apis.Endpoints (
  Endpoint (..),
  SwEndpoint (..),
  EndpointId (..),
  EndpointRequestStats (..),
  Host (..),
  HostEvents (..),
  bulkInsertEndpoints,
  dependenciesAndEventsCount,
  endpointsByProjectId,
  endpointUrlPath,
  endpointRequestStatsByProject,
  dependencyEndpointsRequestStatsByProject,
  endpointRequestStatsByEndpoint,
  endpointById,
  endpointIdText,
  endpointToUrlPath,
  endpointByHash,
  endpointsByHashes,
  getProjectHosts,
  insertEndpoints,
  countEndpointInbox,
  getEndpointsByAnomalyTargetHash,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import GHC.Records (HasField (getField))
import Language.Haskell.TH.Syntax qualified as THS
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Relude
import Web.HttpApiData (FromHttpApiData)


newtype EndpointId = EndpointId {unEndpointId :: UUID.UUID}
  deriving stock (Generic, Read, Show, THS.Lift)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, NFData, Ord, ToField)
  deriving anyclass (FromRow, ToRow)


newtype Host = Host {host :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)


instance HasField "toText" EndpointId Text where
  getField = UUID.toText . unEndpointId


endpointIdText :: EndpointId -> Text
endpointIdText = UUID.toText . unEndpointId


-- TODO: Introduce request header hashes and response header hashes
data Endpoint = Endpoint
  { id :: EndpointId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , projectId :: Projects.ProjectId
  , urlPath :: Text
  , urlParams :: AE.Value -- Key value map of key to the type. Needs a bit more figuring out.
  , method :: Text
  , host :: Text
  , hash :: Text
  , outgoing :: Bool
  , description :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (FromField) via Aeson Endpoint
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint


-- | endpointToUrlPath builds an apitoolkit path link to the endpoint details page of that endpoint.
endpointToUrlPath :: Endpoint -> Text
endpointToUrlPath enp = endpointUrlPath enp.projectId enp.id


endpointUrlPath :: Projects.ProjectId -> EndpointId -> Text
endpointUrlPath pid eid = "/p/" <> pid.toText <> "/endpoints/" <> endpointIdText eid


bulkInsertEndpoints :: DB :> es => V.Vector Endpoint -> Eff es ()
bulkInsertEndpoints endpoints = void $ dbtToEff $ executeMany q $ V.toList rowsToInsert
  where
    q =
      [sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing)
          VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT (hash) DO  NOTHING;
      |]
    rowsToInsert =
      endpoints <&> \endpoint ->
        ( endpoint.projectId
        , endpoint.urlPath
        , endpoint.urlParams
        , endpoint.method
        , endpoint.host
        , endpoint.hash
        , endpoint.outgoing
        )


-- Based of a view which is generated every 5minutes.
data EndpointRequestStats = EndpointRequestStats
  { endpointId :: EndpointId
  , endpointHash :: Text
  , projectId :: Projects.ProjectId
  , urlPath :: Text
  , method :: Text
  , host :: Text
  , min :: Double
  , p50 :: Double
  , p75 :: Double
  , p90 :: Double
  , p95 :: Double
  , p99 :: Double
  , max :: Double
  , totalTime :: Double
  , totalTimeProj :: Double
  , totalRequests :: Int
  , totalRequestsProj :: Int
  , ongoingAnomalies :: Int
  , ongoingAnomaliesProj :: Int
  , acknowlegedAt :: Maybe UTCTime
  , archivedAt :: Maybe UTCTime
  , anomalyId :: UUID.UUID
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "endpoint_request_stats", PrimaryKey "endpoint_id", FieldModifiers '[CamelToSnake]] EndpointRequestStats)


-- FIXME: Include and return a boolean flag to show if fields that have annomalies.
-- FIXME: return endpoint_hash as well.
endpointRequestStatsByProject :: Projects.ProjectId -> Bool -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Text -> PgT.DBT IO (V.Vector EndpointRequestStats)
endpointRequestStatsByProject pid ackd archived pHostM sortM searchM page requestType = query (Query $ encodeUtf8 q) queryParams
  where
    -- Construct the list of parameters conditionally
    pHostParams = maybe [] (\h -> [toField h]) pHostM
    queryParams = [toField pid, toField isOutgoing] ++ pHostParams ++ [toField offset]

    isOutgoing = requestType == "Outgoing"
    offset = page * 30
    ackdAt = if ackd && not archived then "AND ann.acknowleged_at IS NOT NULL AND ann.archived_at IS NULL " else "AND ann.acknowleged_at IS NULL "
    archivedAt = if archived then "AND ann.archived_at IS NOT NULL " else " AND ann.archived_at IS NULL"
    search = case searchM of Just s -> " AND enp.url_path LIKE '%" <> s <> "%'"; Nothing -> ""
    pHostQuery = case pHostM of Just h -> " AND enp.host = ?"; Nothing -> ""
    orderBy = case fromMaybe "" sortM of "first_seen" -> "enp.created_at ASC"; "last_seen" -> "enp.created_at DESC"; _ -> " coalesce(ers.total_requests,0) DESC"
    -- TODO This query to get the anomalies for the anomalies page might be too complex.
    -- Does it make sense yet to remove the call to endpoint_request_stats? since we're using async charts already
    q =
      [text| SELECT enp.id endpoint_id, enp.hash endpoint_hash, enp.project_id, enp.url_path, enp.method, enp.host, coalesce(min,0),  coalesce(p50,0),  coalesce(p75,0),  coalesce(p90,0),  coalesce(p95,0),  coalesce(p99,0),  coalesce(max,0) ,
         coalesce(total_time,0), coalesce(total_time_proj,0), coalesce(ers.total_requests,0), coalesce(total_requests_proj,0),
         (SELECT count(*) from apis.issues
                 where project_id=enp.project_id AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
         ) ongoing_anomalies,
        (SELECT count(*) from apis.issues
                 where project_id=enp.project_id AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
        ) ongoing_anomalies_proj,
        ann.acknowleged_at,
        ann.archived_at,
        ann.id
     from apis.endpoints enp
     left join apis.endpoint_request_stats ers on (enp.id=ers.endpoint_id)
     left join apis.issues ann on (ann.anomaly_type='endpoint' AND ann.target_hash=enp.hash)
     where enp.project_id=? and ann.id is not null and enp.outgoing=? $ackdAt $archivedAt $pHostQuery $search
     order by $orderBy , url_path ASC
     offset ? limit 30;
     ;
  |]


dependencyEndpointsRequestStatsByProject :: Projects.ProjectId -> Text -> Bool -> Bool -> Maybe Text -> Maybe Text -> Int -> PgT.DBT IO (V.Vector EndpointRequestStats)
dependencyEndpointsRequestStatsByProject pid host ack arch sortM searchM page = query (Query $ encodeUtf8 q) (pid, host)
  where
    q =
      [text|
      SELECT enp.id endpoint_id, enp.hash endpoint_hash, enp.project_id, enp.url_path, enp.method, enp.host, coalesce(min,0),  coalesce(p50,0),  coalesce(p75,0),  coalesce(p90,0), coalesce(p95,0),  coalesce(p99,0),  coalesce(max,0) ,
         coalesce(total_time,0), coalesce(total_time_proj,0), coalesce(total_requests,0), coalesce(total_requests_proj,0),
         (SELECT count(*) from apis.issues
                 where project_id=enp.project_id AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
         ) ongoing_anomalies,
        (SELECT count(*) from apis.issues
                 where project_id=enp.project_id AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
        ) ongoing_anomalies_proj,
        ann.acknowleged_at,
        ann.archived_at,
        ann.id
     from apis.endpoints enp
     left join apis.endpoint_request_stats ers on (enp.id=ers.endpoint_id)
     left join apis.anomalies ann on (ann.anomaly_type='endpoint' AND target_hash=endpoint_hash)
     where enp.project_id=? and enp.id is not null and ann.id is not null AND enp.outgoing = true AND enp.host = ?
     order by total_requests DESC, enp.url_path ASC;
    |]


-- FIXME: return endpoint_hash as well.
-- This would require tampering with the view.
endpointRequestStatsByEndpoint :: EndpointId -> PgT.DBT IO (Maybe EndpointRequestStats)
endpointRequestStatsByEndpoint eid = queryOne q (eid, eid)
  where
    q =
      [sql| SELECT endpoint_id, endpoint_hash, project_id, url_path, method, host, coalesce(min, 0), coalesce(p50, 0), coalesce(p75, 0), coalesce(p90, 0), coalesce(p95, 0), coalesce(p99, 0), coalesce
      (max, 0),
                   coalesce(total_time, 0), coalesce(total_time_proj,0), coalesce(total_requests,0), coalesce(total_requests_proj,0),
                   (SELECT count(*) from apis.anomalies
                           where endpoint_id=? AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
                   ) ongoing_anomalies,
                  (SELECT count(*) from apis.anomalies
                           where project_id=project_id AND acknowleged_at is null AND archived_at is null AND anomaly_type != 'field'
                   ) ongoing_anomalies_proj,
                  null, null, '00000000-0000-0000-0000-000000000000'::uuid
              FROM apis.endpoint_request_stats WHERE endpoint_id=?|]


endpointById :: EndpointId -> PgT.DBT IO (Maybe Endpoint)
endpointById eid = queryOne q (Only eid)
  where
    q = [sql| SELECT id, created_at, updated_at, project_id, url_path, url_params, method, host, hash, outgoing, description from apis.endpoints where id=? |]


endpointByHash :: Projects.ProjectId -> Text -> PgT.DBT IO (Maybe Endpoint)
endpointByHash pid hash = queryOne q (pid, hash)
  where
    q = [sql| SELECT id, created_at, updated_at, project_id, url_path, url_params, method, host, hash, outgoing, description from apis.endpoints where project_id=? AND hash=? |]


endpointsByHashes :: Projects.ProjectId -> V.Vector Text -> PgT.DBT IO (V.Vector Endpoint)
endpointsByHashes pid hashes
  | V.null hashes = pure V.empty
  | otherwise = query q (pid, hashes)
  where
    q = [sql| SELECT id, created_at, updated_at, project_id, url_path, url_params, method, host, hash, outgoing, description from apis.endpoints where project_id=? AND hash=ANY(?)|]


getEndpointsByAnomalyTargetHash :: Projects.ProjectId -> V.Vector Text -> PgT.DBT IO (V.Vector Host)
getEndpointsByAnomalyTargetHash pid hashes
  | V.null hashes = pure V.empty
  | otherwise = query q (pid, prefixHashes)
  where
    q = [sql|select distinct host from apis.endpoints where project_id=? AND hash LIKE ANY(?)|]
    prefixHashes = (<> "%") <$> hashes


data SwEndpoint = SwEndpoint
  { urlPath :: Text
  , urlParams :: AE.Value -- Key value map of key to the type. Needs a bit more figuring out.
  , method :: Text
  , host :: Text
  , hash :: Text
  , description :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (FromField) via Aeson SwEndpoint
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwEndpoint


data HostEvents = HostEvents
  { host :: Text
  , eventCount :: Integer
  , first_seen :: Maybe ZonedTime
  , last_seen :: Maybe ZonedTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)


endpointsByProjectId :: Projects.ProjectId -> Text -> PgT.DBT IO (V.Vector SwEndpoint)
endpointsByProjectId pid host = query q (pid, host)
  where
    q =
      [sql|
         SELECT url_path, url_params, method, host, hash, description
         FROM apis.endpoints enp
         INNER JOIN
         apis.anomalies ann ON (ann.anomaly_type = 'endpoint' AND ann.target_hash = enp.hash)
         WHERE enp.project_id = ?
           AND enp.host = ?
           AND ann.acknowleged_at IS NOT NULL
       |]


insertEndpoints :: [Endpoint] -> DBT IO Int64
insertEndpoints endpoints = do
  let q =
        [sql|
        INSERT INTO apis.endpoints
        (project_id, url_path, url_params, method, host, hash, description)
        VALUES (?,?,?,?,?,?,?) ON CONFLICT (hash) DO UPDATE SET
         description = CASE WHEN EXCLUDED.description <> '' THEN EXCLUDED.description ELSE apis.endpoints.description END;
      |]
  let params = map getEndpointParams endpoints
  executeMany q params
  where
    getEndpointParams :: Endpoint -> (Projects.ProjectId, Text, AE.Value, Text, Text, Text, Text)
    getEndpointParams endpoint =
      ( endpoint.projectId
      , endpoint.urlPath
      , endpoint.urlParams
      , endpoint.method
      , endpoint.host
      , endpoint.hash
      , endpoint.description
      )


getProjectHosts :: Projects.ProjectId -> PgT.DBT IO (V.Vector Host)
getProjectHosts pid = query q (Only pid)
  where
    q = [sql| SELECT DISTINCT host FROM apis.endpoints where  project_id = ? AND outgoing=false AND host!= '' |]


dependenciesAndEventsCount :: Projects.ProjectId -> Text -> Text -> DBT IO (V.Vector HostEvents)
dependenciesAndEventsCount pid requestType sortT = query (Query $ encodeUtf8 q) (pid, isOutgoing, pid)
  where
    orderBy = case sortT of
      "first_seen" -> "first_seen ASC;"
      "last_seen" -> "last_seen DESC;"
      _ -> "eventsCount DESC;"

    endpointFilter = case requestType of
      "Outgoing" -> "ep.outgoing = true"
      "Incoming" -> "ep.outgoing = false"
      _ -> "ep.outgoing =  false"

    isOutgoing = requestType == "Outgoing"

    q =
      [text|
WITH filtered_requests AS (
    SELECT COALESCE(attributes->>'net.host.name', attributes->>'http.host', '') AS host,
           COUNT(*) AS eventsCount,
           MAX(timestamp) AS last_seen,
           MIN(timestamp) AS first_seen
    FROM otel_logs_and_spans
    WHERE project_id = ?
      AND timestamp > NOW() - interval '14' day
      AND kind = CASE WHEN ? THEN 'client' ELSE 'server' END
      AND (attributes->>'http.request.method' IS NOT NULL 
           OR attributes->>'http.response.status_code' IS NOT NULL)
    GROUP BY 1
)
SELECT DISTINCT ep.host,
       COALESCE(fr.eventsCount, 0) eventsCount,
       fr.last_seen last_seen,
       fr.first_seen first_seen
FROM apis.endpoints ep
LEFT JOIN filtered_requests fr ON ep.host = fr.host
WHERE ep.project_id = ?
  AND ep.host != ''
  AND $endpointFilter
  ORDER BY $orderBy
      |]


countEndpointInbox :: Projects.ProjectId -> Text -> Text -> DBT IO Int
countEndpointInbox pid host requestType = do
  result <- query (Query $ encodeUtf8 q) (pid, host)
  case result of
    [Only count] -> return count
    v -> return $ length v
  where
    showCountBaseOnRequestType = case requestType of
      "Outgoing" -> "enp.outgoing = true"
      "Incoming" -> "enp.outgoing = false"
      _ -> "ep.outgoing =  false"
    q =
      [text|
        SELECT COUNT(*)
        FROM apis.endpoints enp
        LEFT JOIN apis.issues ann ON (ann.anomaly_type = 'endpoint' AND ann.target_hash = enp.hash)
        WHERE
            enp.project_id = ?
            AND $showCountBaseOnRequestType
            AND ann.id IS NOT NULL
            AND ann.acknowleged_at IS NULL
            AND host = ?
     |]
