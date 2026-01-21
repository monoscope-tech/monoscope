{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Models.Apis.Endpoints (
  Endpoint (..),
  SwEndpoint (..),
  EndpointId,
  EndpointRequestStats (..),
  Host (..),
  HostEvents (..),
  EndpointWithCurrentRates (..),
  EndpointStats (..),
  bulkInsertEndpoints,
  dependenciesAndEventsCount,
  endpointRequestStatsByProject,
  countEndpointInbox,
  getEndpointsWithCurrentRates,
  getEndpointStats,
  updateEndpointBaseline,
  getEndpointByHash,
  getActiveEndpoints,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (UTCTime, ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL (withConnection)
import Effectful.PostgreSQL qualified as PG
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.DeriveUtils (BaselineState (..), UUIDId (..))
import Relude
import System.Types (DB)


type EndpointId = UUIDId "endpoint"


newtype Host = Host {host :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)


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
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  , service :: Maybe Text
  , baselineState :: BaselineState
  , baselineSamples :: Int
  , baselineUpdatedAt :: Maybe UTCTime
  , baselineErrorRateMean :: Maybe Double
  , baselineErrorRateStddev :: Maybe Double
  , baselineLatencyMean :: Maybe Double
  , baselineLatencyStddev :: Maybe Double
  , baselineLatencyP95 :: Maybe Double
  , baselineLatencyP99 :: Maybe Double
  , baselineVolumeHourlyMean :: Maybe Double
  , baselineVolumeHourlyStddev :: Maybe Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (FromField) via Aeson Endpoint
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint


bulkInsertEndpoints :: DB es => V.Vector Endpoint -> Eff es ()
bulkInsertEndpoints endpoints = void $ PG.executeMany q $ V.toList rowsToInsert
  where
    q =
      [sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing, first_trace_id, recent_trace_id, service)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (hash) DO  NOTHING;
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
        , endpoint.firstTraceId
        , endpoint.recentTraceId
        , endpoint.service
        )


-- Based of a view which is generated every 5minutes.
data EndpointRequestStats = EndpointRequestStats
  { endpointId :: EndpointId
  , endpointHash :: Text
  , projectId :: Projects.ProjectId
  , urlPath :: Text
  , method :: Text
  , host :: Text
  , totalRequests :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "endpoint_request_stats", PrimaryKey "endpoint_id", FieldModifiers '[CamelToSnake]] EndpointRequestStats)


-- FIXME: Include and return a boolean flag to show if fields that have annomalies.
-- FIXME: return endpoint_hash as well.
endpointRequestStatsByProject :: DB es => Projects.ProjectId -> Bool -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Text -> Eff es (V.Vector EndpointRequestStats)
endpointRequestStatsByProject pid ackd archived pHostM sortM searchM page requestType = withConnection \conn -> liftIO $ V.fromList <$> PGS.query conn (Query $ encodeUtf8 q) queryParams
  where
    -- Construct the list of parameters conditionally
    pHostParams = maybe [] (\h -> [toField h]) pHostM
    queryParams = [toField pid] ++ pHostParams ++ [toField pid, toField isOutgoing] ++ pHostParams ++ [toField offset]

    isOutgoing = requestType == "Outgoing"
    offset = page * 30
    -- Todo: FIX anomaly trigger and enable anomaly joins
    -- ackdAt = if ackd && not archived then "AND ann.acknowledged_at IS NOT NULL AND ann.archived_at IS NULL " else "AND ann.acknowledged_at IS NULL "
    -- archivedAt = if archived then "AND ann.archived_at IS NOT NULL " else "AND ann.archived_at IS NULL "
    search = case searchM of Just s -> " AND enp.url_path LIKE '%" <> s <> "%'"; Nothing -> ""
    pHostQuery = case pHostM of Just h -> " AND enp.host = ?"; Nothing -> ""
    hostFilter = case pHostM of Just h -> " AND attributes->'net'->'host'->>'name' = ?"; Nothing -> ""
    orderBy = case fromMaybe "" sortM of "first_seen" -> "enp.created_at ASC"; "last_seen" -> "enp.created_at DESC"; _ -> "coalesce(fr.eventsCount, 0) DESC"
    q =
      [text| 
      
   WITH filtered_requests AS (
    SELECT attributes->'http'->>'route' AS url_path,
           attributes___http___request___method AS method,
           COUNT(*) AS eventsCount
    FROM otel_logs_and_spans
    WHERE project_id = ? AND name = 'monoscope.http' $hostFilter
    GROUP BY url_path, method
)
      SELECT enp.id endpoint_id, enp.hash endpoint_hash, enp.project_id, enp.url_path, enp.method, enp.host, coalesce(fr.eventsCount, 0) as total_requests
     from apis.endpoints enp
     left join filtered_requests fr on (enp.url_path=fr.url_path and enp.method=fr.method)
     where enp.project_id=? and enp.outgoing=? $pHostQuery $search
     order by $orderBy , url_path ASC
     offset ? limit 30;
     
  |]


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


dependenciesAndEventsCount :: DB es => Projects.ProjectId -> Text -> Text -> Int -> Text -> Eff es [HostEvents]
dependenciesAndEventsCount pid requestType sortT skip timeF = PG.query (Query $ encodeUtf8 q) (pid, isOutgoing, isOutgoing, pid, skip)
  where
    orderBy = case sortT of
      "first_seen" -> "first_seen ASC"
      "last_seen" -> "last_seen DESC"
      _ -> "eventsCount DESC"

    timeRange = case timeF of
      "14D" -> "timestamp > now() - interval '14 day'"
      _ -> "timestamp > now() - interval '1 day'"

    endpointFilter = case requestType of
      "Outgoing" -> "ep.outgoing = true"
      "Incoming" -> "ep.outgoing = false"
      _ -> "ep.outgoing = false"

    isOutgoing = requestType == "Outgoing"

    q =
      [text|
WITH filtered_requests AS (
    SELECT attributes->'net'->'host'->>'name' AS host,
           COUNT(*) AS eventsCount,
           MAX(timestamp) AS last_seen,
           MIN(timestamp) AS first_seen
    FROM otel_logs_and_spans
    WHERE project_id = ? 
      AND $timeRange
      AND (name = 'monoscope.http' OR name = 'apitoolkit-http-span')
      AND kind IN (CASE  WHEN ? THEN 'client' ELSE 'server' END, CASE  WHEN ? THEN NULL ELSE 'internal' END)
    GROUP BY host
)
SELECT DISTINCT ep.host,
       COALESCE(fr.eventsCount, 0) AS eventsCount,
       fr.last_seen,
       fr.first_seen
FROM apis.endpoints ep
LEFT JOIN filtered_requests fr ON ep.host = fr.host
WHERE ep.project_id = ?
  AND ep.host != ''
  AND $endpointFilter
ORDER BY $orderBy
LIMIT 20 OFFSET ?
      |]


countEndpointInbox :: DB es => Projects.ProjectId -> Text -> Text -> Eff es Int
countEndpointInbox pid host requestType = do
  result <- PG.query (Query $ encodeUtf8 q) (pid, host)
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
        SELECT coalesce(COUNT(*), 0)
        FROM apis.endpoints enp
        LEFT JOIN apis.issues ann ON (ann.issue_type = 'api_change' AND ann.endpoint_hash = enp.hash)
        WHERE
            enp.project_id = ?
            AND $showCountBaseOnRequestType
            AND ann.id IS NOT NULL
            AND ann.acknowledged_at IS NULL
            AND host = ?
     |]


-- | Endpoint with current hourly rates for anomaly detection
data EndpointWithCurrentRates = EndpointWithCurrentRates
  { endpointId :: EndpointId
  , endpointHash :: Text
  , method :: Text
  , urlPath :: Text
  , host :: Text
  , baselineState :: BaselineState
  , baselineErrorRateMean :: Maybe Double
  , baselineErrorRateStddev :: Maybe Double
  , baselineLatencyMean :: Maybe Double
  , baselineLatencyStddev :: Maybe Double
  , baselineLatencyP95 :: Maybe Double
  , baselineLatencyP99 :: Maybe Double
  , baselineVolumeHourlyMean :: Maybe Double
  , baselineVolumeHourlyStddev :: Maybe Double
  , currentHourRequests :: Int
  , currentHourErrors :: Int
  , currentHourLatencyP50 :: Maybe Double
  , currentHourLatencyP95 :: Maybe Double
  , currentHourLatencyP99 :: Maybe Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData)


-- | Endpoint stats for baseline calculation
-- Using median + MAD instead of mean + stddev for robustness against outliers/spikes
data EndpointStats = EndpointStats
  { totalHours :: Int
  , hourlyMedianRequests :: Double -- Actually stores median for robustness
  , hourlyMADRequestsScaled :: Double -- Actually stores MAD * 1.4826 (scaled to be comparable to stddev)
  , hourlyMedianErrors :: Double -- Actually stores median error rate
  , hourlyMADErrorsScaled :: Double -- Actually stores MAD * 1.4826 for error rate
  , medianLatency :: Double -- Median latency
  , madLatencyScaled :: Double -- MAD * 1.4826 for latency
  , p95Latency :: Double
  , p99Latency :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData)


-- | Get endpoints with their current hourly rates for anomaly detection
getEndpointsWithCurrentRates :: DB es => Projects.ProjectId -> Eff es [EndpointWithCurrentRates]
getEndpointsWithCurrentRates pid = PG.query q (pid, pid)
  where
    q =
      [sql|
        WITH current_hour_stats AS (
          SELECT
            attributes->'http'->>'route' AS url_path,
            attributes___http___request___method AS method,
            COUNT(*) AS request_count,
            COUNT(*) FILTER (WHERE attributes___http___response___status_code::int >= 500 OR attributes___http___response___status_code::int = 0) AS error_count,
            PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY duration) AS p50_latency,
            PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY duration) AS p95_latency,
            PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY duration) AS p99_latency
          FROM otel_logs_and_spans
          WHERE project_id = ?
            AND name = 'monoscope.http'
            AND timestamp >= NOW() - INTERVAL '1 hour'
          GROUP BY url_path, method
        )
        SELECT
          e.id AS endpoint_id,
          e.hash AS endpoint_hash,
          e.method,
          e.url_path,
          e.host,
          e.baseline_state,
          e.baseline_error_rate_mean,
          e.baseline_error_rate_stddev,
          e.baseline_latency_mean,
          e.baseline_latency_stddev,
          e.baseline_latency_p95,
          e.baseline_latency_p99,
          e.baseline_volume_hourly_mean,
          e.baseline_volume_hourly_stddev,
          COALESCE(chs.request_count, 0)::int AS current_hour_requests,
          COALESCE(chs.error_count, 0)::int AS current_hour_errors,
          chs.p50_latency AS current_hour_latency_p50,
          chs.p95_latency AS current_hour_latency_p95,
          chs.p99_latency AS current_hour_latency_p99
        FROM apis.endpoints e
        LEFT JOIN current_hour_stats chs
          ON e.url_path = chs.url_path AND e.method = chs.method
        WHERE e.project_id = ?
          AND e.baseline_state = 'established'
      |]


-- | Get endpoint stats for baseline calculation over N hours
-- Returns median and MAD (Median Absolute Deviation) for robust baseline calculation
getEndpointStats :: DB es => Projects.ProjectId -> Text -> Int -> Eff es (Maybe EndpointStats)
getEndpointStats pid endpointHash hours = listToMaybe <$> PG.query q (pid, endpointHash, hours)
  where
    q =
      [sql|
        WITH hourly_stats AS (
          SELECT
            DATE_TRUNC('hour', timestamp) AS hour,
            COUNT(*) AS request_count,
            COUNT(*) FILTER (WHERE attributes___http___response___status_code::int >= 500 OR attributes___http___response___status_code::int = 0)::float / NULLIF(COUNT(*), 0) AS error_rate,
            AVG(duration) AS avg_latency,
            PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY duration) AS p95_latency,
            PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY duration) AS p99_latency
          FROM otel_logs_and_spans ols
          JOIN apis.endpoints e ON e.url_path = (ols.attributes->'http'->>'route')
            AND e.method = ols.attributes___http___request___method
          WHERE ols.project_id = ?
            AND e.hash = ?
            AND ols.name = 'monoscope.http'
            AND ols.timestamp >= NOW() - MAKE_INTERVAL(hours => ?)
          GROUP BY DATE_TRUNC('hour', timestamp)
        ),
        -- Median calculations
        medians AS (
          SELECT
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY request_count) AS median_requests,
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY error_rate) AS median_error_rate,
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY avg_latency) AS median_latency,
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY p95_latency) AS median_p95,
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY p99_latency) AS median_p99
          FROM hourly_stats
        ),
        -- MAD calculations (median absolute deviation)
        mads AS (
          SELECT
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(hs.request_count - m.median_requests)) AS mad_requests,
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(hs.error_rate - m.median_error_rate)) AS mad_error_rate,
            PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(hs.avg_latency - m.median_latency)) AS mad_latency
          FROM hourly_stats hs, medians m
        )
        SELECT
          (SELECT COUNT(*)::int FROM hourly_stats) AS total_hours,
          COALESCE(m.median_requests, 0)::float8 AS hourly_median_requests,
          COALESCE(mad.mad_requests * 1.4826, 0)::float8 AS hourly_mad_requests_scaled,
          COALESCE(m.median_error_rate, 0)::float8 AS hourly_median_errors,
          COALESCE(mad.mad_error_rate * 1.4826, 0)::float8 AS hourly_mad_errors_scaled,
          COALESCE(m.median_latency, 0)::float8 AS median_latency,
          COALESCE(mad.mad_latency * 1.4826, 0)::float8 AS mad_latency_scaled,
          COALESCE(m.median_p95, 0)::float8 AS p95_latency,
          COALESCE(m.median_p99, 0)::float8 AS p99_latency
        FROM medians m, mads mad
        WHERE (SELECT COUNT(*) FROM hourly_stats) > 0
      |]


-- | Update endpoint baseline values
updateEndpointBaseline :: DB es => EndpointId -> BaselineState -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int -> Eff es ()
updateEndpointBaseline eid bState errMean errStddev latMean latStddev latP95 latP99 volMean volStddev samples =
  void $ PG.execute q (bState, errMean, errStddev, latMean, latStddev, latP95, latP99, volMean, volStddev, samples, eid)
  where
    q =
      [sql|
        UPDATE apis.endpoints
        SET baseline_state = ?,
            baseline_error_rate_mean = ?,
            baseline_error_rate_stddev = ?,
            baseline_latency_mean = ?,
            baseline_latency_stddev = ?,
            baseline_latency_p95 = ?,
            baseline_latency_p99 = ?,
            baseline_volume_hourly_mean = ?,
            baseline_volume_hourly_stddev = ?,
            baseline_samples = ?,
            baseline_updated_at = NOW()
        WHERE id = ?
      |]


-- | Get endpoint by hash
getEndpointByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe Endpoint)
getEndpointByHash pid hash = listToMaybe <$> PG.query q (pid, hash)
  where
    q =
      [sql|
        SELECT id, created_at, updated_at, project_id, url_path, url_params, method, host, hash, outgoing, description,
               first_trace_id, recent_trace_id, service,
               baseline_state, baseline_samples, baseline_updated_at,
               baseline_error_rate_mean, baseline_error_rate_stddev,
               baseline_latency_mean, baseline_latency_stddev, baseline_latency_p95, baseline_latency_p99,
               baseline_volume_hourly_mean, baseline_volume_hourly_stddev
        FROM apis.endpoints
        WHERE project_id = ? AND hash = ?
      |]


-- | Get all active endpoints for a project (for baseline calculation)
getActiveEndpoints :: DB es => Projects.ProjectId -> Eff es [Endpoint]
getActiveEndpoints pid = PG.query q (Only pid)
  where
    q =
      [sql|
        SELECT id, created_at, updated_at, project_id, url_path, url_params, method, host, hash, outgoing, description,
               first_trace_id, recent_trace_id, service,
               baseline_state, baseline_samples, baseline_updated_at,
               baseline_error_rate_mean, baseline_error_rate_stddev,
               baseline_latency_mean, baseline_latency_stddev, baseline_latency_p95, baseline_latency_p99,
               baseline_volume_hourly_mean, baseline_volume_hourly_stddev
        FROM apis.endpoints
        WHERE project_id = ?
      |]
