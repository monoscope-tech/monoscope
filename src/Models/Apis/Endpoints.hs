module Models.Apis.Endpoints (
  Endpoint (..),
  EndpointId,
  EndpointRequestStats (..),
  Host (..),
  HostEvents (..),
  bulkInsertEndpoints,
  dependenciesAndEventsCount,
  endpointRequestStatsByProject,
  countEndpointInbox,
  countEndpointsByProject,
  getEndpointById,
  -- Endpoint template discovery
  getUnmergedEndpoints,
  setEndpointCanonical,
  insertCanonicalEndpoints,
  -- Endpoint embedding + merge
  getUnembeddedEndpoints,
  fetchEndpointTexts,
  updateEndpointEmbeddings,
  getCanonicalEndpoints,
  assignEndpointsToCanonical,
  setEndpointCanonicalTemplate,
  getMergedEndpointPairs,
  migrateAndDeleteMergedEndpoints,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Effectful.Hasql qualified as Hasql
import Data.Map.Lazy qualified as Map
import Data.Time (UTCTime, ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.DeriveUtils (UUIDId (..), rawSql, showPGFloatArray)
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
  , serviceName :: Maybe Text
  , environment :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, NFData, ToRow)
  deriving (FromField) via Aeson Endpoint
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint


bulkInsertEndpoints :: DB es => V.Vector Endpoint -> Eff es ()
bulkInsertEndpoints eps | V.null eps = pass
bulkInsertEndpoints eps =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing, service_name, environment)
           SELECT * FROM unnest(#{pids}::uuid[], #{paths}::text[], #{params}::jsonb[], #{methods}::text[], #{hosts}::text[], #{hashes}::text[], #{outs}::bool[], #{svcs}::text[], #{envs}::text[])
           ON CONFLICT (hash) DO NOTHING |]
  where
    pids = V.map (.projectId) eps
    paths = V.map (.urlPath) eps
    params = V.map (.urlParams) eps
    methods = V.map (.method) eps
    hosts = V.map (.host) eps
    hashes = V.map (.hash) eps
    outs = V.map (.outgoing) eps
    svcs = V.map (.serviceName) eps
    envs = V.map (.environment) eps


-- Based of a view which is generated every 5minutes.
data EndpointRequestStats = EndpointRequestStats
  { endpointId :: EndpointId
  , endpointHash :: Text
  , projectId :: Projects.ProjectId
  , urlPath :: Text
  , method :: Text
  , host :: Text
  , totalRequests :: Int
  , lastSeen :: Maybe ZonedTime
  , activityBuckets :: PGArray Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, ToRow)


-- Shared SQL fragment: resolve the remote host from various OTEL attribute locations
hostCoalesceExpr :: Text
hostCoalesceExpr = "COALESCE(attributes___server___address, attributes->'net'->'host'->>'name', attributes->'http'->>'host', NULLIF(split_part(split_part(split_part(attributes___url___full, '://', 2), '/', 1), ':', 1), ''), resource___service___name, '')"


periodBuckets :: Text -> (Text, Text)
periodBuckets "7d" = ("(CURRENT_DATE - INTERVAL '6 days')", "7")
periodBuckets _ = ("(NOW() - INTERVAL '23 hours')", "24")


-- FIXME: Include and return a boolean flag to show if fields that have annomalies.
-- FIXME: return endpoint_hash as well.
endpointRequestStatsByProject :: DB es => Projects.ProjectId -> Bool -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Text -> Text -> Eff es (V.Vector EndpointRequestStats)
endpointRequestStatsByProject pid _ackd _archived pHostM sortM searchM page requestType period = V.fromList <$> Hasql.interp query
  where
    isOutgoing = requestType == "Outgoing"
    offset = page * 30
    (seriesStart, numBuckets) = periodBuckets period
    hostFilter = maybe "" (\h -> [HI.sql| AND (|] <> rawSql hostCoalesceExpr <> [HI.sql| = #{h})|]) pHostM
    pHostQuery = maybe "" (\h -> [HI.sql| AND enp.host = #{h}|]) pHostM
    search = maybe "" (\s -> let pat = "%" <> s <> "%" in [HI.sql| AND enp.url_path LIKE #{pat}|]) searchM
    orderBy = case fromMaybe "" sortM of "first_seen" -> "enp.created_at ASC"; "last_seen" -> "enp.created_at DESC"; _ -> "coalesce(es.eventsCount, 0) DESC"
    seriesStartSql = rawSql seriesStart
    numBucketsSql = rawSql numBuckets
    query =
      "WITH combined AS (\
      \ SELECT COALESCE(NULLIF(attributes->'http'->>'route', ''), attributes___url___path, '/') AS url_path,\
      \        attributes___http___request___method AS method,\
      \        COUNT(*) AS eventsCount,\
      \        MAX(timestamp) AS last_seen,\
      \        LEAST(width_bucket(EXTRACT(EPOCH FROM timestamp), EXTRACT(EPOCH FROM "
        <> seriesStartSql
        <> "::timestamptz), EXTRACT(EPOCH FROM NOW()), "
        <> numBucketsSql
        <> "), "
        <> numBucketsSql
        <> ") AS bucket_idx,\
           \        COUNT(*)::int AS cnt\
           \ FROM otel_logs_and_spans\
           \ WHERE project_id = "
        <> [HI.sql|#{pid}::text|]
        <> " AND attributes___http___request___method IS NOT NULL"
        <> hostFilter
        <> " AND timestamp >= "
        <> seriesStartSql
        <> "::timestamptz\
           \ GROUP BY url_path, method, bucket_idx\
           \), endpoint_stats AS (\
           \ SELECT url_path, method, SUM(eventsCount)::bigint AS eventsCount, MAX(last_seen) AS last_seen FROM combined GROUP BY url_path, method\
           \), bucketed_agg AS (\
           \ SELECT c.url_path, c.method, ARRAY_AGG(COALESCE(c2.cnt, 0) ORDER BY s.idx) AS activity_buckets\
           \ FROM (SELECT DISTINCT url_path, method FROM combined) c\
           \ CROSS JOIN generate_series(1, "
        <> numBucketsSql
        <> ") AS s(idx)\
           \ LEFT JOIN combined c2 ON c2.url_path = c.url_path AND c2.method = c.method AND c2.bucket_idx = s.idx\
           \ GROUP BY c.url_path, c.method\
           \) SELECT enp.id endpoint_id, enp.hash endpoint_hash, enp.project_id, enp.url_path, enp.method, enp.host,\
           \        coalesce(es.eventsCount, 0) as total_requests, es.last_seen,\
           \        COALESCE(ba.activity_buckets, ARRAY[]::int[]) AS activity_buckets\
           \ FROM apis.endpoints enp\
           \ LEFT JOIN endpoint_stats es ON (enp.url_path=es.url_path AND enp.method=es.method)\
           \ LEFT JOIN bucketed_agg ba ON (enp.url_path=ba.url_path AND enp.method=ba.method)\
           \ WHERE enp.project_id="
        <> [HI.sql|#{pid}|]
        <> " AND enp.outgoing="
        <> [HI.sql|#{isOutgoing}|]
        <> pHostQuery
        <> search
        <> " ORDER BY "
        <> rawSql orderBy
        <> ", url_path ASC OFFSET "
        <> [HI.sql|#{offset}|]
        <> " LIMIT 30"


data HostEvents = HostEvents
  { host :: Text
  , eventCount :: Int64
  , first_seen :: Maybe ZonedTime
  , last_seen :: Maybe ZonedTime
  , activityBuckets :: PGArray Int
  , services :: PGArray Text -- distinct resource.service.name emitting traffic for this host
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, ToRow)


dependenciesAndEventsCount :: (DB es, Time :> es) => Projects.ProjectId -> Text -> Text -> Int -> Text -> Text -> Eff es [HostEvents]
dependenciesAndEventsCount pid requestType sortT skip timeF period = do
  now <- Time.currentTime
  let timeRangeSql = case timeF of
        "14D" -> [HI.sql|timestamp > #{now}::timestamptz - interval '14 day'|]
        _ -> [HI.sql|timestamp > #{now}::timestamptz - interval '1 day'|]
      hostExpr = hostCoalesceExpr
      (seriesStart, numBuckets) = periodBuckets period
      orderBy = case sortT of
        "first_seen" -> "first_seen ASC"
        "last_seen" -> "last_seen DESC"
        _ -> "eventsCount DESC"
      endpointFilter = case requestType of
        "Outgoing" -> "outgoing = true"
        "Incoming" -> "outgoing = false"
        _ -> "outgoing = false"
      isOutgoing = requestType == "Outgoing"
  Hasql.interp
    $ rawSql
      [text|
WITH endpoint_hosts AS (
    SELECT DISTINCT host FROM apis.endpoints
    WHERE project_id = |]
    <> [HI.sql|#{pid} |]
    <> rawSql
      [text| AND host != '' AND $endpointFilter
), combined AS (
    SELECT $hostExpr AS host,
           resource___service___name AS service,
           MAX(timestamp) AS last_seen,
           MIN(timestamp) AS first_seen,
           LEAST(width_bucket(EXTRACT(EPOCH FROM timestamp), EXTRACT(EPOCH FROM $seriesStart::timestamptz), EXTRACT(EPOCH FROM NOW()), $numBuckets), $numBuckets) AS bucket_idx,
           COUNT(*)::int AS cnt
    FROM otel_logs_and_spans
    WHERE project_id = |]
    <> [HI.sql|#{pid}::text AND |]
    <> timeRangeSql
    <> [HI.sql| AND attributes___http___request___method IS NOT NULL AND kind IN (CASE WHEN #{isOutgoing} THEN 'client' ELSE 'server' END, CASE WHEN #{isOutgoing} THEN NULL ELSE 'internal' END) GROUP BY host, service, bucket_idx
), host_stats AS (
    SELECT host,
           SUM(cnt)::bigint AS eventsCount,
           MAX(last_seen) AS last_seen,
           MIN(first_seen) AS first_seen,
           COALESCE(ARRAY_AGG(DISTINCT service) FILTER (WHERE service IS NOT NULL AND service != '' AND service != host), ARRAY[]::text[]) AS services
    FROM combined GROUP BY host
), bucketed_agg AS (|]
    <> rawSql
      [text|
    SELECT c.host, ARRAY_AGG(COALESCE(c2.cnt, 0) ORDER BY s.idx) AS activity_buckets
    FROM (SELECT DISTINCT host FROM combined) c
    CROSS JOIN generate_series(1, $numBuckets) AS s(idx)
    LEFT JOIN (SELECT host, bucket_idx, SUM(cnt)::int AS cnt FROM combined GROUP BY host, bucket_idx) c2
      ON c2.host = c.host AND c2.bucket_idx = s.idx
    GROUP BY c.host
)
SELECT eh.host, COALESCE(hs.eventsCount, 0) AS eventsCount, hs.last_seen, hs.first_seen,
       COALESCE(ba.activity_buckets, ARRAY[]::int[]) AS activity_buckets,
       COALESCE(hs.services, ARRAY[]::text[]) AS services
FROM endpoint_hosts eh
LEFT JOIN host_stats hs ON eh.host = hs.host
LEFT JOIN bucketed_agg ba ON eh.host = ba.host
ORDER BY $orderBy
LIMIT 20 OFFSET |]
    <> [HI.sql| #{skip}|]


countEndpointInbox :: DB es => Projects.ProjectId -> Text -> Text -> Eff es Int
countEndpointInbox pid host requestType = do
  let showCountBaseOnRequestType = case requestType of
        "Outgoing" -> "enp.outgoing = true"
        "Incoming" -> "enp.outgoing = false"
        _ -> "ep.outgoing =  false"
  fromMaybe 0
    <$> Hasql.interpOne
      ( [HI.sql|SELECT coalesce(COUNT(*)::INT, 0)
        FROM apis.endpoints enp
        LEFT JOIN apis.issues ann ON (ann.issue_type = 'api_change' AND ann.endpoint_hash = enp.hash)
        WHERE enp.project_id = #{pid} AND |]
          <> rawSql showCountBaseOnRequestType
          <> [HI.sql| AND ann.id IS NOT NULL AND ann.acknowledged_at IS NULL AND host = #{host}|]
      )


countEndpointsByProject :: DB es => Projects.ProjectId -> Bool -> Eff es Int
countEndpointsByProject pid outgoing =
  fromMaybe 0
    <$> Hasql.interpOne
      [HI.sql| SELECT COUNT(*)::int FROM apis.endpoints WHERE project_id = #{pid} AND outgoing = #{outgoing} |]


getEndpointById :: DB es => Projects.ProjectId -> EndpointId -> Eff es (Maybe Endpoint)
getEndpointById pid eid =
  Hasql.interpOne
    [HI.sql| SELECT id, created_at, updated_at, project_id, url_path, url_params, method, host, hash, outgoing, description, service_name, environment
             FROM apis.endpoints WHERE id = #{eid} AND project_id = #{pid} |]


-- Endpoint template discovery ---------------------------------------------------

getUnmergedEndpoints :: DB es => Projects.ProjectId -> Eff es [(Text, Text, Text, Text)]
getUnmergedEndpoints pid =
  Hasql.interp
    [HI.sql| SELECT hash, method, host, url_path FROM apis.endpoints
        WHERE project_id = #{pid} AND canonical_hash IS NULL AND merge_override = FALSE
        ORDER BY created_at ASC LIMIT 5000 |]


setEndpointCanonical :: DB es => [(Text, Text, Text)] -> Eff es Int64
setEndpointCanonical [] = pure 0
setEndpointCanonical triples =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.endpoints SET canonical_hash = u.chash, canonical_path = u.cpath
        FROM (SELECT unnest(#{hashesV}::text[]) AS hash, unnest(#{chashesV}::text[]) AS chash, unnest(#{cpathsV}::text[]) AS cpath) u
        WHERE apis.endpoints.hash = u.hash |]
  where
    (hashes, chashes, cpaths) = unzip3 triples
    (hashesV, chashesV, cpathsV) = (V.fromList hashes, V.fromList chashes, V.fromList cpaths)


insertCanonicalEndpoints :: DB es => [(Projects.ProjectId, Text, Text, Text, Text)] -> Eff es ()
insertCanonicalEndpoints [] = pass
insertCanonicalEndpoints rows =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing, canonical_hash, canonical_path)
           SELECT p, tp, '{}'::jsonb, m, h, eh, FALSE, eh, tp
           FROM unnest(#{pidsV}::uuid[], #{tplsV}::text[], #{methodsV}::text[], #{hostsV}::text[], #{hashesV}::text[]) AS t(p, tp, m, h, eh)
           ON CONFLICT (hash) DO UPDATE SET canonical_hash = EXCLUDED.canonical_hash, canonical_path = EXCLUDED.canonical_path |]
  where
    v = V.fromList rows
    pidsV = V.map (\(p, _, _, _, _) -> p) v
    tplsV = V.map (\(_, t, _, _, _) -> t) v
    methodsV = V.map (\(_, _, m, _, _) -> m) v
    hostsV = V.map (\(_, _, _, h, _) -> h) v
    hashesV = V.map (\(_, _, _, _, e) -> e) v


-- Endpoint embedding + merge ---------------------------------------------------

fetchEndpointTexts :: DB es => [EndpointId] -> Eff es (Map EndpointId Text)
fetchEndpointTexts [] = pure mempty
fetchEndpointTexts ids =
  Map.fromList
    <$> Hasql.interp [HI.sql| SELECT id, url_path FROM apis.endpoints WHERE id = ANY(#{V.fromList ids}) |]


getUnembeddedEndpoints :: DB es => Projects.ProjectId -> Eff es [(EndpointId, Text, Text)]
getUnembeddedEndpoints pid =
  Hasql.interp
    [HI.sql| SELECT id, hash, url_path FROM apis.endpoints
        WHERE project_id = #{pid} AND embedding IS NULL AND merge_override = FALSE
          AND url_path != ''
        LIMIT 500 |]


updateEndpointEmbeddings :: DB es => [(EndpointId, [Float])] -> Eff es Int64
updateEndpointEmbeddings [] = pure 0
updateEndpointEmbeddings pairs =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.endpoints SET
          embedding = u.emb::float4[],
          embedding_at = NOW()
        FROM ROWS FROM (unnest(#{idsV}::uuid[]), unnest(#{embsV}::text[])) AS u(id, emb)
        WHERE apis.endpoints.id = u.id |]
  where
    (ids, embs) = unzip $ map (second showPGFloatArray) pairs
    (idsV, embsV) = (V.fromList ids, V.fromList embs)


getCanonicalEndpoints :: DB es => Projects.ProjectId -> Eff es [(EndpointId, [Float])]
getCanonicalEndpoints pid =
  map (\(eid, emb :: V.Vector Float) -> (eid, V.toList emb))
    <$> Hasql.interp
      [HI.sql| SELECT id, embedding FROM apis.endpoints
        WHERE project_id = #{pid} AND canonical_hash IS NULL
          AND embedding IS NOT NULL AND merge_override = FALSE
        LIMIT 10000 |]


assignEndpointsToCanonical :: DB es => [(EndpointId, EndpointId)] -> Eff es Int64
assignEndpointsToCanonical [] = pure 0
assignEndpointsToCanonical pairs =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.endpoints e SET canonical_hash = canon.hash
        FROM (SELECT unnest(#{pidsV}::uuid[]) AS id, unnest(#{cidsV}::uuid[]) AS canonical) u
        JOIN apis.endpoints canon ON canon.id = u.canonical
        WHERE e.id = u.id |]
  where
    (pids, cids) = unzip pairs
    (pidsV, cidsV) = (V.fromList pids, V.fromList cids)


-- | Set a centroid endpoint's url_path and canonical_path to an LLM-suggested template.
setEndpointCanonicalTemplate :: DB es => EndpointId -> Text -> Eff es Int64
setEndpointCanonicalTemplate eid ePath =
  Hasql.interpExecute [HI.sql| UPDATE apis.endpoints SET url_path = #{ePath}, canonical_path = #{ePath} WHERE id = #{eid} |]


getMergedEndpointPairs :: DB es => Projects.ProjectId -> Eff es [(Text, Text)]
getMergedEndpointPairs pid =
  Hasql.interp
    [HI.sql| SELECT hash, canonical_hash FROM apis.endpoints
          WHERE project_id = #{pid} AND canonical_hash IS NOT NULL AND hash != canonical_hash
          LIMIT 10000 |]


-- | Migrate shapes/fields/formats from old endpoints to canonical ones, remap anomalies/issues, then delete old data.
migrateAndDeleteMergedEndpoints :: DB es => [(Text, Text)] -> Eff es ()
migrateAndDeleteMergedEndpoints [] = pass
migrateAndDeleteMergedEndpoints pairs = do
  let (oldHashes, canonHashes) = unzip pairs
      oldArr = V.fromList oldHashes
      canonArr = V.fromList canonHashes
  -- Step 1: Migrate shapes (prefix-replace endpoint_hash and hash, remap field_hashes array)
  Hasql.interpExecute_
    [HI.sql|
      INSERT INTO apis.shapes (id, created_at, updated_at, project_id, endpoint_hash, hash,
            field_hashes, query_params_keypaths, request_body_keypaths, response_body_keypaths,
            request_headers_keypaths, response_headers_keypaths, status_code,
            response_description, request_description, new_unique_fields, deleted_fields, updated_field_formats)
          SELECT gen_random_uuid(), s.created_at, s.updated_at, s.project_id,
            m.canonical, m.canonical || substring(s.hash FROM 9),
            array(SELECT m2.canonical || substring(fh FROM 9)
                  FROM unnest(s.field_hashes) fh
                  LEFT JOIN unnest(#{oldArr}::text[], #{canonArr}::text[]) m2(old, canonical) ON LEFT(fh, 8) = m2.old),
            s.query_params_keypaths, s.request_body_keypaths, s.response_body_keypaths,
            s.request_headers_keypaths, s.response_headers_keypaths, s.status_code,
            s.response_description, s.request_description, s.new_unique_fields, s.deleted_fields, s.updated_field_formats
          FROM apis.shapes s
          JOIN unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical) ON s.endpoint_hash = m.old
          ON CONFLICT (hash) DO NOTHING |]
  -- Step 2: Migrate fields
  Hasql.interpExecute_
    [HI.sql|
      INSERT INTO apis.fields (id, created_at, updated_at, project_id, endpoint_hash, key,
            field_type, field_type_override, format, format_override, description, key_path,
            field_category, hash, is_enum, is_required)
          SELECT gen_random_uuid(), f.created_at, f.updated_at, f.project_id,
            m.canonical, f.key, f.field_type, f.field_type_override, f.format, f.format_override,
            f.description, f.key_path, f.field_category,
            m.canonical || substring(f.hash FROM 9),
            f.is_enum, f.is_required
          FROM apis.fields f
          JOIN unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical) ON f.endpoint_hash = m.old
          ON CONFLICT (hash) DO NOTHING |]
  -- Step 3: Migrate formats
  Hasql.interpExecute_
    [HI.sql|
      INSERT INTO apis.formats (id, created_at, updated_at, project_id, field_hash, field_type,
            field_format, examples, hash)
          SELECT gen_random_uuid(), fmt.created_at, fmt.updated_at, fmt.project_id,
            m.canonical || substring(fmt.field_hash FROM 9),
            fmt.field_type, fmt.field_format, fmt.examples,
            m.canonical || substring(fmt.hash FROM 9)
          FROM apis.formats fmt
          JOIN unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical) ON LEFT(fmt.field_hash, 8) = m.old
          ON CONFLICT (hash) DO NOTHING |]
  -- Step 4: Remap anomalies (skip if canonical target already exists for same project)
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.anomalies a
          SET target_hash = m.canonical || substring(a.target_hash FROM 9)
          FROM unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical)
          WHERE LEFT(a.target_hash, 8) = m.old
            AND NOT EXISTS (SELECT 1 FROM apis.anomalies a2
                           WHERE a2.project_id = a.project_id
                             AND a2.target_hash = m.canonical || substring(a.target_hash FROM 9)) |]
  -- Step 5: Remap issues (skip if canonical target already exists for same project+type)
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.issues i
          SET endpoint_hash = m.canonical,
              target_hash = m.canonical || substring(i.target_hash FROM 9)
          FROM unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical)
          WHERE LEFT(i.target_hash, 8) = m.old
            AND NOT EXISTS (SELECT 1 FROM apis.issues i2
                           WHERE i2.project_id = i.project_id
                             AND i2.target_hash = m.canonical || substring(i.target_hash FROM 9)
                             AND i2.issue_type = i.issue_type
                             AND i2.acknowledged_at IS NULL AND i2.archived_at IS NULL) |]
  -- Step 6: Delete old data (reverse dependency order)
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.formats WHERE LEFT(field_hash, 8) = ANY(#{oldArr}) |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.fields WHERE endpoint_hash = ANY(#{oldArr}) |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.shapes WHERE endpoint_hash = ANY(#{oldArr}) |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.anomalies WHERE LEFT(target_hash, 8) = ANY(#{oldArr}) |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.issues WHERE LEFT(target_hash, 8) = ANY(#{oldArr}) |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.endpoints WHERE hash = ANY(#{oldArr}) |]
