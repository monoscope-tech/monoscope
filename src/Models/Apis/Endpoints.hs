module Models.Apis.Endpoints (
  Endpoint (..),
  SwEndpoint (..),
  EndpointId,
  EndpointRequestStats (..),
  Host (..),
  HostEvents (..),
  bulkInsertEndpoints,
  dependenciesAndEventsCount,
  endpointRequestStatsByProject,
  countEndpointInbox,
  -- Endpoint template discovery
  getUnmergedEndpoints,
  setEndpointCanonical,
  insertCanonicalEndpoints,
  -- Endpoint embedding + merge
  getUnembeddedEndpoints,
  getEmbeddedEndpointTexts,
  updateEndpointEmbeddings,
  getCanonicalEndpoints,
  assignEndpointsToCanonical,
  unmergeEndpoint,
  getMergedEndpointPairs,
  migrateAndDeleteMergedEndpoints,
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
import Database.PostgreSQL.Simple.Types (PGArray (..), Query (Query))
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL (withConnection)
import Effectful.PostgreSQL qualified as PG
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Types (DB)
import Utils (formatUTC)


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
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (FromField) via Aeson Endpoint
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint


bulkInsertEndpoints :: DB es => V.Vector Endpoint -> Eff es ()
bulkInsertEndpoints endpoints = void $ PG.executeMany q $ V.toList rowsToInsert
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


dependenciesAndEventsCount :: (DB es, Time :> es) => Projects.ProjectId -> Text -> Text -> Int -> Text -> Eff es [HostEvents]
dependenciesAndEventsCount pid requestType sortT skip timeF = do
  now <- Time.currentTime
  let nowStr = formatUTC now
      timeRange = case timeF of
        "14D" -> "timestamp > '" <> nowStr <> "'::timestamptz - interval '14 day'"
        _ -> "timestamp > '" <> nowStr <> "'::timestamptz - interval '1 day'"
      orderBy = case sortT of
        "first_seen" -> "first_seen ASC"
        "last_seen" -> "last_seen DESC"
        _ -> "eventsCount DESC"
      endpointFilter = case requestType of
        "Outgoing" -> "ep.outgoing = true"
        "Incoming" -> "ep.outgoing = false"
        _ -> "ep.outgoing = false"
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
  PG.query (Query $ encodeUtf8 q) (pid, isOutgoing, isOutgoing, pid, skip)
  where
    isOutgoing = requestType == "Outgoing"


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


-- Endpoint template discovery ---------------------------------------------------

getUnmergedEndpoints :: DB es => Projects.ProjectId -> Eff es [(Text, Text, Text, Text)]
getUnmergedEndpoints pid =
  PG.query
    [sql| SELECT hash, method, host, url_path FROM apis.endpoints
        WHERE project_id = ? AND canonical_hash IS NULL AND merge_override = FALSE
        ORDER BY created_at ASC LIMIT 5000 |]
    (Only pid)


setEndpointCanonical :: DB es => [(Text, Text, Text)] -> Eff es Int64
setEndpointCanonical [] = pure 0
setEndpointCanonical triples =
  PG.execute
    [sql| UPDATE apis.endpoints SET canonical_hash = u.chash, canonical_path = u.cpath
        FROM (SELECT unnest(?::text[]) AS hash, unnest(?::text[]) AS chash, unnest(?::text[]) AS cpath) u
        WHERE apis.endpoints.hash = u.hash |]
    (V.fromList hashes, V.fromList chashes, V.fromList cpaths)
  where
    (hashes, chashes, cpaths) = unzip3 triples


insertCanonicalEndpoints :: DB es => [(Projects.ProjectId, Text, Text, Text, Text)] -> Eff es ()
insertCanonicalEndpoints [] = pure ()
insertCanonicalEndpoints rows =
  void
    $ PG.executeMany
      [sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing, canonical_hash, canonical_path)
        VALUES (?, ?, '{}', ?, ?, ?, false, ?, ?)
        ON CONFLICT (hash) DO UPDATE SET canonical_hash = EXCLUDED.canonical_hash, canonical_path = EXCLUDED.canonical_path |]
      (map (\(pid, templatePath, method, host, hash) -> (pid, templatePath, method, host, hash, hash, templatePath)) rows)


-- Endpoint embedding + merge ---------------------------------------------------

getEmbeddedEndpointTexts :: DB es => Projects.ProjectId -> Eff es [(EndpointId, Text)]
getEmbeddedEndpointTexts pid =
  PG.query [sql| SELECT id, url_path FROM apis.endpoints WHERE project_id = ? AND embedding IS NOT NULL |] (Only pid)


getUnembeddedEndpoints :: DB es => Projects.ProjectId -> Eff es [(EndpointId, Text, Text)]
getUnembeddedEndpoints pid =
  PG.query
    [sql| SELECT id, hash, url_path FROM apis.endpoints
        WHERE project_id = ? AND embedding IS NULL AND merge_override = FALSE
        LIMIT 500 |]
    (Only pid)


updateEndpointEmbeddings :: DB es => [(EndpointId, [Float])] -> Eff es Int64
updateEndpointEmbeddings [] = pure 0
updateEndpointEmbeddings pairs =
  PG.execute
    [sql| UPDATE apis.endpoints SET
          embedding = u.emb::float4[],
          embedding_at = NOW()
        FROM (SELECT unnest(?::uuid[]) AS id, unnest(?::float4[][]) AS emb) u
        WHERE apis.endpoints.id = u.id |]
    (V.fromList ids, V.fromList embs)
  where
    (ids, embs) = unzip $ map (\(eid, e) -> (eid, PGArray e)) pairs


getCanonicalEndpoints :: DB es => Projects.ProjectId -> Eff es [(EndpointId, [Float])]
getCanonicalEndpoints pid =
  map (\(eid, PGArray e) -> (eid, e))
    <$> PG.query
      [sql| SELECT id, embedding FROM apis.endpoints
        WHERE project_id = ? AND canonical_hash IS NULL
          AND embedding IS NOT NULL AND merge_override = FALSE |]
      (Only pid)


assignEndpointsToCanonical :: DB es => [(EndpointId, EndpointId)] -> Eff es Int64
assignEndpointsToCanonical [] = pure 0
assignEndpointsToCanonical pairs =
  PG.execute
    [sql| UPDATE apis.endpoints e SET canonical_hash = canon.hash
        FROM (SELECT unnest(?::uuid[]) AS id, unnest(?::uuid[]) AS canonical) u
        JOIN apis.endpoints canon ON canon.id = u.canonical
        WHERE e.id = u.id |]
    (V.fromList pids, V.fromList cids)
  where
    (pids, cids) = unzip pairs


unmergeEndpoint :: DB es => EndpointId -> Eff es Int64
unmergeEndpoint eid =
  PG.execute
    [sql| UPDATE apis.endpoints SET merge_override = TRUE, canonical_hash = NULL, canonical_path = NULL WHERE id = ? |]
    (Only eid)


getMergedEndpointPairs :: DB es => Projects.ProjectId -> Eff es [(Text, Text)]
getMergedEndpointPairs pid =
  PG.query
    [sql| SELECT hash, canonical_hash FROM apis.endpoints
          WHERE project_id = ? AND canonical_hash IS NOT NULL AND hash != canonical_hash
          LIMIT 10000 |]
    (Only pid)


-- | Migrate shapes/fields/formats from old endpoints to canonical ones, remap anomalies/issues, then delete old data.
migrateAndDeleteMergedEndpoints :: DB es => [(Text, Text)] -> Eff es ()
migrateAndDeleteMergedEndpoints [] = pure ()
migrateAndDeleteMergedEndpoints pairs = do
  let (oldHashes, canonHashes) = unzip pairs
      oldArr = V.fromList oldHashes
      canonArr = V.fromList canonHashes
      oldOnly = Only (PGArray oldHashes)
  -- Step 1: Migrate shapes (prefix-replace endpoint_hash and hash, remap field_hashes array)
  void
    $ PG.execute
      [sql| INSERT INTO apis.shapes (id, created_at, updated_at, project_id, endpoint_hash, hash,
            field_hashes, query_params_keypaths, request_body_keypaths, response_body_keypaths,
            request_headers_keypaths, response_headers_keypaths, status_code,
            response_description, request_description, new_unique_fields, deleted_fields, updated_field_formats)
          SELECT gen_random_uuid(), s.created_at, s.updated_at, s.project_id,
            m.canonical, m.canonical || substring(s.hash FROM 9),
            array(SELECT m2.canonical || substring(fh FROM 9)
                  FROM unnest(s.field_hashes) fh
                  LEFT JOIN unnest(?::text[], ?::text[]) m2(old, canonical) ON LEFT(fh, 8) = m2.old),
            s.query_params_keypaths, s.request_body_keypaths, s.response_body_keypaths,
            s.request_headers_keypaths, s.response_headers_keypaths, s.status_code,
            s.response_description, s.request_description, s.new_unique_fields, s.deleted_fields, s.updated_field_formats
          FROM apis.shapes s
          JOIN unnest(?::text[], ?::text[]) m(old, canonical) ON s.endpoint_hash = m.old
          ON CONFLICT (hash) DO NOTHING |]
      (oldArr, canonArr, oldArr, canonArr)
  -- Step 2: Migrate fields
  void
    $ PG.execute
      [sql| INSERT INTO apis.fields (id, created_at, updated_at, project_id, endpoint_hash, key,
            field_type, field_type_override, format, format_override, description, key_path,
            field_category, hash, is_enum, is_required)
          SELECT gen_random_uuid(), f.created_at, f.updated_at, f.project_id,
            m.canonical, f.key, f.field_type, f.field_type_override, f.format, f.format_override,
            f.description, f.key_path, f.field_category,
            m.canonical || substring(f.hash FROM 9),
            f.is_enum, f.is_required
          FROM apis.fields f
          JOIN unnest(?::text[], ?::text[]) m(old, canonical) ON f.endpoint_hash = m.old
          ON CONFLICT (hash) DO NOTHING |]
      (oldArr, canonArr)
  -- Step 3: Migrate formats
  void
    $ PG.execute
      [sql| INSERT INTO apis.formats (id, created_at, updated_at, project_id, field_hash, field_type,
            field_format, examples, hash)
          SELECT gen_random_uuid(), fmt.created_at, fmt.updated_at, fmt.project_id,
            m.canonical || substring(fmt.field_hash FROM 9),
            fmt.field_type, fmt.field_format, fmt.examples,
            m.canonical || substring(fmt.hash FROM 9)
          FROM apis.formats fmt
          JOIN unnest(?::text[], ?::text[]) m(old, canonical) ON LEFT(fmt.field_hash, 8) = m.old
          ON CONFLICT (hash) DO NOTHING |]
      (oldArr, canonArr)
  -- Step 4: Remap anomalies (skip if canonical target already exists for same project)
  void
    $ PG.execute
      [sql| UPDATE apis.anomalies a
          SET target_hash = m.canonical || substring(a.target_hash FROM 9)
          FROM unnest(?::text[], ?::text[]) m(old, canonical)
          WHERE LEFT(a.target_hash, 8) = m.old
            AND NOT EXISTS (SELECT 1 FROM apis.anomalies a2
                           WHERE a2.project_id = a.project_id
                             AND a2.target_hash = m.canonical || substring(a.target_hash FROM 9)) |]
      (oldArr, canonArr)
  -- Step 5: Remap issues (skip if canonical target already exists for same project+type)
  void
    $ PG.execute
      [sql| UPDATE apis.issues i
          SET endpoint_hash = m.canonical,
              target_hash = m.canonical || substring(i.target_hash FROM 9)
          FROM unnest(?::text[], ?::text[]) m(old, canonical)
          WHERE LEFT(i.target_hash, 8) = m.old
            AND NOT EXISTS (SELECT 1 FROM apis.issues i2
                           WHERE i2.project_id = i.project_id
                             AND i2.target_hash = m.canonical || substring(i.target_hash FROM 9)
                             AND i2.issue_type = i.issue_type
                             AND i2.acknowledged_at IS NULL AND i2.archived_at IS NULL) |]
      (oldArr, canonArr)
  -- Step 6: Delete old data (reverse dependency order)
  void $ PG.execute [sql| DELETE FROM apis.formats WHERE LEFT(field_hash, 8) = ANY(?) |] oldOnly
  void $ PG.execute [sql| DELETE FROM apis.fields WHERE endpoint_hash = ANY(?) |] oldOnly
  void $ PG.execute [sql| DELETE FROM apis.shapes WHERE endpoint_hash = ANY(?) |] oldOnly
  void $ PG.execute [sql| DELETE FROM apis.anomalies WHERE LEFT(target_hash, 8) = ANY(?) |] oldOnly
  void $ PG.execute [sql| DELETE FROM apis.issues WHERE LEFT(target_hash, 8) = ANY(?) |] oldOnly
  void $ PG.execute [sql| DELETE FROM apis.endpoints WHERE hash = ANY(?) |] oldOnly
