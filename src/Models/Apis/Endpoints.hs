module Models.Apis.Endpoints (
  Endpoint (..),
  EndpointId,
  EndpointRequestStats (..),
  Host (..),
  HostEvents (..),
  bulkInsertEndpoints,
  bulkInsertHosts,
  countEndpointsForHost,
  dependenciesAndEventsCount,
  archiveHosts,
  unarchiveHosts,
  endpointRequestStatsByProject,
  countEndpointInbox,
  listEndpointsPaged,
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
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Map.Lazy qualified as Map
import Data.Time (UTCTime, ZonedTime, addUTCTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Labeled (Labeled)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
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
  deriving (AE.FromJSON) via DAE.Snake Endpoint


-- | Persist newly discovered endpoints and their host records together. Both
-- writes run on the same connection so callers don't have to coordinate the
-- order, and the host upsert can't be skipped if the endpoint upsert succeeds.
bulkInsertEndpoints :: DB es => V.Vector Endpoint -> Eff es ()
bulkInsertEndpoints eps | V.null eps = pass
bulkInsertEndpoints eps = do
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing, service_name, environment)
           SELECT * FROM unnest(#{pids}::uuid[], #{paths}::text[], #{params}::jsonb[], #{methods}::text[], #{hosts}::text[], #{hashes}::text[], #{outs}::bool[], #{svcs}::text[], #{envs}::text[])
           ON CONFLICT (hash) DO NOTHING |]
  bulkInsertHosts eps
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


-- | Upsert one row in apis.hosts per (project_id, host, outgoing) seen in the
-- vector. Idempotent: existing rows (including archived ones) are left alone.
bulkInsertHosts :: DB es => V.Vector Endpoint -> Eff es ()
bulkInsertHosts eps | V.null eps = pass
bulkInsertHosts eps =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.hosts (project_id, host, outgoing)
           SELECT p, h, o FROM unnest(#{pids}::uuid[], #{hosts}::text[], #{outs}::bool[]) AS t(p, h, o)
           WHERE h <> ''
           ON CONFLICT (project_id, host, outgoing) DO NOTHING |]
  where
    pids = V.map (.projectId) eps
    hosts = V.map (.host) eps
    outs = V.map (.outgoing) eps


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
  , activityBuckets :: V.Vector Int
  , services :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- Per-(url_path, method, service, bucket) telemetry slice, aggregated in Haskell.
data EndpointTelRow = EndpointTelRow
  { urlPath :: Text
  , method :: Maybe Text
  , service :: Maybe Text
  , bucketIdx :: Int
  , cnt :: Int64
  , lastSeen :: Maybe ZonedTime
  }
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- Endpoint identity/metadata (Postgres only — TimeFusion has no @apis.*@ tables).
data EndpointMetaRow = EndpointMetaRow
  { endpointId :: EndpointId
  , endpointHash :: Text
  , projectId :: Projects.ProjectId
  , urlPath :: Text
  , method :: Text
  , host :: Text
  , createdAt :: ZonedTime
  }
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- | Resolve the remote host of a span. Mirrors the priority used by
-- 'ProcessMessage.hs:223' when stamping @apis.endpoints.host@ at ingest, so
-- the value computed here equals what's stored in @apis.hosts@.
-- Trailing entries (url.full / service.name) are legacy fallbacks for spans
-- emitted before the stamping logic existed; they're harmless when the
-- primary attributes are present. The @prefix@ is the column alias (e.g.
-- @"s."@) or empty for unaliased queries.
-- Uses chained @->@/@->>@ (not @#>>@): TimeFusion's VariantAwareExprPlanner only
-- rewrites @->@/@->>@ on the Variant @attributes@ column; @#>>@ falls through and
-- doesn't extract from it. Semantically identical to @#>>@ on Postgres.
hostCoalesceExpr :: Text -> Text
hostCoalesceExpr prefix =
  "COALESCE(NULLIF("
    <> prefix
    <> "attributes->'net'->'host'->>'name',''), "
    <> "NULLIF("
    <> prefix
    <> "attributes___server___address,''), "
    <> "NULLIF("
    <> prefix
    <> "attributes->'http'->>'host',''), "
    <> "NULLIF(split_part(split_part(split_part("
    <> prefix
    <> "attributes___url___full, '://', 2), '/', 1), ':', 1), ''), "
    <> "NULLIF("
    <> prefix
    <> "resource___service___name,''), '')"


-- | Rolling activity window for a period: (window start, bucket count, integer bucket
-- width in seconds). Widths divide evenly (24h/24 = 3600, 7d/7 = 86400).
periodWindow :: Text -> UTCTime -> (UTCTime, Int, Int)
periodWindow p now = (addUTCTime (negate $ fromIntegral (n * w)) now, n, w)
  where
    (n, w) = if p == "7d" then (7, 86400) else (24, 3600)


-- | Assemble a dense activity vector of @n@ buckets from sparse (bucketIdx, count)
-- pairs. Indices outside @[0, n)@ (e.g. a boundary row) are simply dropped.
denseBuckets :: Int -> [(Int, Int64)] -> V.Vector Int
denseBuckets n pairs = V.generate n \i -> fromIntegral $ fromMaybe 0 $ Map.lookup i m
  where
    m = Map.fromListWith (+) pairs


-- Integer epoch seconds of a window start, for engine-portable time bucketing.
epochSecs :: UTCTime -> Int64
epochSecs = floor . utcTimeToPOSIXSeconds


-- | Max @last_seen@ / min @first_seen@ over rows, comparing via UTC (ZonedTime has no Ord).
pickZoned :: (UTCTime -> UTCTime -> Ordering) -> [Maybe ZonedTime] -> Maybe ZonedTime
pickZoned cmp zs = case catMaybes zs of
  [] -> Nothing
  (x : xs) -> Just $ foldl' (\a b -> if cmp (zonedTimeToUTC a) (zonedTimeToUTC b) == LT then b else a) x xs


-- | @AND outgoing = ?@ when a direction is specified, empty otherwise.
directionClauseSql :: Maybe Bool -> HI.Sql
directionClauseSql = maybe [HI.sql| |] (\o -> [HI.sql| AND outgoing = #{o} |])


archivedHostClauseSql :: Bool -> HI.Sql
archivedHostClauseSql True = [HI.sql| AND h.archived_at IS NOT NULL|]
archivedHostClauseSql False = [HI.sql| AND h.archived_at IS NULL|]


-- FIXME: Include and return a boolean flag to show if fields that have annomalies.
-- FIXME: return endpoint_hash as well.
-- Endpoint identity comes from Postgres (@apis.*@); per-endpoint traffic stats come
-- from the telemetry store (TimeFusion when @useTf@), which holds no @apis.*@ tables —
-- so the two are fetched separately and joined + sorted + paginated in Haskell.
endpointRequestStatsByProject :: (DB es, Labeled "timefusion" Hasql :> es, Time :> es) => Bool -> Projects.ProjectId -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Int -> Text -> Text -> Eff es (V.Vector EndpointRequestStats)
endpointRequestStatsByProject useTf pid archived pHostM sortM searchM page perPage requestType period = do
  now <- Time.currentTime
  let isOutgoing = requestType == "Outgoing"
      (start, numBuckets, width) = periodWindow period now
      startEpoch = epochSecs start
      startSql = rawSql $ "'" <> toText (iso8601Show start) <> "'"
      pHostQuery = foldMap (\h -> [HI.sql| AND enp.host = #{h}|]) pHostM
      search = foldMap (\s -> let pat = "%" <> s <> "%" in [HI.sql| AND enp.url_path LIKE #{pat}|]) searchM
      archivedClause = archivedHostClauseSql archived
      hostFilter = foldMap (\h -> [HI.sql| AND (^{rawSql (hostCoalesceExpr "")} = #{h})|]) pHostM
  metas :: [EndpointMetaRow] <-
    Hasql.interp
      [HI.sql|
        SELECT enp.id, enp.hash, enp.project_id, enp.url_path, enp.method, enp.host, enp.created_at
        FROM apis.endpoints enp
        JOIN apis.hosts h ON (h.project_id = enp.project_id AND h.host = enp.host AND h.outgoing = enp.outgoing)
        WHERE enp.project_id = #{pid} AND enp.outgoing = #{isOutgoing} ^{pHostQuery} ^{search} ^{archivedClause}|]
  tels :: [EndpointTelRow] <-
    Hasql.withHasqlTimefusion useTf
      $ Hasql.interp
        [HI.sql|
          WITH filtered AS (
            SELECT COALESCE(NULLIF(attributes->'http'->>'route', ''), attributes___url___path, '/') AS url_path,
                   attributes___http___request___method AS method,
                   resource___service___name AS service,
                   floor((extract(epoch from timestamp) - #{startEpoch}) / #{width})::bigint AS bucket_idx,
                   timestamp
            FROM otel_logs_and_spans
            WHERE project_id = #{pid}::text
              AND attributes___http___request___method IS NOT NULL
              ^{hostFilter}
              AND timestamp >= ^{startSql}
          )
          SELECT url_path, method, service, bucket_idx, COUNT(*)::bigint AS cnt, MAX(timestamp) AS last_seen
          FROM filtered GROUP BY url_path, method, service, bucket_idx|]
  let telMap = Map.fromListWith (<>) [((r.urlPath, fromMaybe "" r.method), [r]) | r <- tels]
      mk m =
        let rs = fromMaybe [] $ Map.lookup (m.urlPath, m.method) telMap
         in ( m
            , EndpointRequestStats
                { endpointId = m.endpointId
                , endpointHash = m.endpointHash
                , projectId = m.projectId
                , urlPath = m.urlPath
                , method = m.method
                , host = m.host
                , totalRequests = fromIntegral $ sum $ map (.cnt) rs
                , lastSeen = pickZoned compare $ map (.lastSeen) rs
                , activityBuckets = denseBuckets numBuckets [(r.bucketIdx, r.cnt) | r <- rs]
                , services = V.fromList $ ordNub $ mapMaybe (.service) rs
                }
            )
      rows = map mk metas
      ordered = case fromMaybe "" sortM of
        "first_seen" -> sortOn (zonedTimeToUTC . (.createdAt) . fst) rows
        "last_seen" -> sortOn (Down . zonedTimeToUTC . (.createdAt) . fst) rows
        _ -> sortOn (\(m, s) -> (Down s.totalRequests, m.urlPath)) rows
  pure $ V.fromList $ map snd $ take perPage $ drop (page * perPage) ordered


data HostEvents = HostEvents
  { host :: Text
  , outgoing :: Bool
  , eventCount :: Int64
  , first_seen :: Maybe ZonedTime
  , last_seen :: Maybe ZonedTime
  , activityBuckets :: V.Vector Int
  , services :: V.Vector Text -- distinct resource.service.name emitting traffic for this host
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- Per-(host, direction, service, bucket) telemetry slice, aggregated in Haskell.
data HostTelRow = HostTelRow
  { host :: Text
  , outgoing :: Bool
  , service :: Maybe Text
  , bucketIdx :: Int
  , cnt :: Int64
  , lastSeen :: Maybe ZonedTime
  , firstSeen :: Maybe ZonedTime
  }
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- | When @outgoingM@ is @Nothing@, both directions are returned (used for the
-- combined Archived tab). Spans are attributed to a host using the same
-- coalesce that ProcessMessage applies at ingest to set @apis.endpoints.host@
-- (see 'src/ProcessMessage.hs:223'), so each row tallies only its own traffic.
dependenciesAndEventsCount :: (DB es, Labeled "timefusion" Hasql :> es, Time :> es) => Bool -> Projects.ProjectId -> Maybe Bool -> Text -> Int -> Text -> Text -> Bool -> Eff es [HostEvents]
dependenciesAndEventsCount useTf pid outgoingM sortT skip timeF period showArchived = do
  now <- Time.currentTime
  let intervalDays = if timeF == "14D" then 14 else 1 :: Int
      windowStart = addUTCTime (negate $ fromIntegral (intervalDays * 86400)) now
      windowStartSql = rawSql $ "'" <> toText (iso8601Show windowStart) <> "'"
      (start, numBuckets, width) = periodWindow period now
      startEpoch = epochSecs start
      directionClause = directionClauseSql outgoingM
      hostExpr = rawSql (hostCoalesceExpr "s.")
      archivedClause = rawSql if showArchived then "archived_at IS NOT NULL" else "archived_at IS NULL"
  hosts :: [(Text, Bool)] <-
    Hasql.interp
      [HI.sql|
        SELECT host, outgoing FROM apis.hosts
        WHERE project_id = #{pid} AND host != '' ^{directionClause} AND ^{archivedClause}|]
  tels :: [HostTelRow] <-
    Hasql.withHasqlTimefusion useTf
      $ Hasql.interp
        [HI.sql|
          WITH filtered AS (
            SELECT ^{hostExpr} AS host,
                   (s.kind = 'client') AS outgoing,
                   s.resource___service___name AS service,
                   floor((extract(epoch from s.timestamp) - #{startEpoch}) / #{width})::bigint AS bucket_idx,
                   s.timestamp AS ts
            FROM otel_logs_and_spans s
            WHERE s.project_id = #{pid}::text
              AND s.timestamp > ^{windowStartSql}
              AND s.attributes___http___request___method IS NOT NULL
          )
          SELECT host, outgoing, service, bucket_idx, COUNT(*)::bigint AS cnt, MAX(ts) AS last_seen, MIN(ts) AS first_seen
          FROM filtered GROUP BY host, outgoing, service, bucket_idx|]
  let telMap = Map.fromListWith (<>) [((r.host, r.outgoing), [r]) | r <- tels, r.host /= ""]
      mk (h, o) =
        let rs = fromMaybe [] $ Map.lookup (h, o) telMap
         in HostEvents
              { host = h
              , outgoing = o
              , eventCount = sum $ map (.cnt) rs
              , first_seen = pickZoned (flip compare) $ map (.firstSeen) rs
              , last_seen = pickZoned compare $ map (.lastSeen) rs
              , activityBuckets = denseBuckets numBuckets [(r.bucketIdx, r.cnt) | r <- rs]
              , services = V.fromList $ filter (/= h) $ ordNub $ mapMaybe (.service) rs
              }
      rows = map mk hosts
      ordered = case sortT of
        "first_seen" -> sortOn (fmap zonedTimeToUTC . (.first_seen)) rows
        "last_seen" -> sortOn (Down . fmap zonedTimeToUTC . (.last_seen)) rows
        _ -> sortOn (Down . (.eventCount)) rows
  pure $ take 200 $ drop skip ordered


-- | Mark hosts as archived. When @outgoingM@ is @Nothing@ both directions for
-- the host are archived (used by archive-all flows). Idempotent.
archiveHosts :: DB es => Projects.ProjectId -> Maybe Bool -> Maybe Projects.UserId -> [Text] -> Eff es Int64
archiveHosts _ _ _ [] = pure 0
archiveHosts pid outgoingM byM hosts =
  Hasql.interpExecute
    [HI.sql|
      UPDATE apis.hosts
         SET archived_at = NOW(), archived_by = #{byM}, updated_at = NOW()
       WHERE project_id = #{pid}
         AND host = ANY(#{hosts}::text[])
         AND archived_at IS NULL ^{directionClauseSql outgoingM} |]


unarchiveHosts :: DB es => Projects.ProjectId -> Maybe Bool -> [Text] -> Eff es Int64
unarchiveHosts _ _ [] = pure 0
unarchiveHosts pid outgoingM hosts =
  Hasql.interpExecute
    [HI.sql|
      UPDATE apis.hosts
         SET archived_at = NULL, archived_by = NULL, updated_at = NOW()
       WHERE project_id = #{pid}
         AND host = ANY(#{hosts}::text[])
         AND archived_at IS NOT NULL ^{directionClauseSql outgoingM} |]


countEndpointInbox :: DB es => Projects.ProjectId -> Text -> Text -> Eff es Int
countEndpointInbox pid host requestType =
  let dirClause = rawSql case requestType of
        "Outgoing" -> "enp.outgoing = true"
        _ -> "enp.outgoing = false"
   in fromMaybe 0
        <$> Hasql.interpOne
          [HI.sql|
            SELECT coalesce(COUNT(*)::BIGINT, 0)
            FROM apis.endpoints enp
            LEFT JOIN apis.issues ann ON (ann.issue_type = 'api_change' AND ann.endpoint_hash = enp.hash)
            WHERE enp.project_id = #{pid} AND ^{dirClause}
              AND ann.id IS NOT NULL AND ann.acknowledged_at IS NULL AND host = #{host} |]


-- | Count of endpoints under a (project, direction) optionally filtered by
-- host, url_path search, and archived status — mirrors the row filters used by
-- 'endpointRequestStatsByProject' so paginators stay in sync.
countEndpointsForHost :: DB es => Projects.ProjectId -> Bool -> Bool -> Maybe Text -> Maybe Text -> Eff es Int
countEndpointsForHost pid outgoing archived pHostM searchM =
  let hostQ = foldMap (\h -> [HI.sql| AND enp.host = #{h}|]) pHostM
      searchQ = foldMap (\s -> let pat = "%" <> s <> "%" in [HI.sql| AND enp.url_path LIKE #{pat}|]) searchM
      archivedQ = archivedHostClauseSql archived
   in fromMaybe 0
        <$> Hasql.interpOne
          [HI.sql|
            SELECT COUNT(*)::bigint
            FROM apis.endpoints enp
            JOIN apis.hosts h ON (h.project_id = enp.project_id AND h.host = enp.host AND h.outgoing = enp.outgoing)
            WHERE enp.project_id = #{pid} AND enp.outgoing = #{outgoing}
              ^{hostQ} ^{searchQ} ^{archivedQ} |]


-- | Paginated endpoint list with optional url_path LIKE filter. Returns (rows, total count).
listEndpointsPaged :: DB es => Projects.ProjectId -> Bool -> Maybe Text -> Int -> Int -> Eff es ([Endpoint], Int)
listEndpointsPaged pid outgoing searchM limit offset = do
  let hasSearch = isJust searchM
      pat = maybe "" (\s -> "%" <> s <> "%") searchM
      whereSql =
        [HI.sql|
          WHERE project_id = #{pid} AND outgoing = #{outgoing}
            AND (NOT #{hasSearch} OR url_path LIKE #{pat})
        |]
  rows <-
    Hasql.interp
      [HI.sql|
        SELECT id, created_at, updated_at, project_id, url_path, url_params, method, host, hash, outgoing, description, service_name, environment
        FROM apis.endpoints ^{whereSql}
        ORDER BY url_path ASC LIMIT #{limit} OFFSET #{offset} |]
  total <- fromMaybe 0 <$> Hasql.interpOne [HI.sql| SELECT COUNT(*)::bigint FROM apis.endpoints ^{whereSql} |]
  pure (rows, total)


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
        FROM (SELECT unnest(#{hashes}::text[]) AS hash, unnest(#{chashes}::text[]) AS chash, unnest(#{cpaths}::text[]) AS cpath) u
        WHERE apis.endpoints.hash = u.hash |]
  where
    (hashes, chashes, cpaths) = unzip3 triples


insertCanonicalEndpoints :: DB es => [(Projects.ProjectId, Text, Text, Text, Text)] -> Eff es ()
insertCanonicalEndpoints [] = pass
insertCanonicalEndpoints rows =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing, canonical_hash, canonical_path)
           SELECT p, tp, '{}'::jsonb, m, h, eh, FALSE, eh, tp
           FROM unnest(#{pids}::uuid[], #{tpls}::text[], #{methods}::text[], #{hosts}::text[], #{hashes}::text[]) AS t(p, tp, m, h, eh)
           ON CONFLICT (hash) DO UPDATE SET canonical_hash = EXCLUDED.canonical_hash, canonical_path = EXCLUDED.canonical_path |]
  where
    pids = [p | (p, _, _, _, _) <- rows]
    tpls = [t | (_, t, _, _, _) <- rows]
    methods = [m | (_, _, m, _, _) <- rows]
    hosts = [h | (_, _, _, h, _) <- rows]
    hashes = [eh | (_, _, _, _, eh) <- rows]


-- Endpoint embedding + merge ---------------------------------------------------

fetchEndpointTexts :: DB es => [EndpointId] -> Eff es (Map EndpointId Text)
fetchEndpointTexts [] = pure mempty
fetchEndpointTexts ids =
  Map.fromList
    <$> Hasql.interp [HI.sql| SELECT id, url_path FROM apis.endpoints WHERE id = ANY(#{ids}) |]


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
        FROM ROWS FROM (unnest(#{ids}::uuid[]), unnest(#{embs}::text[])) AS u(id, emb)
        WHERE apis.endpoints.id = u.id |]
  where
    (ids, embs) = unzip $ map (second showPGFloatArray) pairs


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
        FROM (SELECT unnest(#{pids}::uuid[]) AS id, unnest(#{cids}::uuid[]) AS canonical) u
        JOIN apis.endpoints canon ON canon.id = u.canonical
        WHERE e.id = u.id |]
  where
    (pids, cids) = unzip pairs


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


-- | Remap anomalies/issues from merged endpoints to their canonical hashes, then delete old endpoints.
-- Legacy apis.shapes/fields/formats migration steps removed (tables dropped in 0090);
-- the schema-learning catalog (apis.schema_catalog) re-derives structure on the fly per
-- canonical key, so no explicit row migration is needed for the new model.
migrateAndDeleteMergedEndpoints :: DB es => [(Text, Text)] -> Eff es ()
migrateAndDeleteMergedEndpoints [] = pass
migrateAndDeleteMergedEndpoints pairs = do
  let (oldHashes, canonHashes) = unzip pairs
      oldArr = V.fromList oldHashes
      canonArr = V.fromList canonHashes
  -- Remap anomalies (skip if canonical target already exists for same project)
  Hasql.interpExecute_
    [HI.sql|
      UPDATE apis.anomalies a
          SET target_hash = m.canonical || substring(a.target_hash FROM 9)
          FROM unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical)
          WHERE LEFT(a.target_hash, 8) = m.old
            AND NOT EXISTS (SELECT 1 FROM apis.anomalies a2
                           WHERE a2.project_id = a.project_id
                             AND a2.target_hash = m.canonical || substring(a.target_hash FROM 9)) |]
  -- Remap issues (skip if canonical target already exists for same project+type)
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
  -- Delete leftover anomalies/issues for the merged-out endpoints, then the endpoints themselves.
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.anomalies WHERE LEFT(target_hash, 8) = ANY(#{oldArr}) |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.issues WHERE LEFT(target_hash, 8) = ANY(#{oldArr}) |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.endpoints WHERE hash = ANY(#{oldArr}) |]
