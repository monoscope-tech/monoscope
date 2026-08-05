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
  StatsMode (..),
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
import Data.List qualified as L
import Data.Map.Strict qualified as Map
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
import Pkg.DeriveUtils (DB, UUIDId (..), rawSql, showPGFloatArray)
import Relude


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


-- Per-(endpoint hash, service, bucket) telemetry slice, aggregated in Haskell.
data EndpointTelRow = EndpointTelRow
  { hash :: Text
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


-- | Rolling activity window for a period: (window start, bucket count, integer bucket
-- width in seconds). Widths divide evenly (24h/24 = 3600, 7d/7 = 86400).
periodWindow :: Text -> UTCTime -> (UTCTime, Int, Int)
periodWindow p now = (addUTCTime (negate $ fromIntegral (n * w)) now, n, w)
  where
    (n, w) = if p == "7d" then (7, 86400) else (24, 3600)


-- | Assemble a dense activity vector of @n@ buckets from sparse (bucketIdx, count)
-- pairs. Indices outside @[0, n)@ (e.g. a boundary row) are simply dropped.
denseBuckets :: Int -> [(Int, Int64)] -> V.Vector Int
denseBuckets n pairs = V.generate n \i -> fromIntegral $ Map.findWithDefault 0 i m
  where
    m = Map.fromListWith (+) pairs


-- Integer epoch seconds of a window start, for engine-portable time bucketing.
epochSecs :: UTCTime -> Int64
epochSecs = floor . utcTimeToPOSIXSeconds


-- | Quoted ISO8601 timestamp literal, inlined into the SQL text rather than bound.
tsLit :: UTCTime -> HI.Sql
tsLit t = rawSql $ "'" <> toText (iso8601Show t) <> "'"


-- | Max @last_seen@ / min @first_seen@ over rows, comparing via UTC (ZonedTime has no Ord).
pickZoned :: (UTCTime -> UTCTime -> Ordering) -> [Maybe ZonedTime] -> Maybe ZonedTime
pickZoned cmp = viaNonEmpty (L.maximumBy (cmp `on` zonedTimeToUTC)) . catMaybes


-- | @AND outgoing = ?@ when a direction is specified, empty otherwise.
directionClauseSql :: Maybe Bool -> HI.Sql
directionClauseSql = maybe [HI.sql| |] (\o -> [HI.sql| AND outgoing = #{o} |])


-- | Row filters shared by 'endpointRequestStatsByProject' and 'countEndpointsForHost'
-- (optional host, url_path search, archived state) so their paginators stay in sync.
endpointFiltersSql :: Maybe Text -> Maybe Text -> Bool -> HI.Sql
endpointFiltersSql pHostM searchM archived =
  foldMap (\h -> [HI.sql| AND enp.host = #{h}|]) pHostM
    <> foldMap (\s -> let pat = "%" <> s <> "%" in [HI.sql| AND enp.url_path LIKE #{pat}|]) searchM
    <> bool [HI.sql| AND h.archived_at IS NULL|] [HI.sql| AND h.archived_at IS NOT NULL|] archived


-- FIXME: Include and return a boolean flag to show if fields that have annomalies.
-- Endpoint identity comes from Postgres (@apis.*@); per-endpoint traffic stats come
-- from the telemetry store (TimeFusion when @useTf@), which holds no @apis.*@ tables —
-- so the two are fetched separately and joined + sorted + paginated in Haskell.
-- 'ShellOnly' skips the telemetry aggregate entirely: the page renders skeleton stat
-- cells and defers the (seconds-long) stats fetch, mirroring 'dependenciesAndEventsCount'.
endpointRequestStatsByProject :: (DB es, Labeled "timefusion" Hasql :> es, Time :> es) => StatsMode -> Bool -> Projects.ProjectId -> Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Int -> Text -> Text -> Eff es (V.Vector EndpointRequestStats)
endpointRequestStatsByProject statsMode useTf pid archived pHostM sortM searchM page perPage requestType period = do
  now <- Time.currentTime
  let isOutgoing = requestType == "Outgoing"
      (start, numBuckets, width) = periodWindow period now
      startEpoch = epochSecs start
  metas :: [EndpointMetaRow] <-
    Hasql.interp
      [HI.sql|
        SELECT enp.id, enp.hash, enp.project_id, enp.url_path, enp.method, enp.host, enp.created_at
        FROM apis.endpoints enp
        JOIN apis.hosts h ON (h.project_id = enp.project_id AND h.host = enp.host AND h.outgoing = enp.outgoing)
        WHERE enp.project_id = #{pid} AND enp.outgoing = #{isOutgoing} ^{endpointFiltersSql pHostM searchM archived}|]
  -- Endpoint hashes are stamped onto telemetry by the extraction worker; grouping by
  -- the stamped hash (instead of re-deriving route/method per span, which forced the
  -- wide JSON @attributes@ blob to be materialised per span) more than halved the
  -- query cost on a busy host (22s → 10s). Non-endpoint hashes fall out at the join.
  let endpointHashes = V.fromList $ map (.endpointHash) metas
  tels :: [EndpointTelRow] <-
    if V.null endpointHashes || statsMode == ShellOnly
      then pure []
      else
        Hasql.withHasqlTimefusion useTf
          $ Hasql.interp
            [HI.sql|
              WITH filtered AS (
                SELECT unnest(hashes) AS hash,
                       resource___service___name AS service,
                       floor((extract(epoch from timestamp) - #{startEpoch}) / #{width})::bigint AS bucket_idx,
                       timestamp
                FROM otel_logs_and_spans
                WHERE project_id = #{pid}::text
                  AND attributes___http___request___method IS NOT NULL
                  AND hashes && #{endpointHashes}::text[]
                  AND timestamp >= ^{tsLit start}
              )
              SELECT hash, service, bucket_idx, COUNT(*)::bigint AS cnt, MAX(timestamp) AS last_seen
              FROM filtered GROUP BY hash, service, bucket_idx|]
  let telMap = Map.fromListWith (<>) [(r.hash, [r]) | r <- tels]
      mk m =
        let rs = fromMaybe [] $ Map.lookup m.endpointHash telMap
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
        "last_seen" -> sortOn (Down . fmap zonedTimeToUTC . (.lastSeen) . snd) rows
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


-- | Whether to pay for the telemetry-store aggregate. The host list itself comes from
-- Postgres and is cheap; the per-host counts/sparkline scan a full window of spans and
-- can take tens of seconds, so the page renders 'ShellOnly' first and fills in after.
data StatsMode = ShellOnly | WithStats
  deriving stock (Eq, Show)


-- | When @outgoingM@ is @Nothing@, both directions are returned (used for the
-- combined Archived tab). Spans are attributed to a host using the same
-- coalesce that ProcessMessage applies at ingest to set @apis.endpoints.host@
-- (see 'src/ProcessMessage.hs:223'), so each row tallies only its own traffic.
dependenciesAndEventsCount :: (DB es, Labeled "timefusion" Hasql :> es, Time :> es) => StatsMode -> Bool -> Projects.ProjectId -> Maybe Bool -> Text -> Int -> Text -> Text -> Bool -> Eff es [HostEvents]
dependenciesAndEventsCount statsMode useTf pid outgoingM sortT skip timeF period showArchived = do
  now <- Time.currentTime
  let windowStartSql = tsLit $ addUTCTime (bool (-1) (-14) (timeF == "14D") * 86400) now
      (start, numBuckets, width) = periodWindow period now
      startEpoch = epochSecs start
  hosts :: [(Text, Bool)] <-
    Hasql.interp
      [HI.sql|
        SELECT host, outgoing FROM apis.hosts
        WHERE project_id = #{pid} AND host != '' ^{directionClauseSql outgoingM}
          AND ^{rawSql $ bool "archived_at IS NULL" "archived_at IS NOT NULL" showArchived}|]
  tels :: [HostTelRow] <- case statsMode of
    ShellOnly -> pure []
    WithStats ->
      Hasql.withHasqlTimefusion useTf
        $ Hasql.interp
          -- Host reads one flat column: every ingest path normalises the legacy semconv
          -- host attributes (net.host.name, http.host, net.peer.name) onto server.address.
          -- The COALESCE over JSON paths this replaced forced the wide @attributes@ blob to
          -- be materialised for every span in the window (107s vs 21s on a busy project).
          -- Spans written before that normalisation have no server.address and fall out as
          -- the empty string (dropped below); they age out of the window on their own.
          [HI.sql|
          WITH filtered AS (
            SELECT COALESCE(s.attributes___server___address, '') AS host,
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
      -- Shell rows carry no counts, so the traffic sorts would be arbitrary; order by
      -- host instead to keep the placeholder stable until the stats swap arrives.
      ordered = case (statsMode, sortT) of
        (ShellOnly, _) -> sortOn (.host) rows
        (WithStats, "first_seen") -> sortOn (fmap zonedTimeToUTC . (.first_seen)) rows
        (WithStats, "last_seen") -> sortOn (Down . fmap zonedTimeToUTC . (.last_seen)) rows
        (WithStats, _) -> sortOn (Down . (.eventCount)) rows
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
  let isOutgoing = requestType == "Outgoing"
   in fromMaybe 0
        <$> Hasql.interpOne
          [HI.sql|
            SELECT coalesce(COUNT(*)::BIGINT, 0)
            FROM apis.endpoints enp
            LEFT JOIN apis.issues ann ON (ann.issue_type = 'api_change' AND ann.endpoint_hash = enp.hash)
            WHERE enp.project_id = #{pid} AND enp.outgoing = #{isOutgoing}
              AND ann.id IS NOT NULL AND ann.acknowledged_at IS NULL AND host = #{host} |]


-- | Count of endpoints under a (project, direction), under the same row filters as
-- 'endpointRequestStatsByProject'.
countEndpointsForHost :: DB es => Projects.ProjectId -> Bool -> Bool -> Maybe Text -> Maybe Text -> Eff es Int
countEndpointsForHost pid outgoing archived pHostM searchM =
  fromMaybe 0
    <$> Hasql.interpOne
      [HI.sql|
        SELECT COUNT(*)::bigint
        FROM apis.endpoints enp
        JOIN apis.hosts h ON (h.project_id = enp.project_id AND h.host = enp.host AND h.outgoing = enp.outgoing)
        WHERE enp.project_id = #{pid} AND enp.outgoing = #{outgoing}
          ^{endpointFiltersSql pHostM searchM archived} |]


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
    (pids, tpls, methods, hosts, hashes) = L.unzip5 rows


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
  map (second (V.toList @Float))
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
  let (oldArr, canonArr) = bimap V.fromList V.fromList $ unzip pairs
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
