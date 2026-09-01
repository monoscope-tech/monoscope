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
  listEndpointsPaged,
  getEndpointById,
  -- Endpoint template discovery
  getUnmergedEndpoints,
  getCanonicalTemplateKeys,
  getReviewedGroupHashes,
  ReviewedGroup (..),
  recordGroupReviews,
  GroupReview (..),
  getGroupReviews,
  markGroupApplied,
  revertGroupApply,
  getQuarantinedMerges,
  appliedCanonicalHashes,
  fleetShapeReport,
  endpointShapeAgreement,
  autoApplyAccuracy,
  knownStaticSegments,
  learnedIdRulePrefixes,
  insertLearnedIdRule,
  learnedIdRulesFor,
  unmergedScanLimit,
  setEndpointCanonical,
  insertCanonicalEndpoints,
  -- Endpoint merge cleanup
  getMergedEndpointPairs,
  migrateAndDeleteMergedEndpoints,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
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
import Pkg.DeriveUtils (DB, UUIDId (..), rawSql)
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

-- | Endpoints scanned per discovery run. Bounded so one exploded project cannot
-- monopolise a job slot; the run logs when it hit the limit and the next one
-- picks up where it left off.
unmergedScanLimit :: Int
unmergedScanLimit = 5000


getUnmergedEndpoints :: DB es => Projects.ProjectId -> Eff es [(Text, Text, Text, Text)]
getUnmergedEndpoints pid =
  Hasql.interp
    [HI.sql| SELECT hash, method, host, url_path FROM apis.endpoints
        WHERE project_id = #{pid} AND canonical_hash IS NULL AND merge_override = FALSE
        ORDER BY created_at ASC LIMIT #{unmergedScanLimit} |]


-- | (method, host, canonical_path) for templates this project already has, so
-- discovery can assign a lone straggler to a template the population already
-- proved — without which a project re-inflates one row at a time after the
-- bulk merge.
getCanonicalTemplateKeys :: DB es => Projects.ProjectId -> Eff es (V.Vector (Text, Text, Text))
getCanonicalTemplateKeys pid =
  V.fromList
    <$> Hasql.interp
      [HI.sql| SELECT DISTINCT method, host, canonical_path FROM apis.endpoints
          WHERE project_id = #{pid} AND canonical_path IS NOT NULL |]


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


-- | Merged-out rows ready for deletion.
--
-- Merges this codebase inferred deterministically are always ready. Merges an
-- LLM verdict caused are held back for a day: deletion is the step that makes a
-- wrong merge permanent, and the quarantine is the window in which the
-- re-check can still take it back.
getMergedEndpointPairs :: DB es => Projects.ProjectId -> Eff es [(Text, Text)]
getMergedEndpointPairs pid = do
  quarantined <- quarantinedCanonicalHashes pid
  Hasql.interp
    [HI.sql| SELECT hash, canonical_hash FROM apis.endpoints
          WHERE project_id = #{pid} AND canonical_hash IS NOT NULL AND hash != canonical_hash
            AND NOT (canonical_hash = ANY(#{quarantined}))
          LIMIT 10000 |]


-- | Remap anomalies/issues from merged endpoints to their canonical hashes, then delete old endpoints.
-- Legacy apis.shapes/fields/formats migration steps removed (tables dropped in 0090);
-- the schema-learning catalog (apis.schema_catalog) re-derives structure on the fly per
-- canonical key, so no explicit row migration is needed for the new model.
-- | A stored verdict plus the evidence accumulated for it.
data GroupReview = GroupReview
  { groupKey :: Text
  , membersHash :: Text
  , memberCount :: Int64
  , firstMemberCount :: Int64
  , verdict :: Text
  , confirmations :: Int64
  , appliedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


getGroupReviews :: DB es => Projects.ProjectId -> Eff es [GroupReview]
getGroupReviews pid =
  Hasql.interp
    [HI.sql| SELECT group_key, members_hash, member_count::int8, first_member_count::int8,
                    verdict, confirmations::int8, applied_at
             FROM apis.endpoint_group_reviews
             WHERE project_id = #{pid} AND reverted_at IS NULL |]


-- | Record that a group's merge was applied, and under which canonical hash.
markGroupApplied :: (DB es, Time :> es) => Projects.ProjectId -> Text -> V.Vector Text -> Eff es ()
markGroupApplied pid gkey chashes = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| UPDATE apis.endpoint_group_reviews SET applied_at = #{now}, applied_canonical_hashes = #{chashes}
             WHERE project_id = #{pid} AND group_key = #{gkey} |]


-- | Undo an applied merge that the quarantine re-check refuted.
--
-- Only reachable while the merge is still quarantined, so the concrete rows are
-- still there to un-assign. @merge_override@ keeps them out of every future
-- pass — a group the model has both merged and then disowned is not one to
-- keep re-litigating.
revertGroupApply :: (DB es, Time :> es) => Projects.ProjectId -> Text -> V.Vector Text -> Eff es Int64
revertGroupApply pid gkey chashes = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| UPDATE apis.endpoint_group_reviews SET reverted_at = #{now}
             WHERE project_id = #{pid} AND group_key = #{gkey} |]
  Hasql.interpExecute
    [HI.sql| UPDATE apis.endpoints SET canonical_hash = NULL, canonical_path = NULL, merge_override = TRUE
             WHERE project_id = #{pid} AND canonical_hash = ANY(#{chashes}) AND hash <> canonical_hash |]


-- | Canonical hashes from LLM-applied merges still inside their quarantine.
-- Cleanup must not delete their sources yet: deletion is what makes a wrong
-- merge permanent.
quarantinedCanonicalHashes :: DB es => Projects.ProjectId -> Eff es (V.Vector Text)
quarantinedCanonicalHashes pid =
  V.fromList
    <$> Hasql.interp
      [HI.sql| SELECT unnest(applied_canonical_hashes) FROM apis.endpoint_group_reviews
               WHERE project_id = #{pid} AND applied_at IS NOT NULL
                 AND reverted_at IS NULL AND applied_at > NOW() - INTERVAL '24 hours' |]


-- | Applied merges still inside their quarantine, with the values each one
-- collapsed, so they can be challenged before deletion makes them permanent.
getQuarantinedMerges :: DB es => Projects.ProjectId -> Eff es [(Text, Text, V.Vector Text)]
getQuarantinedMerges pid =
  Hasql.interp
    [HI.sql| SELECT r.group_key, coalesce(min(e.canonical_path), ''), array_agg(DISTINCT e.url_path)
             FROM apis.endpoint_group_reviews r
             JOIN apis.endpoints e ON e.project_id = r.project_id AND e.canonical_hash = ANY(r.applied_canonical_hashes)
             WHERE r.project_id = #{pid} AND r.applied_at IS NOT NULL AND r.reverted_at IS NULL
               AND r.applied_at > NOW() - INTERVAL '24 hours'
               AND e.hash <> e.canonical_hash
             GROUP BY r.group_key
             LIMIT 20 |]


-- | The canonical hashes one applied verdict produced.
appliedCanonicalHashes :: DB es => Projects.ProjectId -> Text -> Eff es (V.Vector Text)
appliedCanonicalHashes pid gkey =
  fromMaybe V.empty
    <$> Hasql.interpOne
      [HI.sql| SELECT applied_canonical_hashes FROM apis.endpoint_group_reviews
               WHERE project_id = #{pid} AND group_key = #{gkey} |]


-- | How much the endpoints in a candidate group agree on their observed shape:
-- @(how many have been observed at all, how many distinct shapes between them)@.
--
-- The schema catalog fingerprints what an endpoint actually returned. Two ids of
-- one route produce the same fingerprint; two different routes do not. That
-- makes this the one check available that is about behaviour rather than
-- spelling — and unlike anything computed from the path strings, a group of
-- route words genuinely fails it.
endpointShapeAgreement :: DB es => Projects.ProjectId -> V.Vector Text -> Eff es (Int64, Int64)
endpointShapeAgreement pid hashes =
  fromMaybe (0, 0)
    <$> Hasql.interpOne
      [HI.sql| SELECT count(c.key_hash)::int8, count(DISTINCT c.template_hash)::int8
               FROM unnest(#{hashes}::text[]) AS h(hash)
               JOIN apis.schema_catalog c ON c.project_id = #{pid} AND c.key_hash = h.hash |]


-- | @(merges applied, merges the re-check later took back)@ over the last week.
--
-- The revert rate is the system's own error rate, measured without anybody
-- reading anything, and it is what auto-apply is allowed to keep running on.
autoApplyAccuracy :: DB es => Projects.ProjectId -> Eff es (Int64, Int64)
autoApplyAccuracy pid =
  fromMaybe (0, 0)
    <$> Hasql.interpOne
      [HI.sql| SELECT count(*) FILTER (WHERE applied_at IS NOT NULL)::int8,
                      count(*) FILTER (WHERE reverted_at IS NOT NULL)::int8
               FROM apis.endpoint_group_reviews
               WHERE project_id = #{pid} AND applied_at > NOW() - INTERVAL '7 days' |]


-- | Every literal path segment this project is known to route on.
--
-- Taken from canonical templates rather than raw paths: a template's literal
-- segments are the ones the system has already concluded are part of the route,
-- which is exactly the set a new id rule must never collide with.
knownStaticSegments :: DB es => Projects.ProjectId -> Eff es [Text]
knownStaticSegments pid =
  Hasql.interp
    [HI.sql| SELECT DISTINCT s FROM apis.endpoints e, unnest(string_to_array(e.canonical_path, '/')) AS s
             WHERE e.project_id = #{pid} AND e.canonical_path IS NOT NULL
               AND s <> '' AND s NOT LIKE '{%' |]


learnedIdRulePrefixes :: DB es => Projects.ProjectId -> Eff es (HS.HashSet Text)
learnedIdRulePrefixes pid =
  HS.fromList
    <$> Hasql.interp
      [HI.sql| SELECT prefix FROM apis.learned_id_rules WHERE project_id = #{pid} AND disabled_at IS NULL |]


insertLearnedIdRule :: DB es => Projects.ProjectId -> Text -> Int -> Text -> Int -> Eff es ()
insertLearnedIdRule pid prefix minLen gkey checked =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.learned_id_rules (project_id, host, prefix, min_length, source_group_key, collisions_checked)
             VALUES (#{pid}, '', #{prefix}, #{fromIntegral minLen :: Int64}, #{gkey}, #{fromIntegral checked :: Int64})
             ON CONFLICT (project_id, host, prefix) DO NOTHING |]


-- | Live rules for a project, as (prefix, min length), for the ingest cache.
learnedIdRulesFor :: DB es => Projects.ProjectId -> Eff es [(Text, Int64)]
learnedIdRulesFor pid =
  Hasql.interp
    [HI.sql| SELECT prefix, min_length::int8 FROM apis.learned_id_rules
             WHERE project_id = #{pid} AND disabled_at IS NULL |]


-- | Value shapes the model has named, fleet-wide, ranked by how many endpoints
-- they account for. The standing list of deterministic rules nobody has written.
fleetShapeReport :: DB es => Eff es [(Text, Int64, Int64)]
fleetShapeReport =
  Hasql.interp
    [HI.sql| SELECT shape, count(*)::int8, sum(member_count)::int8
             FROM apis.endpoint_group_reviews
             WHERE verdict = 'param' AND shape <> '' AND reverted_at IS NULL
             GROUP BY shape ORDER BY 3 DESC LIMIT 40 |]


-- | What a previous review settled, as far as deciding whether to ask again goes.
data ReviewedGroup = ReviewedGroup
  { membersHash :: Text
  , dueForReconfirm :: Bool
  }
  deriving stock (Eq, Generic, Show)


-- | Membership hash of every group already reviewed, paired with whether the
-- group is due to be asked again even though nothing about it has changed.
--
-- Membership change was once the only trigger, so that a stable project cost no
-- tokens. It also made the second confirmation unreachable: a group is only
-- re-asked when it changes, so a project whose routes have settled stays at one
-- confirmation forever and can never clear 'BackgroundJobs.mergeEvidenceMet'.
-- Measured on the fleet, that is 983 of 1,069 reviews. Re-confirmation is
-- therefore offered on time as well, but only to the groups where a second pass
-- can actually change an outcome: a @param@ verdict, large enough to merge, not
-- merged yet.
getReviewedGroupHashes :: (DB es, Time :> es) => Projects.ProjectId -> Eff es (HM.HashMap Text ReviewedGroup)
getReviewedGroupHashes pid = do
  now <- Time.currentTime
  HM.fromList
    . map (\(k, h, due) -> (k, ReviewedGroup{membersHash = h, dueForReconfirm = due}))
    <$> Hasql.interp
      [HI.sql| SELECT group_key, members_hash,
                      verdict = 'param' AND applied_at IS NULL AND member_count >= 8
                        AND created_at < #{now}::timestamptz - INTERVAL '7 days'
               FROM apis.endpoint_group_reviews WHERE project_id = #{pid} |]


-- | Upsert group verdicts. A changed membership replaces the old verdict rather
-- than accumulating history: the question it answered is no longer the question.
recordGroupReviews :: DB es => Projects.ProjectId -> [(Text, Text, Int, Text, Text)] -> Eff es ()
recordGroupReviews _ [] = pass
recordGroupReviews pid rows =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.endpoint_group_reviews (project_id, group_key, members_hash, member_count, verdict, shape, confirmations, first_member_count)
           SELECT #{pid}, k, h, n, v, s, 1, n
           FROM unnest(#{keys}::text[], #{hashes}::text[], #{counts}::int8[], #{verdicts}::text[], #{shapes}::text[]) AS t(k, h, n, v, s)
           ON CONFLICT (project_id, group_key)
           DO UPDATE SET members_hash = EXCLUDED.members_hash, member_count = EXCLUDED.member_count,
                         verdict = EXCLUDED.verdict, shape = EXCLUDED.shape, created_at = NOW(),
                         -- A pass only counts as confirmation when it agrees with the
                         -- last one. Any disagreement resets the evidence to zero, and
                         -- first_member_count restarts so growth is measured from the
                         -- verdict currently being argued for, not an abandoned one.
                         confirmations = CASE
                           WHEN apis.endpoint_group_reviews.verdict = EXCLUDED.verdict
                             THEN apis.endpoint_group_reviews.confirmations + 1
                           ELSE 1 END,
                         first_member_count = CASE
                           WHEN apis.endpoint_group_reviews.verdict = EXCLUDED.verdict
                             THEN apis.endpoint_group_reviews.first_member_count
                           ELSE EXCLUDED.member_count END
           WHERE apis.endpoint_group_reviews.applied_at IS NULL |]
  where
    keys = V.fromList [k | (k, _, _, _, _) <- rows]
    hashes = V.fromList [h | (_, h, _, _, _) <- rows]
    counts = V.fromList [fromIntegral n :: Int64 | (_, _, n, _, _) <- rows]
    verdicts = V.fromList [v | (_, _, _, v, _) <- rows]
    shapes = V.fromList [s | (_, _, _, _, s) <- rows]


-- | Fold merged-out endpoints' anomalies and issues onto their canonical
-- endpoint, then delete the merged-out rows.
--
-- N endpoints collapsing onto one template means N rows want the same
-- @(project_id, target_hash)@, and both tables have unique indexes on exactly
-- that. The obvious @UPDATE … WHERE NOT EXISTS@ does not save you: the guard is
-- evaluated against the pre-statement snapshot, so every colliding row passes
-- it and the statement dies on the index. So consolidation is explicit —
-- newest row per canonical group wins, the rest are deleted. On a project with
-- 18k endpoints and one open issue each, most of those issues only exist
-- because the endpoints failed to collapse in the first place.
--
-- Scoped by project so @(project_id, LEFT(target_hash, 8))@ can be indexed;
-- without the project column the predicate seq-scans a multi-million-row table.
--
-- Re-runnable: once a batch is remapped its rows no longer match
-- @LEFT(target_hash, 8) = old@, so a crashed run simply repeats.
migrateAndDeleteMergedEndpoints :: DB es => Projects.ProjectId -> [(Text, Text)] -> Eff es ()
migrateAndDeleteMergedEndpoints _ [] = pass
migrateAndDeleteMergedEndpoints pid pairs = do
  let (oldArr, canonArr) = bimap V.fromList V.fromList $ unzip pairs
      -- Rows whose remapped target would duplicate a survivor's. Ranked newest
      -- first so the issue a user is most likely looking at is the one kept.
      anomalyTargets =
        [HI.sql|
          SELECT a.id, m.canonical || substring(a.target_hash FROM 9) AS new_hash,
                 row_number() OVER (PARTITION BY m.canonical || substring(a.target_hash FROM 9)
                                    ORDER BY a.created_at DESC, a.id) AS rn
          FROM apis.anomalies a
          JOIN unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical) ON LEFT(a.target_hash, 8) = m.old
          WHERE a.project_id = #{pid} |]
      issueTargets =
        [HI.sql|
          SELECT i.id, m.canonical AS canonical, m.canonical || substring(i.target_hash FROM 9) AS new_hash,
                 CASE WHEN i.acknowledged_at IS NULL AND i.archived_at IS NULL
                      THEN row_number() OVER (
                             -- Open api_change issues are additionally unique per
                             -- endpoint_hash, so they dedupe on the endpoint, not the target.
                             PARTITION BY m.canonical, i.issue_type,
                                          CASE WHEN i.issue_type = 'api_change' THEN '' ELSE substring(i.target_hash FROM 9) END,
                                          i.acknowledged_at IS NULL AND i.archived_at IS NULL
                             ORDER BY i.updated_at DESC, i.id)
                      ELSE 1 END AS rn
          FROM apis.issues i
          JOIN unnest(#{oldArr}::text[], #{canonArr}::text[]) m(old, canonical) ON LEFT(i.target_hash, 8) = m.old
          WHERE i.project_id = #{pid} |]
  Hasql.interpExecute_
    $ [HI.sql| DELETE FROM apis.anomalies a USING ( |]
    <> anomalyTargets
    <> [HI.sql| ) t WHERE a.id = t.id AND (t.rn > 1
          OR EXISTS (SELECT 1 FROM apis.anomalies o WHERE o.project_id = #{pid} AND o.target_hash = t.new_hash
                       AND NOT (LEFT(o.target_hash, 8) = ANY(#{oldArr})))) |]
  Hasql.interpExecute_
    $ [HI.sql| UPDATE apis.anomalies a SET target_hash = t.new_hash FROM ( |]
    <> anomalyTargets
    <> [HI.sql| ) t WHERE a.id = t.id |]
  Hasql.interpExecute_
    $ [HI.sql| DELETE FROM apis.issues i USING ( |]
    <> issueTargets
    <> [HI.sql| ) t WHERE i.id = t.id AND (t.rn > 1
          OR EXISTS (SELECT 1 FROM apis.issues o WHERE o.project_id = #{pid} AND o.target_hash = t.new_hash
                       AND o.issue_type = i.issue_type AND o.acknowledged_at IS NULL AND o.archived_at IS NULL
                       AND i.acknowledged_at IS NULL AND i.archived_at IS NULL
                       AND NOT (LEFT(o.target_hash, 8) = ANY(#{oldArr})))) |]
  Hasql.interpExecute_
    $ [HI.sql| UPDATE apis.issues i SET endpoint_hash = t.canonical, target_hash = t.new_hash FROM ( |]
    <> issueTargets
    <> [HI.sql| ) t WHERE i.id = t.id |]
  Hasql.interpExecute_ [HI.sql| DELETE FROM apis.endpoints WHERE project_id = #{pid} AND hash = ANY(#{oldArr}) |]
